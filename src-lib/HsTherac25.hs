{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- A simulation of the Therac-25 treatment software, following
--   N. G. Leveson and C. S. Turner, "An Investigation of the Therac-25 Accidents",
--   IEEE Computer 26(7), July 1993.
-- Text in "double quotes" in the comments below is quoted from that paper.
--
-- The two software races the paper blames for the Tyler (Malfunction 54) and
-- Yakima (Class3 overflow) overdoses are reproduced ON PURPOSE. Everything else
-- (the plumbing between this module and the UIs) is meant to be boringly correct.
-- README.md lists which details come from the paper and which are assumptions.

module HsTherac25 (externalCallWrap, startMachine, requestStateInfo, theracState, externalCalls, TheracState (..), WrappedComms (..)) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TChan,
    TMVar,
    atomically,
    newTChan,
    newTMVarIO,
    putTMVar,
    readTChan,
    readTMVar,
    retry,
    takeTMVar,
    writeTChan,
  )
import Control.Exception (SomeException, catch)
import Control.Lens (ASetter, Getting, makeFields, (%~), (&), (.~), (^.))
import Control.Lens.Tuple (Field1 (_1), Field2 (_2))
import Control.Monad (forever, unless, when)
import Data.Bits (clearBit, setBit)
import Data.Map.Strict qualified as M
import Data.Word (Word16, Word8)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types ()
import Foreign.StablePtr
  ( StablePtr,
    deRefStablePtr,
    newStablePtr,
  )
import System.Random (randomRIO)

data TPhase = TP_Reset | TP_Datent | TP_SetupDone | TP_SetupTest | TP_PatientTreatment | TP_PauseTreatment | TP_TerminateTreatment | TP_Date_Time_IDChanges
  deriving (Eq, Show)

-- "three cardinal turntable positions: electron beam, X ray, and field light".
-- CollimatorPositionUndefined only appears on the console side and means "nothing requested yet".
-- CollimatorPositionFieldLight is only reachable through the hand control (ExtCallFieldLight).
data CollimatorPosition = CollimatorPositionXRay | CollimatorPositionElectronBeam | CollimatorPositionUndefined | CollimatorPositionFieldLight
  deriving (Eq, Show)

type CollimatorPositionInt = Int

cpMap :: M.Map CollimatorPositionInt CollimatorPosition
cpMap = M.fromList [(1, CollimatorPositionXRay), (2, CollimatorPositionElectronBeam), (3, CollimatorPositionUndefined)]

type BeamTypeInt = Int

data BeamType = BeamTypeXRay | BeamTypeElectron | BeamTypeUndefined
  deriving (Eq, Show)

btMap :: M.Map BeamTypeInt BeamType
btMap = M.fromList [(1, BeamTypeXRay), (2, BeamTypeElectron), (3, BeamTypeUndefined)]

-- ExtCallToggleDatentComplete and ExtCallToggleEditingTakingPlace keep their old names and numbers
-- for the UIs, but they SET their flag now (see `keyboardHandler`)
data ExtCallType = ExtCallSendMEOS | ExtCallToggleDatentComplete | ExtCallToggleEditingTakingPlace | ExtCallReset | ExtCallProceed | ExtCallHardReset | ExtCallSet | ExtCallFieldLight | ExtCallBeamOn | ExtCallUseBeamOnKey | ExtCallPrescribeDose

type ExtCallTypeInt = Int

ectMap :: M.Map ExtCallTypeInt ExtCallType
ectMap = M.fromList [(1, ExtCallSendMEOS), (2, ExtCallToggleDatentComplete), (3, ExtCallToggleEditingTakingPlace), (4, ExtCallReset), (5, ExtCallProceed), (6, ExtCallHardReset), (7, ExtCallSet), (8, ExtCallFieldLight), (9, ExtCallBeamOn), (10, ExtCallUseBeamOnKey), (11, ExtCallPrescribeDose)]

data ExternalCall = ExternalCall
  { _ecType :: ExtCallType,
    _ecMEOS :: MEOS,
    _ecValue :: Int -- ExtCallPrescribeDose: monitor units
  }

type BeamEnergy = Int

-- Mode/Energy Offset. In the real machine a 2-byte variable: one byte used by Datent to set the
-- beam parameters, the other used by Hand to position the turntable.
data MEOS = MEOS
  { _mEOSDatentParams :: (BeamType, BeamEnergy),
    _mEOSHandParams :: CollimatorPosition
  }
  deriving (Eq, Show)

$(makeFields ''MEOS)

newMEOS :: MEOS
newMEOS = MEOS (BeamTypeUndefined, 1477) CollimatorPositionUndefined

-- Nothing for values the C header doesn't define (including its own *CheekyPadding = 0).
makeMEOSFromCParams :: BeamTypeInt -> CollimatorPositionInt -> BeamEnergy -> Maybe MEOS
makeMEOSFromCParams bti cpi be = do
  bt <- M.lookup bti btMap
  cp <- M.lookup cpi cpMap
  pure $ MEOS (bt, be) cp

data TheracState = TheracState
  { _theracStateClass3 :: Word8, -- one byte, incremented by every pass through Set-Up Test, so it is 0 on every 256th pass
    _theracStateFMal :: Word16, -- F$mal, the interlock/malfunction word. Bit 9 = upper collimator inconsistent (written by `chkcol`), read by Set-Up Test
    _theracStateConsoleMeos :: MEOS, -- set by the keyboard handler; read by Datent (beam) and by Hand (turntable)
    _theracStateTPhase :: TPhase, -- used by `treat` - treatment phase
    _theracStateDataEntryComplete :: Bool, -- set by the keyboard handler when the cursor reaches the command line. Only a reset clears it
    _theracStateBendingMagnetFlag :: Bool, -- set by `magnet`, cleared by `pTime` (at the end of the FIRST pTime - that's the bug)
    _theracStateEditingTakingPlace :: Bool, -- set by the keyboard handler when the prescription is edited; only a reset clears it
    _theracStateHardwareMeos :: MEOS, -- what the machine is really doing: datentParams = beam parameters Datent output, handParams = where the turntable physically is
    _theracStateTreatmentOutcome :: String, -- message for the last beam-on attempt
    _theracStateResetPending :: Bool, -- set by the keyboard handler (R)
    _theracStateMalfunctionCount :: Int, -- pauses during this treatment; the 5th one suspends
    _theracStateTreatmentSuspended :: Bool, -- "treatment suspend, which required a complete machine reset to restart"
    _theracStateAwaitingSet :: Bool, -- the hand control put the turntable in field-light position; console says "PRESS SET BUTTON"
    _theracStateTurntableTarget :: CollimatorPosition, -- where the turntable motor is driving to
    _theracStateTurntableEta :: Int, -- housekeeper passes until it gets there
    _theracStateDisplayedDose :: Int, -- monitor units the dose monitor SHOWED for the last attempt
    _theracStatePatientDose :: Int, -- rads the patient ACTUALLY received since the last reset. The real console had no way to show this
    _theracStatePrescribedMu :: Int, -- monitor units the console prescribed; a normal treatment delivers and shows this
    _theracStateBeamOnKey :: Bool -- the UI has a "B" command (ExtCallUseBeamOnKey): Set-Up Done waits for it instead of firing by itself
  }
  deriving (Eq, Show)

$(makeFields ''TheracState)

-- "Typical single therapeutic doses are in the 200-rad range". Used until the UI sends
-- ExtCallPrescribeDose.
prescribedDose :: Int
prescribedDose = 200

newTherac :: TheracState
newTherac =
  TheracState
    { _theracStateClass3 = 1,
      _theracStateFMal = 0,
      _theracStateConsoleMeos = newMEOS,
      _theracStateTPhase = TP_Datent,
      _theracStateDataEntryComplete = False,
      _theracStateBendingMagnetFlag = False,
      _theracStateEditingTakingPlace = False,
      _theracStateHardwareMeos = newMEOS & handParams .~ CollimatorPositionFieldLight, -- power up with no beam expected
      _theracStateTreatmentOutcome = "",
      _theracStateResetPending = False,
      _theracStateMalfunctionCount = 0,
      _theracStateTreatmentSuspended = False,
      _theracStateAwaitingSet = False,
      _theracStateTurntableTarget = CollimatorPositionFieldLight,
      _theracStateTurntableEta = 0,
      _theracStateDisplayedDose = 0,
      _theracStatePatientDose = 0,
      _theracStatePrescribedMu = prescribedDose,
      _theracStateBeamOnKey = False
    }

data WrappedComms = WrappedComms
  { _wrappedCommsTheracState :: TMVar TheracState,
    _wrappedCommsExternalCalls :: TChan ExternalCall
  }

$(makeFields ''WrappedComms)

-- timing

-- "Tasks are initiated every 0.1 second"
schedulerTick :: Int
schedulerTick = 100000

-- The housekeeper runs twice per Treat tick so it always gets a look at Class3 between two
-- Set-Up Test passes. (The paper only says tasks run every 0.1 s; see README.)
housekeeperTick :: Int
housekeeperTick = schedulerTick `div` 2

-- "Setting the bending magnets takes about 8 seconds." The paper only says "several magnets".
numMagnets :: Int
numMagnets = 4

hysteresisDelay :: Int
hysteresisDelay = 2000000

pTimePoll :: Int
pTimePoll = 20000

-- Not in the paper. This is how long the turntable is still in the wrong place after it is
-- told to move, i.e. how wide the Yakima window is. See README.
turntableTravelPasses :: Int
turntableTravelPasses = 2000000 `div` housekeeperTick

-- "This convenient and simple feature could be invoked a maximum of five times before the
-- machine automatically suspended treatment"
maxPauses :: Int
maxPauses = 5

-- Not in the paper; "Malfunction messages were commonplace". High enough that P becomes a
-- habit, low enough that normal treatments finish.
spuriousPausePercent :: Int
spuriousPausePercent = 30

-- all of the helper functions that aren't part of the therac software are mercifully doing most things atomically

modifyTherac :: TMVar TheracState -> (TheracState -> TheracState) -> STM ()
modifyTherac ts f = takeTMVar ts >>= \l -> putTMVar ts $! f l

setFieldInStruct :: ASetter a1 a1 a2 b -> b -> TMVar a1 -> STM ()
setFieldInStruct ff fv ts = takeTMVar ts >>= \l -> putTMVar ts $! (ff .~ fv $ l)

setTPhase :: (HasTPhase a1 b) => b -> TMVar a1 -> STM ()
setTPhase = setFieldInStruct tPhase

readFieldFromStruct :: Getting b s b -> TMVar s -> STM b
readFieldFromStruct ff ts = readTMVar ts >>= \l -> return $ l ^. ff

-- soft reset (R): a new treatment. Class3 is never re-initialised, and the turntable
-- physically stays where it is. Whether the UI has a "B" command is not the treatment's business.
resetTherac :: TMVar TheracState -> STM ()
resetTherac ts = modifyTherac ts $ \s ->
  newTherac
    & class3 .~ (s ^. class3)
    & hardwareMeos . handParams .~ (s ^. hardwareMeos . handParams)
    & turntableTarget .~ (s ^. hardwareMeos . handParams)
    & beamOnKey .~ (s ^. beamOnKey)

-- the turntable position the console is asking for; the UIs usually send it, otherwise it follows the mode
requestedTurntable :: MEOS -> CollimatorPosition
requestedTurntable m = case m ^. handParams of
  CollimatorPositionUndefined -> case m ^. datentParams . _1 of
    BeamTypeXRay -> CollimatorPositionXRay
    BeamTypeElectron -> CollimatorPositionElectronBeam
    BeamTypeUndefined -> CollimatorPositionUndefined
  p -> p

-- BEGIN virtual task keyboardhandler

keyboardHandler :: TMVar TheracState -> ExternalCall -> STM ()
keyboardHandler ts (ExternalCall ect m v) = case ect of
  ExtCallSendMEOS -> editMEOS m ts
  ExtCallToggleDatentComplete -> cursorToCommandLine ts
  ExtCallToggleEditingTakingPlace -> setFieldInStruct editingTakingPlace True ts
  ExtCallReset -> setFieldInStruct resetPending True ts
  ExtCallProceed -> proceedTreatment ts
  ExtCallHardReset -> modifyTherac ts $ \s -> newTherac & beamOnKey .~ (s ^. beamOnKey)
  ExtCallSet -> setFieldInStruct awaitingSet False ts
  ExtCallFieldLight -> fieldLight ts
  ExtCallBeamOn -> beamOn ts
  ExtCallUseBeamOnKey -> setFieldInStruct beamOnKey True ts
  ExtCallPrescribeDose -> when (v > 0) $ setFieldInStruct prescribedMu v ts

-- An edit of the prescription. While Datent is running it just lands in MEOS - that's the Tyler
-- race. Once Datent has exited, the edit sends the machine back through Datent. The paper doesn't
-- say what caught slow edits, only that "data-entry speed during editing was the key factor"
-- (ASSUMPTION, see README).
editMEOS :: MEOS -> TMVar TheracState -> STM ()
editMEOS m ts = modifyTherac ts $ \s ->
  let s' = s & consoleMeos .~ m & editingTakingPlace .~ True
   in if m /= s ^. consoleMeos && s ^. tPhase `elem` [TP_SetupTest, TP_SetupDone, TP_PauseTreatment]
        then s' & tPhase .~ TP_Datent
        else s'

-- "the data-entry completion variable only indicates that the cursor has been down to the
-- command line, not that it is still there. A potential race condition is set up."
-- So: set it, and nothing but a reset clears it.
-- (editingTakingPlace is left alone: on the PDP-11 an edit took keystrokes, long enough for Ptime to
-- see the flag, but a UI can send "edit" and "back to the command line" in the same millisecond.)
cursorToCommandLine :: TMVar TheracState -> STM ()
cursorToCommandLine = setFieldInStruct dataEntryComplete True

-- "She hit the one-key command 'B' (for 'beam on') to begin the treatment." Only means
-- anything once the console says BEAM READY.
beamOn :: TMVar TheracState -> STM ()
beamOn ts = modifyTherac ts $ \s ->
  if s ^. tPhase == TP_SetupDone then s & tPhase .~ TP_PatientTreatment else s

proceedTreatment :: TMVar TheracState -> STM ()
proceedTreatment ts = do
  tp <- readFieldFromStruct tPhase ts
  case tp of
    TP_PauseTreatment -> setTPhase TP_PatientTreatment ts
    _ -> return ()

-- The hand control in the treatment room rotates the turntable to the field-light position to
-- check the patient's position; "The console displays the message 'Press set button' while
-- the turntable is in the field-light position." At Yakima this happened during a pause
-- between exposures; going back to Set-Up Test from a pause is an ASSUMPTION.
fieldLight :: TMVar TheracState -> STM ()
fieldLight ts = modifyTherac ts $ \s -> case s ^. tPhase of
  TP_Datent -> s & awaitingSet .~ True
  TP_SetupTest -> s & awaitingSet .~ True
  TP_SetupDone -> s & awaitingSet .~ True & tPhase .~ TP_SetupTest
  TP_PauseTreatment -> s & awaitingSet .~ True & tPhase .~ TP_SetupTest
  _ -> s

-- END virtual task keyboardhandler

handleExternalCalls :: TMVar TheracState -> TChan ExternalCall -> IO ()
handleExternalCalls ts ecc = forever $ atomically $ readTChan ecc >>= keyboardHandler ts

-- task - treatment monitor - the supervisor task basically
-- "Treat ... directs and monitors patient setup and treatment via eight operating phases.
-- These are called as subroutines, depending on the value of the Tphase control variable.
-- Following the execution of a particular subroutine, Treat reschedules itself."
treat :: TMVar TheracState -> IO ()
treat ts = forever $ do
  threadDelay schedulerTick
  curTPhase <- atomically $ readFieldFromStruct tPhase ts
  case curTPhase of
    TP_Reset -> atomically $ resetTherac ts
    TP_Datent -> datent ts
    TP_SetupDone -> atomically $ setupDone ts
    TP_SetupTest -> atomically $ setupTest ts
    TP_PatientTreatment -> zapTheSpecimen ts
    TP_PauseTreatment -> waitForProceedOrReset ts
    TP_TerminateTreatment -> waitForReset ts
    TP_Date_Time_IDChanges -> return () -- this + a bunch of other purely cosmetic things will be implemented elsewhere (the c++ class or the UI in unreal engine probably)

-- The console says "BEAM READY". A UI with a "B" command (ExtCallUseBeamOnKey) fires the beam
-- with ExtCallBeamOn; for the others Begin doubles as the "B" key.
setupDone :: TMVar TheracState -> STM ()
setupDone ts = do
  s <- readTMVar ts
  if
    | s ^. resetPending -> setTPhase TP_Reset ts
    | not (s ^. beamOnKey) -> setTPhase TP_PatientTreatment ts
    | otherwise -> return ()

waitForProceedOrReset :: TMVar TheracState -> IO ()
waitForProceedOrReset ts = atomically $ do
  s <- readTMVar ts
  if s ^. resetPending
    then setTPhase TP_Reset ts
    else when (s ^. tPhase == TP_PauseTreatment) retry

waitForReset :: TMVar TheracState -> IO ()
waitForReset ts = atomically $ do
  tsrp <- readFieldFromStruct resetPending ts
  if tsrp then setTPhase TP_Reset ts else retry

-- BEGIN zapping

-- What the beam physically does, given the beam parameters Datent output and where the turntable
-- really is. Note that the software never compares these two: "The software appears to include
-- no checks to detect such an incompatibility."
data Delivery
  = Treated -- correct setup
  | TylerOverdose -- X-ray current, turntable in electron position: no target, no flattener
  | YakimaOverdose -- X-ray current, turntable in field-light position: no target, no scanning, a mirror in the beam
  | NuisancePause -- electron current with the wrong accessories: not documented, modelled as harmless

beamPhysics :: BeamType -> CollimatorPosition -> Delivery
beamPhysics BeamTypeXRay CollimatorPositionXRay = Treated
beamPhysics BeamTypeElectron CollimatorPositionElectronBeam = Treated
-- "Much greater electron-beam current is required for photon mode (some 100 times greater than
-- that for electron therapy)" because the flattener is "a very efficient attenuator"
beamPhysics BeamTypeXRay CollimatorPositionElectronBeam = TylerOverdose
beamPhysics BeamTypeXRay _ = YakimaOverdose
beamPhysics _ _ = NuisancePause

zapTheSpecimen :: TMVar TheracState -> IO ()
zapTheSpecimen ts = do
  spuriousRoll <- randomRIO (1, 100) :: IO Int
  spuriousKind <- randomRIO (0, 3) :: IO Int
  channel <- randomRIO (1, 63) :: IO Int
  -- "After-the-fact simulations of the accident revealed possible doses of 16,500 to 25,000 rads"
  tylerRads <- randomRIO (16500, 25000) :: IO Int
  -- "the dose delivered under these conditions - that is, when the turntable was in the
  -- field-light position - was on the order of 4,000 to 5,000 rads"
  yakimaRads <- randomRIO (4000, 5000) :: IO Int
  atomically $ modifyTherac ts $ \s ->
    let hw = s ^. hardwareMeos
        pause msg shown rads =
          let mc = s ^. malfunctionCount + 1
              suspend = mc >= maxPauses
           in s
                & treatmentOutcome .~ msg
                & displayedDose .~ shown
                & patientDose %~ (+ rads)
                & malfunctionCount .~ mc
                & treatmentSuspended .~ suspend
                & tPhase .~ (if suspend then TP_TerminateTreatment else TP_PauseTreatment)
     in case beamPhysics (hw ^. datentParams . _1) (hw ^. handParams) of
          -- "Malfunction 54 ... a 'dose input 2' error ... a dose had been delivered that was either too
          -- high or too low." The saturated ion chamber read low: "6 monitor units delivered, whereas the
          -- operator had requested 202 monitor units", every time P was pressed.
          TylerOverdose -> pause "MALFUNCTION 54" 6 tylerRads
          -- "the console displayed no dose or dose rate. After 5 or 6 seconds, the unit shut down with a
          -- pause" ... "The machine paused again, this time displaying 'flatness' on the reason line."
          -- (there's no ion chamber in the field-light position)
          YakimaOverdose -> pause "FLATNESS" 0 yakimaRads
          NuisancePause -> pause "LOW DOSE RATE" 0 0
          Treated
            -- simulate shitty fucking computer doodad breaking all the time to prime people to P(roceed) repeatedly and carelessly
            | spuriousRoll <= spuriousPausePercent -> pause (spuriousMessage spuriousKind channel) 0 0
            | otherwise ->
                s
                  & treatmentOutcome .~ "TREATMENT OK"
                  & displayedDose .~ (s ^. prescribedMu)
                  & patientDose %~ (+ (s ^. prescribedMu))
                  & tPhase .~ TP_TerminateTreatment

-- "They would give messages of low dose rate, V-tilt, H-tilt, and other things", and "some
-- merely consisted of the word 'malfunction' followed by a number from 1 to 64 denoting an
-- analog/digital channel number". 54 is kept for the real thing.
spuriousMessage :: Int -> Int -> String
spuriousMessage kind channel = case kind of
  0 -> "LOW DOSE RATE"
  1 -> "H-TILT"
  2 -> "V-TILT"
  _ -> "MALFUNCTION " ++ show (if channel >= 54 then channel + 1 else channel)

-- END zapping

#ifdef mingw32_HOST_OS
foreign export stdcall externalCallWrap :: StablePtr WrappedComms -> ExtCallTypeInt -> BeamTypeInt -> CollimatorPositionInt -> BeamEnergy -> IO ()
#else
foreign export ccall externalCallWrap :: StablePtr WrappedComms -> ExtCallTypeInt -> BeamTypeInt -> CollimatorPositionInt -> BeamEnergy -> IO ()
#endif
-- Unknown values are dropped here instead of being stored for some other thread to trip over
-- (an uncaught exception in a foreign export takes the host process down with it).
externalCallWrap :: StablePtr WrappedComms -> ExtCallTypeInt -> BeamTypeInt -> CollimatorPositionInt -> BeamEnergy -> IO ()
externalCallWrap mywc ecti bti cpi be =
  ( do
      mywc' <- deRefStablePtr mywc
      let send = atomically . writeTChan (_wrappedCommsExternalCalls mywc')
      case M.lookup ecti ectMap of
        Nothing -> return ()
        Just ExtCallSendMEOS -> mapM_ (\m -> send (ExternalCall ExtCallSendMEOS m 0)) (makeMEOSFromCParams bti cpi be)
        Just ect -> send (ExternalCall ect newMEOS be)
  )
    `catch` \(_ :: SomeException) -> return ()

-- external start machine
-- hs_exit() will probably kill children threads ?? not sure how else to keep this alive and return from the call on c++ caller's side. need to test
#ifdef mingw32_HOST_OS
foreign export stdcall startMachine :: IO (StablePtr WrappedComms)
#else
foreign export ccall startMachine :: IO (StablePtr WrappedComms)
#endif
startMachine :: IO (StablePtr WrappedComms)
startMachine = do
  ts <- newTMVarIO newTherac
  ecc <- atomically newTChan
  _ <- forkIO $ handleExternalCalls ts ecc
  _ <- forkIO $ treat ts
  _ <- forkIO $ housekeeper ts
  newStablePtr $ WrappedComms ts ecc

data StateInfoRequest = RequestTreatmentOutcome | RequestActiveSubsystem | RequestTreatmentState | RequestReason | RequestBeamMode | RequestBeamEnergy | RequestDumpFullState | RequestClass3 | RequestTurntablePosition | RequestDisplayedDose | RequestPatientDose | RequestSetButtonPrompt

type SIRInt = Int

siriMap :: M.Map Int StateInfoRequest
siriMap = M.fromList [(1, RequestTreatmentOutcome), (2, RequestActiveSubsystem), (3, RequestTreatmentState), (4, RequestReason), (5, RequestBeamMode), (6, RequestBeamEnergy), (7, RequestDumpFullState), (8, RequestClass3), (9, RequestTurntablePosition), (10, RequestDisplayedDose), (11, RequestPatientDose), (12, RequestSetButtonPrompt)]

stateInfo :: TheracState -> SIRInt -> String
stateInfo ts' siri = case M.lookup siri siriMap of
  Just RequestTreatmentOutcome -> ts' ^. treatmentOutcome
  Just RequestActiveSubsystem -> if ts' ^. dataEntryComplete then "TREAT" else "DATA ENTRY"
  Just RequestTreatmentState -> show $ ts' ^. tPhase
  Just RequestReason -> let to = ts' ^. treatmentOutcome in if to == "TREATMENT OK" || to == "" then "OPERATOR" else to
  Just RequestBeamMode -> show $ ts' ^. hardwareMeos . datentParams . _1
  Just RequestBeamEnergy -> show $ ts' ^. hardwareMeos . datentParams . _2
  Just RequestDumpFullState -> show ts'
  Just RequestClass3 -> show $ ts' ^. class3
  Just RequestTurntablePosition -> show $ ts' ^. hardwareMeos . handParams
  Just RequestDisplayedDose -> show $ ts' ^. displayedDose
  Just RequestPatientDose -> show $ ts' ^. patientDose
  Just RequestSetButtonPrompt -> if ts' ^. awaitingSet then "PRESS SET BUTTON" else ""
  Nothing -> ""

-- external return requested state info. The string is malloc'd; free it with free_state_info.
#ifdef mingw32_HOST_OS
foreign export stdcall requestStateInfo :: StablePtr WrappedComms -> SIRInt -> IO CString
#else
foreign export ccall requestStateInfo :: StablePtr WrappedComms -> SIRInt -> IO CString
#endif
requestStateInfo :: StablePtr WrappedComms -> SIRInt -> IO CString
requestStateInfo mywc siri =
  ( do
      mywc' <- deRefStablePtr mywc
      ts' <- atomically $ readTMVar (_wrappedCommsTheracState mywc')
      newCString $ stateInfo ts' siri
  )
    `catch` \(_ :: SomeException) -> newCString ""

-- BEGIN TP_SetupTest phase

-- `treat` Set-Up Test subroutine. "Every pass through the Set-Up Test routine increments the upper
-- collimator position check, a shared variable called Class3. If Class3 is nonzero, there is an
-- inconsistency and treatment should not proceed." ... "After setting the Class3 variable, Set-Up
-- Test next checks for any malfunctions in the system by checking another shared variable ...
-- called F$mal ... When F$mal is zero ... the Set-Up Test subroutine sets the Tphase variable
-- equal to 2". It also waits for the set button while the field light is on.
-- The AECL fix: "the Class3 variable is set to some fixed nonzero value each time through Set-Up
-- Test instead of being incremented."
setupTest :: TMVar TheracState -> STM ()
setupTest ts = modifyTherac ts $ \s ->
  let s' = s & class3 %~ (+ 1) -- a Word8, so 255 + 1 == 0, just like the PDP-11 byte
   in if
        | s ^. resetPending -> s' & tPhase .~ TP_Reset
        | not (s ^. awaitingSet) && s ^. fMal == 0 -> s' & tPhase .~ TP_SetupDone
        | otherwise -> s'

-- END TP_SetupTest phase

-- BEGIN `housekeeper` stuff

-- `housekeeper` subroutine - "analog/digital limit checking". "Lmtchk first checks the Class3
-- variable. If Class3 contains a nonzero value, Lmtchk calls the Check Collimator (Chkcol)
-- subroutine. If Class3 contains zero, Chkcol is bypassed and the upper collimator position check
-- is not performed." F$mal is rebuilt on every pass, so a bypassed check leaves bit 9 clear
-- (ASSUMPTION, see README).
lmtchk :: TMVar TheracState -> STM ()
lmtchk ts = modifyTherac ts $ \s ->
  let s' = s & fMal %~ (`clearBit` 9)
   in if s ^. class3 /= 0 then chkcol s' else s'

-- "If upper collimator position inconsistent with treatment then set bit 9 of F$mal"
chkcol :: TheracState -> TheracState
chkcol s
  | want /= CollimatorPositionUndefined && s ^. hardwareMeos . handParams /= want = s & fMal %~ (`setBit` 9)
  | otherwise = s
  where
    want = requestedTurntable (s ^. consoleMeos)

-- Hand: "used by another task (Hand) to set the collimator/turntable to the proper position for
-- the selected mode/energy". Drives the turntable toward what the console asks for (or to the
-- field-light position while the hand control holds it there). The motor takes a while.
hand :: TheracState -> TheracState
hand s
  | want == CollimatorPositionUndefined || want == cur = s & turntableTarget .~ cur & turntableEta .~ 0
  | s ^. turntableTarget /= want || s ^. turntableEta <= 0 = s & turntableTarget .~ want & turntableEta .~ turntableTravelPasses
  | s ^. turntableEta == 1 = s & hardwareMeos . handParams .~ want & turntableEta .~ 0
  | otherwise = s & turntableEta %~ subtract 1
  where
    cur = s ^. hardwareMeos . handParams
    want = if s ^. awaitingSet then CollimatorPositionFieldLight else requestedTurntable (s ^. consoleMeos)

-- task - runs concurrently to other stuff - "takes care of system-status interlocks and limit
-- checks" - moves the turntable, then checks it. The turntable only moves during data entry and
-- set-up, so after a Yakima overdose it is still in the field-light position when P is pressed:
-- "The machine paused again, this time displaying 'flatness'" (ASSUMPTION, see README).
housekeeper :: TMVar TheracState -> IO ()
housekeeper ts = forever $ do
  threadDelay housekeeperTick
  atomically $ modifyTherac ts $ \s ->
    if s ^. tPhase `elem` [TP_Reset, TP_Datent, TP_SetupTest] then hand s else s
  atomically $ lmtchk ts

-- END `housekeeper` stuff

-- BEGIN TP_Datent stuff

setBendingMagnetFlag :: TMVar TheracState -> STM ()
setBendingMagnetFlag = setFieldInStruct bendingMagnetFlag True

unsetBendingMagnetFlag :: TMVar TheracState -> STM ()
unsetBendingMagnetFlag = setFieldInStruct bendingMagnetFlag False

-- subroutine - part of `treat` TP_Datent - spin until hysteresis delay expired
-- Ptime (Figure 3):
--   repeat
--     if bending magnet flag is set then
--       if editing taking place then
--         if mode/energy has changed then exit
--   until hysteresis delay has expired
--   Clear bending magnet flag
--   return
-- "Since Ptime clears it during its first execution, any edits performed during each succeeding
-- pass through Ptime will not be recognized."
-- Returns True if it noticed an edit.
pTime :: TMVar TheracState -> (BeamType, BeamEnergy) -> IO Bool
pTime ts wanted = go (hysteresisDelay `div` pTimePoll)
  where
    go :: Int -> IO Bool
    go 0 = do
      atomically $ unsetBendingMagnetFlag ts -- THE Tyler bug. AECL's fix moved this to the end of `magnet`
      return False
    go n = do
      -- it's intentional that we read each shared variable separately, like the PDP-11 task did
      bmf <- atomically $ readFieldFromStruct bendingMagnetFlag ts
      editing <- atomically $ readFieldFromStruct editingTakingPlace ts
      current <- atomically $ readFieldFromStruct (consoleMeos . datentParams) ts
      if bmf && editing && current /= wanted
        then do
          atomically $ unsetBendingMagnetFlag ts -- "Ptime clears the bending magnet variable and exits to Magnet"
          return True
        else do
          threadDelay pTimePoll
          go (n - 1)

-- subroutine `magnet` - set bending magnets - part of `treat` TP_Datent
-- Magnet (Figure 3):
--   Set bending magnet flag
--   repeat
--     Set next magnet
--     Call Ptime
--     if mode/energy has changed, then exit
--   until all magnets are set
--   return
magnet :: TMVar TheracState -> (BeamType, BeamEnergy) -> IO Bool
magnet ts wanted = do
  atomically $ setBendingMagnetFlag ts
  let setMagnets :: Int -> IO Bool
      setMagnets 0 = return False
      setMagnets n = do
        edited <- pTime ts wanted
        if edited then return True else setMagnets (n - 1)
  setMagnets numMagnets

-- "it uses the high-order byte to index into a table of preset operating parameters and places
-- them in the digital-to-analog output table"
outputParameters :: TMVar TheracState -> (BeamType, BeamEnergy) -> STM ()
outputParameters ts wanted = setFieldInStruct (hardwareMeos . datentParams) wanted ts

-- subroutine `TP_Datent` - part of `treat`
-- Datent (Figure 3):
--   if mode/energy specified then
--   begin
--     calculate table index
--     repeat fetch parameter, output parameter, point to next parameter until all parameters set
--     call Magnet
--     if mode/energy changed then return
--   end
--   if data entry is complete then set Tphase to 3
--   if data entry is not complete then
--     if reset command entered then set Tphase to 0
--   return
-- We only redo the parameters (and the 8 seconds of magnets) when the console asks for something
-- other than what was last output. Once Tphase is 3, "Datent is not entered again": an edit made
-- while `magnet` was past its first pTime is never looked at.
datent :: TMVar TheracState -> IO ()
datent ts = do
  wanted <- atomically $ readFieldFromStruct (consoleMeos . datentParams) ts
  current <- atomically $ readFieldFromStruct (hardwareMeos . datentParams) ts
  edited <-
    if fst wanted /= BeamTypeUndefined && wanted /= current
      then do
        atomically $ outputParameters ts wanted
        magnet ts wanted
      else return False
  unless edited $ atomically $ do
    s <- readTMVar ts
    -- "Initially, the data-entry process forces the operator to enter the mode and energy"
    if s ^. dataEntryComplete && s ^. consoleMeos . datentParams . _1 /= BeamTypeUndefined
      then setTPhase TP_SetupTest ts
      else when (s ^. resetPending) $ setTPhase TP_Reset ts

-- END TP_Datent stuff
