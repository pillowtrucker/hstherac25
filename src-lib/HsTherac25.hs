{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module HsTherac25(externalCallWrap,startMachine,requestStateInfo,theracState,externalCalls,TheracState(..),WrappedComms(..)) where
import Foreign.C.Types()
import Foreign.C.String ( CString, newCString )
import Foreign.StablePtr
    ( newStablePtr, StablePtr, deRefStablePtr )
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad (forever,when,void)
import Data.Maybe(fromJust)
import Control.Concurrent.STM
    ( TMVar,
      STM,
      TChan,
      takeTMVar,
      putTMVar,
      atomically,
      readTChan,
      readTMVar,
      swapTMVar,
      retry,
      writeTChan,
      newTMVarIO,
      newTChan )
import Control.Lens ( makeFields, (^.), (%~), (.~), ASetter, Getting)
import Control.Lens.Tuple ( Field1(_1), Field2(_2) )
import System.Random(randomRIO)
import qualified Data.Map.Strict as M

data TPhase = TP_Reset | TP_Datent | TP_SetupDone | TP_SetupTest | TP_PatientTreatment | TP_PauseTreatment | TP_TerminateTreatment | TP_Date_Time_IDChanges
  deriving (Eq,Show)
data CollimatorPosition = CollimatorPositionXRay | CollimatorPositionElectronBeam | CollimatorPositionUndefined
  deriving (Eq,Show)
type CollimatorPositionInt = Int
cpMap :: M.Map CollimatorPositionInt CollimatorPosition
cpMap = M.fromList [(1,CollimatorPositionXRay),(2,CollimatorPositionElectronBeam),(3,CollimatorPositionUndefined)]


type BeamTypeInt = Int
data BeamType = BeamTypeXRay | BeamTypeElectron | BeamTypeUndefined
  deriving (Eq,Show)
btMap :: M.Map BeamTypeInt BeamType
btMap = M.fromList [(1,BeamTypeXRay),(2,BeamTypeElectron),(3,BeamTypeUndefined)]


-- datent complete is actually "begin treatment"
data ExtCallType = ExtCallSendMEOS | ExtCallToggleDatentComplete | ExtCallToggleEditingTakingPlace | ExtCallReset | ExtCallProceed
type ExtCallTypeInt = Int
ectMap :: M.Map ExtCallTypeInt ExtCallType
ectMap = M.fromList [(1,ExtCallSendMEOS),(2,ExtCallToggleDatentComplete),(3,ExtCallToggleEditingTakingPlace),(4,ExtCallReset),(5,ExtCallProceed)]

data ExternalCall = ExternalCall {
  _ecType :: ExtCallType,
  _ecMEOS :: MEOS
                    }

type BeamEnergy = Int

-- Mode/Energy Offset
data MEOS = MEOS {
  _mEOSDatentParams :: (BeamType,BeamEnergy),
  _mEOSHandParams :: CollimatorPosition
                 }
            deriving (Eq,Show)
$(makeFields ''MEOS)

newMEOS :: MEOS
newMEOS = MEOS (BeamTypeUndefined, 1477) CollimatorPositionUndefined

makeMEOSFromCParams :: BeamTypeInt -> CollimatorPositionInt -> BeamEnergy -> MEOS
makeMEOSFromCParams bti cpi be =
  let bt = fromJust $ M.lookup bti btMap
      cp = fromJust $ M.lookup cpi cpMap
  in
    MEOS (bt,be) cp


data TheracState = TheracState {
  _theracStateClass3 :: Int, -- fake unsigned byte, incremented by `TP_SetupTest`, which should also set _theracStateClass3Ignore on actual 0 and on overflow
  _theracStateFSmall :: Bool, -- set by `chkcol`, read by `TP_SetupTest`
  _theracStateConsoleMeos :: MEOS, -- set by `keyboardHandler`, read by `TP_Datent` and `hand`, the desired hardware state
  _theracStateTPhase :: TPhase, -- used by `treat` - treatment phase
  _theracStateDataEntryComplete :: Bool, -- set by `keyboardHandler`, read by `treat`
  _theracStateBendingMagnetFlag :: Bool, -- set by magnet, read by `pTime`
  _theracStateEditingTakingPlace :: Bool, -- set by `keyboardHandler`, read by `pTime`
  _theracStateHardwareMeos :: MEOS, -- the actual parameters set in the hardware
  _theracStateTreatmentOutcome :: String, -- outcome of last treatment attempt
  _theracStateResetPending :: Bool, -- set by `keyboardHandler`, read by `treat` (datent)
  _theracStateClass3Ignore :: Bool, -- this is going to be way nicer to implement than actually adding a silently-overflowing Word8. we set this every time class3 % 255 == 0 && class3 == 0.
  _theracStateMalfunctionCount :: Int
                               }
  deriving (Eq,Show)
$(makeFields ''TheracState)

newTherac :: TheracState
newTherac = TheracState 1 True newMEOS TP_Datent False False True newMEOS "" False False 0

resetTherac :: TMVar TheracState -> STM ()
resetTherac ts = do
  ts' <- readTMVar ts
  let preservedClass3 = ts' ^. class3
      preservedClass3Ignore = ts' ^. class3Ignore
  putTMVar ts $ newTherac {_theracStateClass3 = preservedClass3, _theracStateClass3Ignore = preservedClass3Ignore}

data WrappedComms = WrappedComms {
  _wrappedCommsTheracState :: TMVar TheracState,
  _wrappedCommsExternalCalls :: TChan ExternalCall
                                 }
$(makeFields ''WrappedComms)

-- all of the helper functions that aren't part of the therac software are mercifully doing most things atomically

toggleBoolFieldInStruct :: ASetter a a Bool Bool -> TMVar a -> STM ()
toggleBoolFieldInStruct ff ts = takeTMVar ts >>= \l -> putTMVar ts (ff %~ not $! l)

-- BEGIN virtual task keyboardhandler - sets dataEntrycomplete when the cursor leaves the screen after committing Datent data B[egin], sets editingTakingPlace, sends MEOS every time it is changed on screen, sends R[eset] command, sends P[roceed]

toggleEditingTakingPlace :: TMVar TheracState -> STM ()
toggleEditingTakingPlace = toggleBoolFieldInStruct editingTakingPlace

toggleDatentComplete :: TMVar TheracState -> STM ()
toggleDatentComplete = toggleBoolFieldInStruct dataEntryComplete

proceedTreatment :: TMVar TheracState -> STM ()
proceedTreatment = setTPhase TP_PatientTreatment

setResetPending :: TMVar TheracState -> STM ()
setResetPending = setFieldInStruct resetPending True

setTheracConsoleMEOS :: MEOS -> TMVar TheracState -> STM()
setTheracConsoleMEOS = setFieldInStruct consoleMeos

-- END virtual task keyboardhandler


handleExternalCalls :: TMVar TheracState -> TChan ExternalCall -> IO ()
handleExternalCalls ts ecc = do
  ecall <- atomically $ readTChan ecc
  case ecall of
    ExternalCall ExtCallReset _ -> atomically $ setResetPending ts
    ExternalCall ExtCallToggleDatentComplete _ -> atomically $ toggleDatentComplete ts
    ExternalCall ExtCallToggleEditingTakingPlace _ -> atomically $ toggleEditingTakingPlace ts
    ExternalCall ExtCallProceed _ -> atomically $ proceedTreatment ts
    ExternalCall ExtCallSendMEOS m -> atomically $ setTheracConsoleMEOS m ts
  handleExternalCalls ts ecc


setFieldInStruct :: ASetter a1 a1 a2 b -> b -> TMVar a1 -> STM ()
setFieldInStruct ff fv ts = takeTMVar ts >>= \l -> putTMVar ts (ff .~ fv $ l)

setTPhase :: HasTPhase a1 b => b -> TMVar a1 -> STM ()
setTPhase = setFieldInStruct tPhase

readFieldFromStruct :: Getting b s b -> TMVar s -> STM b
readFieldFromStruct ff ts = readTMVar ts >>= \l -> return $ l ^. ff

-- task - treatment monitor - the supervisor task basically
treat :: TMVar TheracState -> IO ()
treat ts = do
  threadDelay 1666 
  curTPhase <- atomically $ readFieldFromStruct tPhase ts
  case curTPhase of
    TP_Reset -> atomically $ resetTherac ts
    TP_Datent -> datent ts
    TP_SetupDone -> atomically $ setTPhase TP_PatientTreatment ts -- it's probably fine to just skip over this
    TP_SetupTest -> setupTest ts
    TP_PatientTreatment -> zapTheSpecimen ts
    TP_PauseTreatment -> do
      mc <- atomically $ readFieldFromStruct malfunctionCount ts
      if mc > 4 then
        atomically $ setTPhase TP_Reset ts
      else
        waitForUnpause ts
    TP_TerminateTreatment -> waitForReset ts
    TP_Date_Time_IDChanges -> return () -- this + a bunch of other purely cosmetic things will be implemented elsewhere (the c++ class or the UI in unreal engine probably)
  treat ts

waitForUnpause :: TMVar TheracState -> IO ()
waitForUnpause ts = atomically $ do
  tp <- readFieldFromStruct tPhase ts
  case tp of
    TP_PauseTreatment -> retry
    _ -> return ()
waitForReset :: TMVar TheracState -> IO ()
waitForReset ts = atomically $ do
  tsrp <- readFieldFromStruct resetPending ts
  if tsrp then setTPhase TP_Reset ts else retry

-- BEGIN zapping
zapTheSpecimen :: TMVar TheracState -> IO ()
zapTheSpecimen ts = do
  reallyGoodNumber <- randomRIO (12,53) :: IO Int
  ts' <- atomically $ readTMVar ts
  let tscm = ts' ^. consoleMeos
      tshm  = ts' ^. hardwareMeos
      mc  = ts' ^. malfunctionCount
  atomically $ if tscm /= tshm then
                 void $ swapTMVar ts (malfunctionCount .~ mc+1 $ (tPhase .~ TP_PauseTreatment $ (treatmentOutcome .~ "MALFUNCTION 54" $ ts')))
                 -- I'm not sure if both the race condition and the overflow bug had the same error number but I'm assuming they did.
               else
                 if reallyGoodNumber > 22 then
                   void $ swapTMVar ts (malfunctionCount .~ mc+1 $ (tPhase .~ TP_PauseTreatment $ (treatmentOutcome .~ ("MALFUNCTION " ++ show reallyGoodNumber) $ ts'))) -- simulate shitty fucking computer doodad breaking all the time to prime people to P(roceed) repeatedly and carelessly
                 else
                   void $ swapTMVar ts (tPhase .~ TP_TerminateTreatment $ (treatmentOutcome .~ "TREATMENT OK" $ ts'))
-- END zapping




foreign export ccall externalCallWrap :: StablePtr WrappedComms -> ExtCallTypeInt -> BeamTypeInt -> CollimatorPositionInt -> BeamEnergy -> IO ()
externalCallWrap :: StablePtr WrappedComms -> ExtCallTypeInt -> BeamTypeInt -> CollimatorPositionInt -> BeamEnergy -> IO ()
externalCallWrap mywc ecti bti cpi be = do
  mywc' <- deRefStablePtr mywc
  let myc = _wrappedCommsExternalCalls mywc'
  atomically $ writeTChan myc (ExternalCall (fromJust $ M.lookup ecti ectMap) (makeMEOSFromCParams bti cpi be))
  return ()

-- external start machine
-- hs_exit() will probably kill children threads ?? not sure how else to keep this alive and return from the call on c++ caller's side. need to test
foreign export ccall startMachine :: IO (StablePtr WrappedComms)
startMachine :: IO (StablePtr WrappedComms)
startMachine = do
  ts <- newTMVarIO newTherac
  ecc <- atomically newTChan
  _ <- forkIO $ handleExternalCalls ts ecc
  _ <- forkIO $ treat ts
  _ <- forkIO $ housekeeper ts
  newStablePtr $ WrappedComms ts ecc
  

data StateInfoRequest = RequestTreatmentOutcome| RequestActiveSubsystem | RequestTreatmentState | RequestReason | RequestBeamMode | RequestBeamEnergy
type SIRInt = Int

siriMap :: M.Map Int StateInfoRequest
siriMap = M.fromList [(1,RequestTreatmentOutcome),(2,RequestActiveSubsystem),(3,RequestTreatmentState),(4,RequestReason),(5,RequestBeamMode),(6,RequestBeamEnergy)]

-- external return requested state info
foreign export ccall requestStateInfo :: StablePtr WrappedComms -> SIRInt -> IO CString
requestStateInfo :: StablePtr WrappedComms -> SIRInt -> IO CString
requestStateInfo mywc siri = do
  mywc' <- deRefStablePtr mywc
  let ts = _wrappedCommsTheracState mywc'

  ts' <- atomically $ readTMVar ts
  newCString $ case fromJust $ M.lookup siri siriMap of
                 RequestTreatmentOutcome -> ts' ^. treatmentOutcome
                 RequestActiveSubsystem -> if (ts' ^. dataEntryComplete) then "TREAT" else "DATA ENTRY"
                 RequestTreatmentState -> show $ ts' ^. tPhase
                 RequestReason -> let to = ts' ^. treatmentOutcome in if (to == "TREATMENT OK" || to == "" ) then "OPERATOR" else to
                 RequestBeamMode -> show $ ts' ^. hardwareMeos . datentParams . _1
                 RequestBeamEnergy -> show $ ts' ^. hardwareMeos . datentParams ._2

-- BEGIN TP_SetupTest phase



-- `treat` TP_SetupTest subroutine
-- increment Class3 on each cycle
-- every time class3 % 255 == 0 && class3 == 0 we also set _theracStateClass3Ignore and we really zero it out
-- if FSmall == False then set tphase TP_SetupDone
setupTest :: TMVar TheracState -> IO ()
setupTest ts = do
  atomically $ do
   ts' <- readTMVar ts
   let c3 = ts' ^. class3
   let nextTPhase = if ts' ^. fSmall then ts' ^. tPhase else TP_SetupDone
   case c3 of
     255 -> void $ swapTMVar ts $ ts' {_theracStateClass3 = 0, _theracStateClass3Ignore = True, _theracStateTPhase = nextTPhase}
     0 -> void $ swapTMVar ts $ ts' {_theracStateClass3 = 1, _theracStateClass3Ignore = True, _theracStateTPhase = nextTPhase}
     _ -> void $ swapTMVar ts $ ts' {_theracStateClass3 = c3+1, _theracStateClass3Ignore = False, _theracStateTPhase = nextTPhase}

-- END TP_SetupTest phase

-- BEGIN `housekeeper` stuff
-- `housekeeper` subroutine - "analog-to-digital limit checking" -- if Class3 /= 0 then call chkcol
lmtchk :: TMVar TheracState -> STM ()
lmtchk ts = do
  ts' <- readTMVar ts
  let c3i = ts' ^. class3Ignore
  if c3i then
    setFieldInStruct fSmall False ts
  else chkcol ts

-- `housekeeper` subroutine - checks if CollimatorPosition is consistent with MEOS CollimatorPosition (_theracStateConsoleMeos vs _theracStateHardwareMeos), sets F$mall if not
chkcol :: TMVar TheracState -> STM ()
chkcol ts = do
 ts' <- readTMVar ts
 let tscmcol = ts' ^. consoleMeos . handParams
 let tshmcol = ts' ^. hardwareMeos . handParams
 setFieldInStruct fSmall (tscmcol /= tshmcol) ts

syncCollimator :: TMVar TheracState -> STM ()
syncCollimator ts = do
  ts' <- readTMVar ts
  let tscmcol = ts' ^. consoleMeos . handParams
  void $ swapTMVar ts $ hardwareMeos .~ ((ts' ^. hardwareMeos) {_mEOSHandParams = tscmcol}) $ ts'

-- task - runs concurrently to other stuff - displays messages to monitor, checks setup verification, decodes info, sets collimator position
housekeeper :: TMVar TheracState -> IO ()
housekeeper ts = forever $ do
  threadDelay 1666 -- check 60 times per second because I couldn't think of a good way to make this wait cooperatively with STM retry and I don't want to benchmark how fast a cpu core can do this
  c3i <- atomically $ readFieldFromStruct class3Ignore ts
  if c3i then return () else atomically $ syncCollimator ts
  atomically $ lmtchk ts

-- END `housekeeper` stuff


-- BEGIN TP_Datent stuff

setBendingMagnetFlag :: TMVar TheracState -> STM ()
setBendingMagnetFlag = setFieldInStruct dataEntryComplete True

unsetBendingMagnetFlag :: TMVar TheracState -> STM ()
unsetBendingMagnetFlag = setFieldInStruct bendingMagnetFlag False

-- subroutine - part of `treat` TP_Datent - spin until hysteresis delay expired
-- pseudocode adapted from Leveson 2010
-- repeat
--   if _theracStateBendingMagnetFlag is set then
--     if _theracStateEditingTakingPlace then
--       if _theracStateConsoleMeos changed then exit -- reschedule entire `datent`
-- until hysteresis delay has expired -- 2 seconds per magnet
-- Clear _theracStateBendingMagnetFlag
-- return
pTime :: TMVar TheracState -> IO Bool
pTime ts = do
  -- it's intentional that we use the worst method of reading each value to simulate american engineering
  tscm' <- atomically $ readFieldFromStruct consoleMeos ts
  tsbmf <- atomically $ readFieldFromStruct bendingMagnetFlag ts
  tsetp <- atomically $ readFieldFromStruct editingTakingPlace ts
  tscm'' <- atomically $ readFieldFromStruct consoleMeos ts -- maybe the check between specified and programmed values was actually less nonsensical in the real hardware but this was reported
  -- ^ maybe this is where they were trying to check for cosmic rays or some shit ?


  case (tsbmf,tsetp,tscm' /= tscm'') of
    (True,True,True) -> return True
    _ -> do
      threadDelay 2000000
      atomically $ unsetBendingMagnetFlag ts -- yes, we unset this after the first execution of the loop
      return False

--copyMEOSFromConsole :: TMVar TheracState -> STM ()
--copyMEOSFromConsole ts = do
--  takeTMVar ts >>= \l -> putTMVar ts $ hardwareMeos .~ (l ^. consoleMeos) $ l

copyBeamAndEnergyToHardwareMEOS :: TMVar TheracState -> BeamType -> BeamEnergy -> STM ()
copyBeamAndEnergyToHardwareMEOS ts wantedBeamType wantedBeamEnergy = do
  ts' <- readTMVar ts
  void $ swapTMVar ts $ hardwareMeos .~ ((ts' ^. hardwareMeos) {_mEOSDatentParams = (wantedBeamType, wantedBeamEnergy)}) $ ts'

-- subroutine `magnet` - set bending magnet - part of `treat` TP_Datent
-- pseudocode adapted from Leveson 2010
-- Set _theracStateBendingMagnetFlag
-- foreach l_magnet in [mag1,mag2,mag3,mag4,mag5]
--   set _theracStateHardwareMeos ^. datentParams -- just set the same thing 5 times or whatever
--   call Ptime -- 2s delay per magnet
-- return
magnet :: TMVar TheracState -> BeamType -> BeamEnergy -> IO Bool
magnet ts wantedBeamType wantedBeamEnergy = do
  atomically $ setBendingMagnetFlag ts
  let numLoops = 5 :: Int
  let setTheMagnet n = do
        if n == 1 then return False
          else do
            atomically $ copyBeamAndEnergyToHardwareMEOS ts wantedBeamType wantedBeamEnergy
            shouldSC <- pTime ts -- no I will not be raising an exception to simulate a short-circuit, fuck ghc exceptions
            if shouldSC then
              return True
            else
              setTheMagnet (n-1)
  setTheMagnet numLoops

-- subroutine `TP_Datent` - part of `treat`
-- pseudocode adapted from Leveson 2010
-- if _theracStateConsoleMeos specified then 
--   call `magnet` -- this actually sets (_theracStateHardwareMeos ^. datentParams)
-- if _theracStateDataEntryComplete then set Tphase to TP_SetupTest
-- if not _theracStateDataEntryComplete then
--   if _theracStateResetPending then set Tphase to TP_Reset
datent :: TMVar TheracState -> IO ()
datent ts = do
  (consoleMEOSBeamType,consoleMEOSBeamEnergy) <- atomically $ readTMVar ts >>= \l -> return $ l ^. consoleMeos . datentParams
  (hardwareMEOSBeamType,hardwareMEOSBeamEnergy)<- atomically $ readTMVar ts >>= \l -> return $ l ^. hardwareMeos . datentParams
  sc <- if (hardwareMEOSBeamType /= consoleMEOSBeamType) || (consoleMEOSBeamEnergy /= hardwareMEOSBeamEnergy) then
          magnet ts consoleMEOSBeamType consoleMEOSBeamEnergy
        else return False
  if not sc then do
    tsdec <- atomically $ readFieldFromStruct dataEntryComplete ts
    if tsdec then
      atomically $ setTPhase TP_SetupTest ts
    else do
      tsrp <- atomically $ readFieldFromStruct resetPending ts
      when tsrp $ atomically $ setTPhase TP_Reset ts
  else do
    atomically $ setTPhase TP_Datent ts
    return ()

-- END TP_Datent stuff
