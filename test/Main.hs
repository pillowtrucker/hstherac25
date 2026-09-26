module Main (main) where

-- Scenario tests. Each one drives its own machine through the same entry points the UIs use,
-- and they all run at the same time (the slowest takes about 40 s).
-- They check that the historical bugs DO happen when provoked the historical way, and that the
-- paths the original software got right stay safe.

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, when)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (free)
import HsTherac25
import System.Exit (exitFailure)

data Machine = Machine
  { call :: Int -> Int -> Int -> Int -> IO (),
    ask :: Int -> IO String
  }

newMachine :: IO Machine
newMachine = do
  wc <- startMachine
  pure
    Machine
      { call = externalCallWrap wc,
        ask = \n -> do
          p <- requestStateInfo wc n
          s <- peekCString p
          free p
          pure s
      }

xRay, electron :: (Int, Int, Int)
xRay = (1, 1, 25000)
electron = (2, 2, 20000)

sendMEOS :: Machine -> (Int, Int, Int) -> IO ()
sendMEOS m (b, c, e) = call m 1 b c e

begin, proceed, reset, setButton, fieldLight :: Machine -> IO ()
begin m = call m 2 0 0 0
proceed m = call m 5 0 0 0
reset m = call m 4 0 0 0
setButton m = call m 7 0 0 0
fieldLight m = call m 8 0 0 0

outcome, tphase, turntable, beam :: Machine -> IO String
outcome m = ask m 1
tphase m = ask m 3
beam m = ask m 5
turntable m = ask m 9

class3, displayed, patientDose :: Machine -> IO Int
class3 m = read <$> ask m 8
displayed m = read <$> ask m 10
patientDose m = read <$> ask m 11

ms :: Int -> IO ()
ms n = threadDelay (n * 1000)

type Check = IO (Either String ())

ensure :: Bool -> String -> Check
ensure ok msg = pure $ if ok then Right () else Left msg

andThen :: Check -> Check -> Check
andThen a b = a >>= either (pure . Left) (const b)

-- poll until the condition holds, or fail after the timeout (ms)
waitUntil :: Int -> String -> IO Bool -> Check
waitUntil timeout what cond = go timeout
  where
    go t = do
      ok <- cond
      if ok
        then pure (Right ())
        else
          if t <= 0
            then pure (Left ("timed out waiting for " ++ what))
            else ms 20 >> go (t - 20)

beamStopped :: Machine -> IO Bool
beamStopped m = (`elem` ["TP_PauseTreatment", "TP_TerminateTreatment"]) <$> tphase m

-- press P through any nuisance pauses until the treatment is over
finishTreatment :: Machine -> Check
finishTreatment m = go (10 :: Int)
  where
    go 0 = pure (Left "treatment never finished")
    go n = do
      r <- waitUntil 15000 "the beam to stop" (beamStopped m)
      case r of
        Left e -> pure (Left e)
        Right () -> do
          p <- tphase m
          if p == "TP_TerminateTreatment"
            then pure (Right ())
            else proceed m >> ms 300 >> go (n - 1)

describe :: Machine -> IO String
describe m = do
  o <- outcome m
  p <- tphase m
  b <- beam m
  t <- turntable m
  d <- patientDose m
  pure (" [outcome=" ++ show o ++ " phase=" ++ p ++ " beam=" ++ b ++ " turntable=" ++ t ++ " patientDose=" ++ show d ++ "]")

-- Tyler: "made an entry indicating the mode/energy, went to the command line, then moved the
-- cursor up to change the mode/energy, and returned to the command line all within 8 seconds"
tylerRace :: Machine -> IO ()
tylerRace m = do
  sendMEOS m xRay
  begin m
  ms 3500 -- past the first Ptime, so the edit goes unnoticed
  sendMEOS m electron
  begin m

tylerRaceOverdosesThenSuspends :: Check
tylerRaceOverdosesThenSuspends = do
  m <- newMachine
  tylerRace m
  first <-
    waitUntil 15000 "the beam to stop" (beamStopped m)
      `andThen` (outcome m >>= \o -> ensure (o == "MALFUNCTION 54") ("expected MALFUNCTION 54, got " ++ show o))
      `andThen` (displayed m >>= \d -> ensure (d == 6) ("dose monitor should read 6 MU, got " ++ show d))
      `andThen` (patientDose m >>= \d -> ensure (d >= 16500) ("expected a Tyler-sized overdose, got " ++ show d))
      `andThen` (beam m >>= \b -> ensure (b == "BeamTypeXRay") ("hardware should still be set up for X-rays, got " ++ b))
      `andThen` (turntable m >>= \t -> ensure (t == "CollimatorPositionElectronBeam") ("turntable should have followed the edit, got " ++ t))
  case first of
    Left e -> pure (Left e)
    Right () -> do
      -- P four more times: every one is another overdose, and the 5th pause suspends
      let pressP = proceed m >> ms 400
      mapM_ (const pressP) [1 .. 4 :: Int]
      (tphase m >>= \p -> ensure (p == "TP_TerminateTreatment") ("expected treatment suspend after 5 pauses, got " ++ p))
        `andThen` (outcome m >>= \o -> ensure (o == "MALFUNCTION 54") ("suspend should keep the message, got " ++ show o))
        `andThen` (patientDose m >>= \d -> ensure (d >= 5 * 16500) ("expected five overdoses, got " ++ show d))
        `andThen` (reset m >> waitUntil 2000 "reset" ((== "TP_Datent") <$> tphase m))

softResetWorksDuringPause :: Check
softResetWorksDuringPause = do
  m <- newMachine
  tylerRace m
  waitUntil 15000 "the pause" ((== "TP_PauseTreatment") <$> tphase m)
    `andThen` (reset m >> waitUntil 2000 "reset out of the pause" ((== "TP_Datent") <$> tphase m))
    `andThen` (patientDose m >>= \d -> ensure (d == 0) "reset should start a new patient record")

-- "Ptime ... If there are edits, then Ptime clears the bending magnet variable and exits to
-- Magnet, which then exits to Datent": an edit during the FIRST magnet is caught
editDuringFirstMagnetIsCaught :: Check
editDuringFirstMagnetIsCaught = do
  m <- newMachine
  sendMEOS m xRay
  begin m
  ms 500
  sendMEOS m electron
  begin m
  r <- finishTreatment m
  d <- describe m
  pure r
    `andThen` (beam m >>= \b -> ensure (b == "BeamTypeElectron") ("Datent should have redone the setup for electrons" ++ d))
    `andThen` (patientDose m >>= \p -> ensure (p <= 200) ("no overdose expected" ++ d))

noBeginNoBeam :: Check
noBeginNoBeam = do
  m <- newMachine
  sendMEOS m xRay
  ms 10000
  (tphase m >>= \p -> ensure (p == "TP_Datent") ("without Begin the machine must stay in data entry, got " ++ p))
    `andThen` (patientDose m >>= \d -> ensure (d == 0) "no beam without Begin")
    `andThen` (beam m >>= \b -> ensure (b == "BeamTypeXRay") "Datent still sets the hardware up while waiting")

-- used to leave the machine stuck in DATA ENTRY (the magnets set the flag, Begin toggled it off)
beginShortlyAfterEntryTreats :: Check
beginShortlyAfterEntryTreats = do
  m <- newMachine
  sendMEOS m xRay
  ms 300
  begin m
  r <- finishTreatment m
  d <- describe m
  pure r
    `andThen` (patientDose m >>= \p -> ensure (p <= 200) ("no overdose expected" ++ d))
    `andThen` (beam m >>= \b -> ensure (b == "BeamTypeXRay") ("expected an X-ray setup" ++ d))

-- press set when the Class3 counter is in the given range, with the field light on
setButtonAt :: Machine -> (Int, Int) -> Check
setButtonAt m (lo, hi) = do
  sendMEOS m xRay
  fieldLight m
  begin m
  waitUntil 15000 "set-up test with the field light on" ((== "TP_SetupTest") <$> tphase m)
    `andThen` waitUntil 5000 "the turntable to reach the field light" ((== "CollimatorPositionFieldLight") <$> turntable m)
    `andThen` (ask m 12 >>= \p -> ensure (p == "PRESS SET BUTTON") ("expected the set prompt, got " ++ show p))
    `andThen` waitUntil 40000 "Class3 to come round" ((\c -> c >= lo && c <= hi) <$> class3 m)
    `andThen` (setButton m >> pure (Right ()))

-- Yakima: "The overexposure occurred when the operator hit the 'set' button at the precise moment
-- that Class3 rolled over to zero ... the upper collimator was still in field-light position."
yakimaSetAtRolloverOverdoses :: Check
yakimaSetAtRolloverOverdoses = do
  m <- newMachine
  setButtonAt m (244, 250)
    `andThen` waitUntil 5000 "the beam to stop" (beamStopped m)
    `andThen` (describe m >>= \d -> outcome m >>= \o -> ensure (o == "FLATNESS") ("expected FLATNESS" ++ d))
    `andThen` (displayed m >>= \d -> ensure (d == 0) "no ion chamber in the field-light position, so no dose shown")
    `andThen` (patientDose m >>= \d -> ensure (d >= 4000) ("expected a Yakima-sized overdose, got " ++ show d))
    -- "The machine paused again, this time displaying 'flatness'"
    `andThen` (proceed m >> ms 400 >> pure (Right ()))
    `andThen` (describe m >>= \d -> outcome m >>= \o -> ensure (o == "FLATNESS") ("P should repeat it" ++ d))
    `andThen` (patientDose m >>= \d -> ensure (d >= 8000) ("expected two overdoses, got " ++ show d))

yakimaSetAtOtherTimesIsSafe :: Check
yakimaSetAtOtherTimesIsSafe = do
  m <- newMachine
  r <- setButtonAt m (100, 180) `andThen` finishTreatment m
  d <- describe m
  pure r
    `andThen` (patientDose m >>= \p -> ensure (p <= 200) ("no overdose expected" ++ d))
    `andThen` (turntable m >>= \t -> ensure (t == "CollimatorPositionXRay") ("turntable should be back in the X-ray position" ++ d))

-- used to kill the keyboard handler, or poison the state and crash the host on the next request
badInputFromUIIsIgnored :: Check
badInputFromUIIsIgnored = do
  m <- newMachine
  call m 0 1 1 25000
  call m 99 1 1 25000
  sendMEOS m (0, 1, 25000)
  sendMEOS m (1, 0, 25000)
  sendMEOS m (7, 7, 25000)
  ms 500
  unknown <- ask m 99
  dump <- ask m 7
  sendMEOS m xRay
  begin m
  ensure (unknown == "") "unknown request should give an empty string"
    `andThen` ensure (take 11 dump == "TheracState") ("full state dump should work, got " ++ show dump)
    `andThen` waitUntil 15000 "the valid calls to still be handled" ((/= "TP_Datent") <$> tphase m)

scenarios :: [(String, Check)]
scenarios =
  [ ("Tyler race gives MALFUNCTION 54 and an overdose, P repeats it, 5th pause suspends", tylerRaceOverdosesThenSuspends),
    ("soft reset works during a pause", softResetWorksDuringPause),
    ("an edit during the first magnet is caught", editDuringFirstMagnetIsCaught),
    ("no Begin, no beam", noBeginNoBeam),
    ("Begin shortly after entering the prescription treats normally", beginShortlyAfterEntryTreats),
    ("Yakima: set just before Class3 rolls over gives an overdose, P repeats it", yakimaSetAtRolloverOverdoses),
    ("Yakima: set at any other time is safe", yakimaSetAtOtherTimesIsSafe),
    ("bad input from the UI is ignored", badInputFromUIIsIgnored)
  ]

main :: IO ()
main = do
  pending <- forM scenarios $ \(name, check) -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      r <- try check
      putMVar done $ case r of
        Left (e :: SomeException) -> Left ("exception: " ++ show e)
        Right x -> x
    pure (name, done)
  results <- forM pending $ \(name, done) -> do
    r <- takeMVar done
    putStrLn $ either (\e -> "FAIL " ++ name ++ ": " ++ e) (const ("ok   " ++ name)) r
    pure r
  let failures = length [() | Left _ <- results]
  when (failures > 0) $ putStrLn (show failures ++ " scenario(s) failed")
  unless (failures == 0) exitFailure
