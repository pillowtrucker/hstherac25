module Main (main) where
import HsTherac25
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Concurrent(forkIO, threadDelay, killThread)
import Control.Monad (forever)
import Control.Concurrent.STM
import Foreign.StablePtr
import Control.Monad.IO.Class
import Foreign.C.String(peekCString)
import Control.Lens
main :: IO ()
main = do
  wc' <- startMachine
  wc <- deRefStablePtr wc'
  let ts' = wc ^. theracState
  let lol = do
        ts <- atomically $ takeTMVar ts'
        putStrLn "CONTROL:"
        putStrLn $ show $! ts
        putStrLn "END CONTROL."
        atomically $! putTMVar ts' ts
  let simulateToggleDatentComplete = externalCallWrap wc' 2 1 1 25000
  let simulateSendMeos = externalCallWrap wc' 1
  let simulateProceedButton = externalCallWrap wc' 5 1 1 25000
  let simulateToggleEditingTakingPlace = externalCallWrap wc' 3 1 1 25000
  let simulateResetButton = externalCallWrap wc' 4 1 1 25000
  let requestTreatmentOutcome = requestStateInfo wc' 1
  let requestActiveSubsystem = requestStateInfo wc' 2
  let requestTreatmentState = requestStateInfo wc' 3
  let requestReason = requestStateInfo wc' 4
  let requestBeamMode = requestStateInfo wc' 5
  let requestBeamEnergy = requestStateInfo wc' 6
  let normalOperation = do
        simulateSendMeos 1 1 25000
        simulateToggleEditingTakingPlace
        simulateToggleDatentComplete
  let failureAlgorithm = do
        simulateSendMeos 1 1 25000
        simulateToggleEditingTakingPlace
        simulateToggleDatentComplete
        threadDelay 500000
        simulateToggleEditingTakingPlace
        simulateToggleDatentComplete
        threadDelay 500000
        simulateSendMeos 2 2 36
        simulateToggleEditingTakingPlace
        simulateToggleDatentComplete
        
  let loop = do
        outcome' <- requestTreatmentOutcome
        outcome <- peekCString outcome'
        requestActiveSubsystem >>= peekCString >>= putStrLn
        requestTreatmentState >>= peekCString >>= putStrLn
        requestReason >>= peekCString >>= putStrLn
        requestBeamMode >>= peekCString >>= putStrLn
        requestBeamEnergy >>= peekCString >>= putStrLn
        putStrLn "TREATMENT OUTCOME:"
        case outcome of
          "MALFUNCTION 54" -> do
            putStrLn "FRIED CHICKEN"
            lol
          "TREATMENT OK" -> do
            putStrLn outcome
            lol
            putStrLn "RESETTING MACHINE"
            simulateResetButton
            threadDelay 1000000
            failureAlgorithm
            loop
          "" -> do putStrLn "TREATMENT IN PROGRESS"
                   lol
                   threadDelay 1000000
                   loop
          o -> do
            putStrLn o
            lol
            putStrLn "PROCEEDING"
            threadDelay 1000000
            simulateProceedButton
            loop
--  failureAlgorithm
  normalOperation
  loop
--    atomically $ toggleDatentComplete $! ts'
--    atomically $ toggleEditingTakingPlace $! ts'
    
--    l <- atomically $ takeTMVar ts'
--    putStrLn "FROM:"
--    putStrLn $ show $ l
--    putStrLn "TO:"
--    putStrLn $ show (dataEntryComplete %~ not $! l)
--    atomically $ putTMVar ts' (dataEntryComplete %~ not $! l)
--    atomically $ putTMVar ts' (treatmentOutcome %~ (++ "a") $! l)    
