{-# LANGUAGE OverloadedStrings #-}
{-
   give me example code for using unagi-chan in Haskell
   with multiple input threads and multiple worker
   threads
-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random
import Text.Printf
import System.IO

-- | Data type to be sent through the channel
data WorkItem = WorkItem 
  { workId :: Int       -- Unique identifier for the work item
  , workLoad :: Int     -- Amount of "work" to do (simulated with delay)
  , sourceThread :: Int -- Which input thread created this item
  } deriving (Show)

-- | Create a new work item with random workload
createWorkItem :: Int -> Int -> IO WorkItem
createWorkItem threadId itemId = do
  load <- randomRIO (100000, 500000) -- Random delay between 0.1 and 0.5 seconds
  return $ WorkItem itemId load threadId

-- | Input producer thread function
inputProducer :: Int -> U.InChan WorkItem -> Int -> IO ()
inputProducer threadId chan itemCount = do
  printf "[Producer %d] Starting, will generate %d items\n" threadId itemCount
  forM_ [1..itemCount] $ \i -> do
    workItem <- createWorkItem threadId i
    U.writeChan chan workItem
    printf "[Producer %d] Submitted item %d with workload %d\n" 
           threadId i (workLoad workItem)
    threadDelay 50000  -- 50ms between submissions
  printf "[Producer %d] Finished\n" threadId

-- | Worker thread function
worker :: Int -> U.OutChan WorkItem -> MVar Int -> IO ()
worker workerId chan counterMVar = do
  printf "[Worker %d] Starting\n" workerId
  forever $ do
    workItem <- U.readChan chan
    let delay = workLoad workItem
    printf "[Worker %d] Processing item %d from producer %d (workload: %d)\n" 
           workerId (workId workItem) (sourceThread workItem) delay
    
    -- Simulate work by delaying
    threadDelay delay
    
    -- Update completed counter
    modifyMVar_ counterMVar $ \count -> return (count + 1)
    
    printf "[Worker %d] Completed item %d from producer %d\n" 
           workerId (workId workItem) (sourceThread workItem)

-- | Monitor thread to track progress
monitor :: MVar Int -> Int -> IO ()
monitor counterMVar totalItems = do
  let loop = do
        count <- readMVar counterMVar
        printf "[Monitor] Progress: %d/%d items completed (%.1f%%)\n" 
               count totalItems (100 * fromIntegral count / fromIntegral totalItems :: Double)
        if count >= totalItems
          then printf "[Monitor] All items completed!\n"
          else do
            threadDelay 1000000  -- Check every second
            loop
  loop

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering  -- Ensure output appears immediately
  
  -- Configuration
  let numProducers = 20
      itemsPerProducer = 20
      numWorkers = 10
      totalItems = numProducers * itemsPerProducer
  
  -- Create unagi channel
  (inChan, outChan) <- U.newChan
  
  counterMVar     <- newMVar 0
  monitorThread   <- async $ monitor counterMVar totalItems
  workerThreads   <- forM [1..numWorkers] $ \i   -> async $ worker i outChan counterMVar
  producerThreads <- forM [1..numProducers] $ \i -> async $ inputProducer i inChan itemsPerProducer
  
  -- Wait for all producers to finish
  putStrLn "Waiting for producers to finish..."
  mapM_ wait producerThreads
  putStrLn "All producers have finished"
  
  -- Wait for all items to be processed
  putStrLn "Waiting for workers to process all items..."
  wait monitorThread
  
  -- Cancel worker threads as they run forever
  putStrLn "Cancelling worker threads..."
  mapM_ cancel workerThreads
  
  putStrLn "Program completed successfully"
