module Main where

import GHC
import GHC.Paths (libdir)
import GHC.Utils.Monad (gcatch)
import GHC.Driver.Session (setSessionDynFlags)
import GHC.Driver.Main (loadWithLogger, LoadHowMuch(..))

loadSourceGhc :: String -> Ghc (Maybe String)
loadSourceGhc path = do
    dflags <- getSessionDynFlags
    setSessionDynFlags (dflags { ghcLink = LinkInMemory, hscTarget = HscInterpreted })
    target <- guessTarget path Nothing
    addTarget target
    result <- loadWithLogger throwingLogger LoadAllTargets
    case result of
        Failed -> return $ Just "Failed to load module"
        Succeeded -> return Nothing

main :: IO ()
main = runGhc (Just libdir) $ do
    loadResult <- loadSourceGhc "LoadExample.hs"
    case loadResult of
        Just err -> putStrLn err
        Nothing -> putStrLn "Module loaded successfully"
