module Utils where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

readEnv :: Read a => String -> a -> IO a
readEnv key def = do
    mVal <- lookupEnv key
    pure $ case mVal of
        Just val -> case readMaybe val of
            Just rval -> rval
            _ -> def
        _ -> def

whenVerbose :: IO () -> IO ()
whenVerbose action = do
    vb <- readEnv "VERBOSE" False
    if vb then action else pure ()
