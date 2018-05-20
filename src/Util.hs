module Util
    ( debugMode
    , debugPutStrLn
    , traceM, traceShowM
    ) where

import Control.Monad.State
import qualified Debug.Trace

debugMode :: Integer
debugMode = 1

debugPutStrLn:: String -> IO ()
debugPutStrLn arg = when (debugMode > 0) (putStrLn arg)

traceM :: Applicative f => String -> f ()
traceM x = when (debugMode > 0) (Debug.Trace.traceM x)

traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM x = when (debugMode > 0) (Debug.Trace.traceShowM x)