{-# OPTIONS_GHC -fwarn-overlapping-patterns -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches #-}
{-
    Main.hs
    - http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/
    - http://lxr.mozilla.org/mozilla/source/js/src/
-}

module Main where
import System.Environment
import IO
import Control.Monad.State
import Control.Monad.Cont

import Parser
import DataTypes 
import Context
import Eval
import Init

runText :: String -> Evaluate Value
runText input =
    case runLex program input of
         Left err -> liftAll $ do { hPutStrLn stderr "parse error:"; hPutStrLn stderr $ showError input err; return Void }
         Right program ->
             do liftAll $ do { hPutStr stderr "runText: program: "; mapM (hPrint stderr) program }
                if null program
                   then return Void
                   else liftM last $ mapM eval program
--              liftAll $ hPrint stderr evaluated
--              toString evaluated >>= liftAll . hPutStrLn stderr

runFile :: String -> IO ()
runFile filename =
    do content <- readFile filename
       nullFrame <- nullFrame
       ((do setupEnv
            e <- callCC $ \cc -> do { pushCont cc CThrow; runText content; return Void }
            when (isException e)
                 (liftAll $ print $ exceptionBody e))
        `runContT` (const $ return Void)) `evalStateT` nullFrame
       return ()

runRepl' :: Evaluate ()
runRepl' =
    do line <- liftAll $ do { putStr "js> "; hFlush stdout; getLine }
       value <- runText line
       unless (isVoid value)
              (do string <- toString value
                  liftAll $ putStrLn string)
       runRepl'

runRepl :: IO ()
runRepl =
    do nullFrame <- nullFrame
       (do { setupEnv; setupCatchAndRunRepl } `runContT` (const $ return Void)) `evalStateT` nullFrame
       return ()
    where setupCatchAndRunRepl =
              do e <- callCC $ \cc -> do { pushCont cc CThrow; return Void }
                 unless (isVoid e)
                        (do when (isException e) (liftAll $ print $ exceptionBody e)
                            setupCatchAndRunRepl)
                 runRepl'

main :: IO ()
main =
    do args <- getArgs
       case length args of
            0 -> runRepl
            1 -> runFile $ args !! 0
