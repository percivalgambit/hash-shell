module Main where

import           Control.Monad                  ( unless )
import           Data.Functor                   ( (<&>) )
import           System.IO                      ( hFlush
                                                , hPrint
                                                , isEOF
                                                , stderr
                                                , stdout
                                                )
import           System.IO.Error                ( catchIOError )
import           System.Posix                   ( changeWorkingDirectory
                                                , executeFile
                                                , forkProcess
                                                , getProcessStatus
                                                )

main :: IO ()
main = shellLoop

shellLoop :: IO ()
shellLoop = do
  putStr "$ "
  hFlush stdout
  done <- isEOF
  unless done $ do
    line <- getLine <&> words
    case line of
      []               -> return ()
      (command : args) -> catchIOError (execLine command args) (hPrint stderr)
    shellLoop

execLine :: String -> [String] -> IO ()
execLine "cd"    []    = ioError $ userError "cd: too few arguments"
execLine "cd"    [dir] = changeWorkingDirectory dir
execLine "cd"    _     = ioError $ userError "cd: too few arguments"
execLine command args  = do
  pid <- forkProcess $ executeFile command True args Nothing
  _   <- getProcessStatus True False pid
  return ()
