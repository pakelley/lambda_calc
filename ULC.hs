module ULC where

import Syntax
import Parser

import Control.Monad.Trans
import System.Console.Haskeline

----------------------- process input -----------------------
process :: String -> IO ()
process input = do
  let res = parseLC input
  case res of
    Left err -> print "Error"
    Right ex -> print ex

-------------------------- repl --------------------------
repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    lcinput <- getInputLine "ulc_repl> "
    case lcinput of
      Nothing -> outputStrLn "Goodbye :)"
      Just input -> (liftIO $ process input) >> loop
