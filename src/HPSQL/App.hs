
module HPSQL.App
    ( repl
    ) where


import System.IO
import HPSQL.Parser
import Text.Pretty.Simple


repl :: IO ()
repl = do
    input <- prompt "hpsql> "
    case input of
        "" ->
            repl
        ":q" ->
            print "Bye!"
        query -> do
            pPrint (parse query)
            repl

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

