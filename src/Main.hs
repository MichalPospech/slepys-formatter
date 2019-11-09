module Main where
import Lexer    
import Text.Megaparsec
import qualified Data.Text.IO as T

main :: IO ()
main = do
    file <- T.getContents
    parseTest programParser file
    return ()
