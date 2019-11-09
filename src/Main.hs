module Main where
import           Lexer
import           Text.Megaparsec
import qualified Data.Text.IO                  as T
import qualified Control.Monad.State.Lazy      as S

main :: IO ()
main = do
    file <- T.getContents
    let tokens = S.evalState (runParserT programParser "x" file) [1]
    putStr $ either errorBundlePretty show tokens
    return ()

