import Control.Monad
import System.IO
import System.Environment (getArgs)
import System.Exit


failure :: String -> IO ()
failure msg = do
    hPutStrLn stderr ("Error: " ++ msg)
    exitWith (ExitFailure 1)
     

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) (failure "Please provide an input file.")

    let filePath = head args
    contents <- lines <$> readFile filePath

    -- do stuff here
    mapM_ putStrLn contents

    exitWith ExitSuccess
