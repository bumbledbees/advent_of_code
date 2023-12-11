import Control.Monad
import System.IO
import System.Environment (getArgs)
import System.Exit
import Text.Read (readMaybe)


data Card = Card
    { num :: Int
    , copies :: Int
    , winNums :: [Int]
    , cardNums :: [Int]
    } deriving (Show)


parse :: String -> Card -> [String] -> Card
parse _ card [] = card

parse prevWord card (word:ws) = do
    -- if last word was "Card", this word is the card number and we should
    -- trim the colon off its end
    let word' = if prevWord == "Card" then init word else word
    case readMaybe word' of
        Just n | word == word' ->
            if prevWord == "|" then
                parse word card{cardNums = [n]} ws
            else
                if null (cardNums card) then
                    parse word card{winNums = (winNums card) ++ [n]} ws
                else
                    parse word card{cardNums = (cardNums card) ++ [n]} ws
        Just n  -> parse word card{num = n} ws
        Nothing -> parse word card ws


matches :: Card -> [Int]
matches card = filter (\n -> n `elem` cardNums card) (winNums card)


score :: [Int] -> Int
score xs = if null xs then 0 else 2 ^ (length xs - 1)


addLists :: Num a => [a] -> [a] -> [a] -> [a] 
addLists sum a b = case (a, b) of 
    ([], []) -> sum
    (a, []) -> addLists (sum ++ [head a]) (tail a) []
    ([], b) -> addLists (sum ++ [head b]) [] (tail b)
    (a, b) -> addLists (sum ++ [head a + head b]) (tail a) (tail b)


update :: [Int] -> [Card] -> [Card] -> [Card]
update _ cards [] = cards

update [] cardStack (c:cs) = update newStack (cardStack ++ [c]) cs
    where newStack = replicate (length $ matches c) (copies c)

update copyStack cardStack (c:cs) = do
    let card' = c{copies = (copies c) + (head copyStack)}
    let newStack = replicate (length $ matches c) (copies card')
    let copyStack' = addLists [] (tail copyStack) newStack
    update copyStack' (cardStack ++ [card']) cs


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
    
    let cards = map (parse "" (Card 0 1 [] [])) (words <$> contents)
    let winningNumbers = map matches cards
    let scores = map score winningNumbers

    putStrLn $ "Sum of scores: " ++ (show (sum scores))

    let cards' = update [] [] cards
    let numCards = sum [copies c | c <- cards']

    putStrLn $ "Number of scratchcards: " ++ show numCards

    exitWith ExitSuccess
