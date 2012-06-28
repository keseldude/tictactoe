module Main where

import Data.Array.IO
import GHC.IOArray
import Data.List
import System.IO
import Control.Monad
import Data.Maybe
import Control.Exception

data Move = X | O deriving (Show, Eq)

same (Just X) (Just X) = True
same (Just O) (Just O) = True
same _ _ = False

checkSame [] = False
checkSame (_:[]) = True
checkSame (x:xs@(y:_)) = same x y && checkSame xs

moveString :: Maybe Move -> String
moveString (Just m) = show m
moveString Nothing = "_"

newtype Board = Board { boardArr :: IOArray (Int, Int) (Maybe Move) }

boardRange arr = ranges' (boundsIOArray arr)
    where ranges' ((startX, _), (endX, _)) = [startX .. endX]

positions arr = let r = boardRange arr
    in [[(x,y) | y <- r] | x <- r]

boardString (Board arr) = do
    elements <- mapM (mapM (readArray arr)) $ positions arr
    return . unlines . intersperse "" $ map (intersperse ' ' . unwords . map moveString) elements

makeBoard size = do
    arr <- newArray ((1, 1), (size, size)) Nothing :: IO (IOArray (Int, Int) (Maybe Move))
    return $ Board arr

getData :: Board -> (Int, Int) -> IO (Either ErrorCall (Maybe Move))
getData (Board arr) = try . readArray arr
setData :: Board -> (Int, Int) -> Maybe Move -> IO (Either ErrorCall ())
setData (Board arr) loc = try . writeArray arr loc

rows range = [[(x, y) | y <- range] | x <- range]
cols range = [[(x, y) | x <- range] | y <- range]
diag1 range = [(x, x) | x <- range]
diag2 range = let   startX = head range
                    endX = last range
    in [(x, endX - x + startX) | x <- range]

playerWon (Board arr) = let r = boardRange arr
    in playerWon' (diag1 r : diag2 r : (rows r ++ cols r))
    where   playerWon' = liftM or . mapM isWinningDir
            isWinningDir = liftM checkSame . mapM (readArray arr)

isTie (Board arr) = liftM (null . filter (not . isJust)) $ mapM (readArray arr) (range . boundsIOArray $ arr)

printBoard = (>>= putStrLn) . boardString

other X = O
other O = X

putErrLn = hPutStrLn stderr

moveLoop board player = do
    putStrLn $ "Player " ++ show player ++ "'s Turn"
    printBoard board
    moveLoop'
    where   moveLoop' = do
                putStr "Enter move location (row, col) (top left is (1, 1)): "
                hFlush stdout
                location <- getLine
                case liftM fst . listToMaybe $ reads location of
                    Nothing -> invalidMove
                    Just l -> makeMove l
            invalidMove = do
                putErrLn "Invalid location! Try again."
                moveLoop'
            makeMove location = do
                eith <- getData board location
                case eith of
                    Left e -> invalidMove
                    Right d -> case d of
                        Nothing -> makeMove' location
                        _ -> invalidMove
            makeMove' location = do
                setData board location (Just player)
                won <- playerWon board
                if won
                    then do
                        printBoard board
                        putStrLn $ "Player " ++ show player ++ " won!"
                    else do
                        tie <- isTie board
                        if tie
                            then do
                                printBoard board
                                putStrLn "It's a tie!"
                            else moveLoop board (other player)

gameLoop = do
    putStr "Enter the size of the tic tac toe board: "
    hFlush stdout
    size <- getLine
    case liftM fst . listToMaybe $ reads size of
        Nothing -> badBoardSize
        Just s -> if s < 3
            then badBoardSize
            else do
                board <- makeBoard s
                moveLoop board X
    where   badBoardSize = do
                putErrLn "Bad board size! Try again."
                gameLoop

testBoard = do
    b <- makeBoard 3
    let arr = boardArr b
    writeArray arr (1,1) (Just O)
    writeArray arr (1,2) (Just X)
    writeArray arr (1,3) (Just O)
    writeArray arr (2,2) (Just X)
    writeArray arr (2,3) (Just O)
    writeArray arr (3,3) (Just X)
    return b

main = gameLoop
