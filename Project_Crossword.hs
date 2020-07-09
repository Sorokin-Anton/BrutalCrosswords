import Data.List (transpose)


data Cell = Empty | Block | Filled Char

instance Show Cell where
    show Empty = "-"
    show Block = "+"
    show (Filled c) = [c]

instance Read Cell where
    readsPrec _ s = [(parse s, "")] where
        parse :: String -> Cell
        parse "+" = Block
        parse "-" = Empty
        parse [c] = Filled c

checkMatch :: Cell -> Char -> Bool
checkMatch Empty = const True
checkMatch Block = const False
checkMatch (Filled c) = (c==)


newtype Crossword = Crossword {getCrossword :: [[Cell]]}

tran = Crossword . transpose . getCrossword

instance Show Crossword where
    show = unlines .  map (concatMap show) . getCrossword

instance Read Crossword where
    readsPrec _ s = [(parse s, "")] where
        parse :: String -> Crossword
        parse = Crossword . map (map (read . (:[]))).lines


newtype Words = Words [String] deriving (Show, Eq)

instance Read Words where
    readsPrec _ s = [(Words $ parse s, "")] where
        parse :: String -> [String]
        parse "" = []
        parse s = if null rest then [word] else word : parse (tail rest) where
            (word, rest) = span (/= ';') s


tryPut :: Crossword -> String -> [Crossword]
tryPut c s = tryPutHorizontal c s ++ tryPutVertical c s

tryPutHorizontal :: Crossword -> String -> [Crossword]
tryPutHorizontal cr s =
    [puttedHorizontal (x,y) cr s | y <- [1..size_y], x <- [1..(size_x - length s + 1)], canPutHorizontal (x,y) cr s]
        where Crossword arr = cr ; size_x = length $ head arr; size_y = length arr


tryPutVertical :: Crossword -> String -> [Crossword]
tryPutVertical (Crossword arr) s = map tran $ tryPutHorizontal (Crossword $ transpose arr) s

puttedHorizontal :: (Int, Int) -> Crossword -> String -> Crossword
puttedHorizontal (x, y) (Crossword arr) s = Crossword $ p1 ++ p2 ++ p3 where
    p1 = take (y-1) arr
    p2 = [put_in_line (arr !! (y-1)) x s]
    p3 = drop y arr
    put_in_line :: [Cell] -> Int -> String -> [Cell]
    put_in_line a x s = q1 ++ map Filled s ++ q2 where
        q1 = take (x - 1) a
        q2 = drop (x - 1 + length s) a

canPutHorizontal :: (Int, Int) -> Crossword -> String -> Bool
canPutHorizontal (x, y) (Crossword arr) s = checkPattern (arr!!(y-1)) x s where
    checkPattern :: [Cell] -> Int -> String -> Bool
    checkPattern a x s = and $ zipWith checkMatch part s where
        part = drop (x-1) $ take (-1 + x + length s) a


tryPutMany :: Crossword -> Words -> [Crossword]
tryPutMany cr (Words []) = [cr]
tryPutMany cr (Words (x:xs)) = tryPut cr x >>= \new_cr -> tryPutMany new_cr (Words xs)


main :: IO ()
main = do
    z <- lines <$> readFile "crossword.txt"
    let (v,s) = (read.unlines.init $ z, read.last $ z)
    mapM_ print $ tryPutMany v s
