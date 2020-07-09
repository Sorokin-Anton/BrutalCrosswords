import Data.List ( transpose )
import Data.Char ( isSpace )

data Cell = Empty | Block | Filled Char
    deriving Eq

instance Show Cell where
    show Empty = "-"
    show Block = "+"
    show (Filled c) = [ c ]

instance Read Cell where
    readsPrec _ s = [ ( parse s, "" ) ]
      where
        parse :: String -> Cell
        parse "+" = Block
        parse "-" = Empty
        parse [ c ] = Filled c

checkMatch :: Cell -> Char -> Bool
checkMatch Empty = const True
checkMatch Block = const False
checkMatch (Filled c) = (c ==)

newtype Crossword = Crossword { getCrossword :: [ [ Cell ] ] }

-- | Check if crossword is already filled.
noEmpty :: Crossword -> Bool
noEmpty = all (notElem Empty) . getCrossword

-- | Transpose a crossword
tran = Crossword . transpose . getCrossword

instance Show Crossword where
    show = unlines . map (concatMap show) . getCrossword

instance Read Crossword where
    readsPrec _ s = [ ( parse s, "" ) ]
      where
        parse :: String -> Crossword
        parse = Crossword . map (map (read . (: []))) . lines

newtype Words = Words [ String ]
    deriving ( Show, Eq )

instance Read Words where
    readsPrec _ s = [ ( Words $ parse s, "" ) ]
      where
        parse :: String -> [ String ]
        parse "" = []
        parse s = if null rest then [ word ] else word : parse (tail rest)
          where
            ( word, rest ) = span (/= ';') s

-- | Returns a list of correct fillings of a crossword with given word, usinng it zero times or once.
tryPut :: Crossword -> String -> [ Crossword ]
tryPut c s = c : tryPutHorizontal c s ++ tryPutVertical c s
  where
    tryPutHorizontal :: Crossword -> String -> [ Crossword ]
    tryPutHorizontal cr s = [ puttedHorizontal ( x, y ) cr s
                            | y <- [ 1 .. size_y ]
                            , x <- [ 1 .. (size_x - length s + 1) ]
                            , canPutHorizontal ( x, y ) cr s
                            ]
      where
        Crossword arr = cr

        size_x = length $ head arr

        size_y = length arr

    tryPutVertical :: Crossword -> String -> [ Crossword ]
    tryPutVertical cr s = map tran $ tryPutHorizontal (tran cr) s

    puttedHorizontal :: ( Int, Int ) -> Crossword -> String -> Crossword
    puttedHorizontal ( x, y ) (Crossword arr) s = Crossword $ p1 ++ p2 ++ p3
      where
        p1 = take (y - 1) arr

        p2 = [ put_in_line (arr !! (y - 1)) x s ]

        p3 = drop y arr

        put_in_line :: [ Cell ] -> Int -> String -> [ Cell ]
        put_in_line a x s = q1 ++ map Filled s ++ q2
          where
            q1 = take (x - 1) a

            q2 = drop (x - 1 + length s) a

    canPutHorizontal :: ( Int, Int ) -> Crossword -> String -> Bool
    canPutHorizontal ( x, y ) (Crossword arr) = checkPattern (arr !! (y - 1))
      where
        checkPattern :: [ Cell ] -> String -> Bool
        checkPattern a s = and $ zipWith checkMatch part s
          where
            part = drop (x - 1) $ take (-1 + x + length s) a

-- | Trying to put some of given words in crossword.
tryPutMany :: Crossword -> Words -> [ Crossword ]
tryPutMany cr _
    | noEmpty cr = [ cr ]
tryPutMany cr (Words []) = [ cr ]
tryPutMany cr (Words (x : xs)) = tryPut cr x >>= \new_cr -> tryPutMany new_cr
    (Words xs)

main :: IO ()
main = do
    v <- read <$> readFile "crossword.txt"
    s <- read . filter (not . isSpace) <$> readFile "dictionary.txt"
    f . filter noEmpty $ tryPutMany v s
  where
    f [] = putStrLn "No correct fillings"
    f x = mapM_ print x
