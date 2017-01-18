module Main where

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Ord

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)

instance Eq a => Eq (Tree a) where
    (==) a b     = case a of
        Node{} -> node a == node b && left a == left b
        Leaf{} -> node a == node b

nodeKey :: Tree (a, b) -> a
nodeKey t = case t of
    (Node n _ _) -> fst n
    (Leaf n)     -> fst n

nodeValue :: Tree (a, b) -> b
nodeValue t = case t of
    (Node n _ _) -> snd n
    (Leaf n)     -> snd n

node :: Tree a -> a
node t = case t of
    (Node n _ _) -> n
    (Leaf n)     -> n

left :: Tree t -> Tree t
left t = case t of
    (Node _ l _) -> l

right :: Tree t -> Tree t
right t = case t of
    (Node _ _ r) -> r


getFrequencyList :: String -> [(String, Int)]
getFrequencyList = map (\ x -> ([head x], length x)) . group . sort

smallest :: [Tree(String, Int)] -> Tree(String, Int)
smallest = minimumBy (comparing nodeValue)

combineLeastFrequentTwo :: [Tree (String, Int)] -> (Tree (String, Int) -> Tree (String, Int) -> Bool) -> [Tree (String, Int)]
combineLeastFrequentTwo leafs predicate =
    let least = smallest leafs
        secondLeast = smallest $ filter (not . predicate least) leafs
        rest = filter (\ x -> not (predicate x least) && not (predicate x secondLeast)) leafs
    in Node ("", nodeValue least + nodeValue secondLeast) least secondLeast : rest

getHuffmanTree :: [Tree (String, Int)] -> (Tree (String, Int) -> Tree (String, Int) -> Bool) -> [Tree (String, Int)]
getHuffmanTree leafs predicate
    | length leafs > 1  = getHuffmanTree (combineLeastFrequentTwo leafs predicate) predicate
    | otherwise         = leafs


getSymbol :: String -> Tree (String, Int) -> Int -> (String, Int)
getSymbol encodedWord huffmanTree depth = case huffmanTree of
    (Leaf n)     -> (fst n, depth)
    (Node n l r) -> getSymbol (tail encodedWord) (if head encodedWord == '0' then l else r) (depth + 1)

eitherOr :: String -> String -> String
eitherOr x y
    | last y == '~'  = x
    | otherwise         = y


{- "Public" API -}

encodeInner :: Tree (String, Int) -> Tree (String, Int) -> String
encodeInner l (Node n left right) = eitherOr ('0' : encodeInner l left) ('1' : encodeInner l right)
encodeInner l l1
    | nodeKey l == nodeKey l1 = ""
    | otherwise = "~"

encode :: String -> (Tree (String, Int) -> Tree (String, Int) -> Bool) -> (Tree (String, Int), String)
encode str predicate = let  freqLst = getFrequencyList str
                            leafs = map Leaf freqLst
                            huffmanTree = head $ getHuffmanTree leafs predicate
                            huffmanCodes = [(nodeKey leaf, encodeInner leaf huffmanTree) | leaf <- leafs]
                            {- I can't seem to make Haskell accept just letter here instead of [letter] -}
                            word = intercalate "" $ map (\letter -> fromJust (lookup [letter] huffmanCodes)) str
             in (huffmanTree, word)

decode :: String -> Tree (String, Int) -> String
decode encodedWord huffmanTree
    | encodedWord == "" = encodedWord
    | otherwise         = symbol ++ decode (drop usedSymbolsNum encodedWord) huffmanTree
    where   symbolTuple = getSymbol encodedWord huffmanTree 0
            symbol = fst symbolTuple
            usedSymbolsNum = snd symbolTuple

main=do
    putStrLn "Enter string:"
    str <- getLine
    {- str = "abracadabra" -}
    let encodedWord = encode str (==)
    print encodedWord
    let decodedWord = decode (snd encodedWord) (fst encodedWord)
    print decodedWord
    return ()

