import Data.String.Utils (join)
import System.Random (getStdGen)
import Control.Monad.Random.Class (MonadRandom)

data BinTree a = Empty | Node a (BinTree a) (BinTree a)

instance Functor BinTree where
    fmap _ Empty = Empty
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = let ~(e, o) = split xs in (x:o, e)

treeFromList :: [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = let (e, o) = split xs in Node x (treeFromList e) (treeFromList o)

myTree = treeFromList [0..]

sublist :: Int -> Int -> [a] -> [a]
sublist a b t = drop a . take b $ t

fstIndx :: Int -> Int
fstIndx depth = 2^depth - 1

lstIndx :: Int -> Int
lstIndx depth = 2^(depth + 1) - 1

subTree :: Int -> BinTree a -> BinTree a
subTree 0 _ = Empty
subTree n Empty = Empty
subTree n (Node x l r) = Node x (subTree (n-1) l) (subTree (n-1) r)


instance (Show a) => Show (BinTree a) where
    show t = show' 0 treeList
        where
            maxDepth = 5
            treeList :: [String]
            treeList = map show (listFromTree t)
            showDepth :: Int -> [String] -> String
            showDepth depth t = join " "
                (sublist (fstIndx depth) (lstIndx depth) t) ++ "\n"
            show' :: Int -> [String] -> String
            show' depth treeList
                | fstIndx depth > length treeList = ""
                | depth > maxDepth = "..."
                | otherwise        = showDepth depth treeList
                    ++ show' (depth + 1) treeList

listFromTree :: BinTree a -> [a]
listFromTree tree = bf [tree]
    where
        bf :: [BinTree a] -> [a]
        bf [] = []
        bf xs = map nodeValue xs ++ bf (concatMap children xs)
        nodeValue :: BinTree a -> a
        nodeValue (Node a _ _) = a
        children (Node _ Empty Empty) = []
        children (Node _ Empty b) = [b]
        children (Node _ a Empty) = [a]
        children (Node _ a b) = [a, b]

main :: IO ()
main = do
        gen <- MonadRandom [1, 2]
        print gen
