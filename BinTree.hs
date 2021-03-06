module BinTree(BinTree(Empty, Node), treeFromList, subTree) where

import Data.String.Utils (join)
import Data.Random.Source.IO
import Data.Random.Sample (sample)
import Data.Random.Distribution.Uniform (uniform)
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.ST
import Control.Monad.Random (getStdGen)
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Monoid

data BinTree a = Empty | Node a (BinTree a) (BinTree a)

instance Functor BinTree where
    fmap = fmapDefault

instance Applicative BinTree where
    pure = treeFromList . repeat
    _ <*> Empty = Empty
    Empty <*> _ = Empty
    (Node f fl fr) <*>  (Node x l r) = Node (f x) (fl <*> l) (fr <*> r)

instance Foldable BinTree where
    foldMap = foldMapDefault

instance Traversable BinTree where
    traverse f Empty = pure Empty
    traverse f  (Node x l r) = Node <$> f x <*> traverse f l <*> traverse f r

split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:xs) = let ~(e, o) = split xs in (x:o, e)

treeFromList :: [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = let (e, o) = split xs in Node x (treeFromList e) (treeFromList o)

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
            treeList :: [String]
            treeList = map show (listFromTree t)
            showDepth :: Int -> [String] -> String
            showDepth depth t = join " "
                (sublist (fstIndx depth) (lstIndx depth) t) ++ "\n"
            show' :: Int -> [String] -> String
            show' depth treeList
                | fstIndx depth > length treeList = ""
                | otherwise        = showDepth depth treeList
                    ++ show' (depth + 1) treeList

listFromTree :: BinTree a -> [a]
listFromTree tree = bf [tree]
    where
        bf :: [BinTree a] -> [a]
        bf [] = []
        bf xs = map nodeValue xs ++ bf (Prelude.concatMap children xs)
        nodeValue :: BinTree a -> a
        nodeValue (Node a _ _) = a
        children Empty = []
        children (Node _ Empty Empty) = []
        children (Node _ Empty b) = [b]
        children (Node _ a Empty) = [a]
        children (Node _ a b) = [a, b]
