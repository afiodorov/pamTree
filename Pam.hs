import BinTree (BinTree (Node, Empty), treeFromList, subTree)
import Data.Monoid

runModel :: BinTree a -> BinTree a -> BinTree a
runModel init potential = potential

sumNeighs :: (Monoid a) => BinTree a -> BinTree a
sumNeighs = sumNeighs' mempty

sumNeighs' :: (Monoid a) => a -> BinTree a -> BinTree a
sumNeighs' parentVal Empty = Empty
sumNeighs' parentVal (Node x l r) = Node (parentVal `mappend` nodeVal l `mappend` nodeVal r) (sumNeighs' x l) (sumNeighs' x r)
    where
        nodeVal :: (Monoid a) => BinTree a -> a
        nodeVal (Node x _ _) = x
        nodeVal Empty = mempty

main :: IO ()
main = print . subTree 5 $ treeFromList [0..]
