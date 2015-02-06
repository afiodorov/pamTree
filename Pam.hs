import BinTree (BinTree (Node, Empty), treeFromList, subTree)
import Data.Monoid
import Control.Applicative

runModel :: BinTree a -> BinTree a -> BinTree a
runModel init potential = potential

sumNeighs ::  (Num a) => BinTree a -> BinTree a
sumNeighs t = getSum <$> mconcatNeighs' mempty (fmap Sum t)

mconcatNeighs' :: (Monoid a) => a -> BinTree a -> BinTree a
mconcatNeighs' parentVal Empty = Empty
mconcatNeighs' parentVal (Node x l r) = Node
    (mconcat [parentVal,  nodeVal l, nodeVal r]) (mconcatNeighs' x l) (mconcatNeighs' x r)
    where
        nodeVal :: (Monoid a) => BinTree a -> a
        nodeVal (Node x _ _) = x
        nodeVal Empty = mempty

main :: IO ()
main = do
    print . subTree 5 . sumNeighs $ treeFromList [7 ..]
    print . subTree 5 $ treeFromList [7 ..]
