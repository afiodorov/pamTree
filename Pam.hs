import BinTree (BinTree (Node, Empty), treeFromList, subTree)
import Data.Monoid
import Control.Applicative

-- replace each Node with a sum of its neighboors
sumNeighs ::  (Num a) => BinTree a -> BinTree a
sumNeighs = (getSum <$>) . mconcatNeighs . (Sum <$>)

-- replace each Node with a product of its neighboors
productNeighs ::  (Num a) => BinTree a -> BinTree a
productNeighs = (getProduct <$>) . mconcatNeighs . (Product <$>)

mconcatNeighs :: (Monoid a) => BinTree a -> BinTree a
mconcatNeighs = mconcatNeighs' mempty

mconcatNeighs' :: (Monoid a) => a -> BinTree a -> BinTree a
mconcatNeighs' parentVal Empty = Empty
mconcatNeighs' parentVal (Node x l r) = Node
    (mconcat [parentVal,  nodeVal l, nodeVal r]) (mconcatNeighs' x l) (mconcatNeighs' x r)
    where
        nodeVal :: (Monoid a) => BinTree a -> a
        nodeVal (Node x _ _) = x
        nodeVal Empty = mempty

runTimeStep :: (Num a) => BinTree a -> BinTree a -> BinTree a
runTimeStep potential t = (+) <$> potTimest <*> sumNeighs t
    where potTimest = (*) <$> potential <*> t

potential = treeFromList [2 ..]
initial = treeFromList $ 1 : repeat 0

runModel :: (Num a) => Int -> BinTree a -> BinTree a -> BinTree a
runModel times potential  = foldr (.) id (replicate times (runTimeStep potential))

main :: IO ()
main = print . subTree 7 $ runModel 7 potential initial
