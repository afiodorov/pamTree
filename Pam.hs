import BinTree (BinTree (Node, Empty), treeFromList, subTree)
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import Data.Random.RVar
import Data.Random.Distribution (Distribution)
import Data.Random.Distribution.Uniform (StdUniform)
import Data.Random.Distribution.Exponential (exponential, Exponential)
import Data.Random.Sample (sample)
import Data.Random.Source.IO
import Data.Traversable (sequence)
import Prelude hiding (sequence)

-- replace each Node with a sum of its neighboors
sumNeighs ::  (Num a) => BinTree a -> BinTree a
sumNeighs = (getSum <$>) . mconcatNeighs . (Sum <$>)

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
runTimeStep potential t = (+) <$> rescaledByPotential <*> sumNeighs t
    where rescaledByPotential = (*) <$> potential <*> t

initial = treeFromList $ 1 : repeat 0

runModel :: (Num a) => Int -> BinTree a -> BinTree a -> BinTree a
runModel times potential  = foldr (.) id (replicate times (runTimeStep potential))

weibull :: Double -> RVar Double
weibull k = (**(1/k)) <$> exponential 1

distribution = weibull 3
treeSize = 4

main :: IO ()
main = do
    potential <- sequence . subTree treeSize
        . treeFromList . repeat $ (sample distribution :: IO Double)
    print . subTree treeSize $ potential
    print . subTree treeSize $ runModel 15 potential initial
