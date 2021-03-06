import BinTree (BinTree (Node, Empty), treeFromList, subTree)
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class()
import Data.Random.RVar
import Data.Random.Distribution.Exponential (exponential)
import Data.Random.Sample (sample)
import Data.Random.Source.IO()
import Data.Traversable (sequence)
import Prelude hiding (sequence)

-- replace each Node with a sum of its neighboors
sumNeighs ::  (Num a) => BinTree a -> BinTree a
sumNeighs = (getSum <$>) . mconcatNeighs . (Sum <$>)

mconcatNeighs :: (Monoid a) => BinTree a -> BinTree a
mconcatNeighs = mconcatNeighs' mempty

mconcatNeighs' :: (Monoid a) => a -> BinTree a -> BinTree a
mconcatNeighs' _ Empty = Empty
mconcatNeighs' parentVal (Node x l r) = Node
    (mconcat [parentVal,  nodeVal l, nodeVal r]) (mconcatNeighs' x l) (mconcatNeighs' x r)
    where
        nodeVal :: (Monoid a) => BinTree a -> a
        nodeVal (Node y _ _) = y
        nodeVal Empty = mempty

runTimeStep :: (Fractional a) => BinTree a -> BinTree a -> BinTree a
runTimeStep potential t = (+) <$> delta <*> t
    where
        delta = (*timeStep) <$> ((+) <$> rescaledByPotential <*> sumNeighs t)
            where rescaledByPotential = (*) <$> potential <*> t
        timeStep = 0.1

runModel :: (Fractional a) => Int -> BinTree a -> BinTree a -> BinTree a
runModel times potential  = foldr (.) id (replicate times (runTimeStep potential))

weibull :: Double -> RVar Double
weibull k = (**(1/k)) <$> exponential 1

initial :: BinTree Double
initial = treeFromList $ 1 : repeat 0

distribution :: RVar Double
distribution = weibull 3
treeSize :: Int
treeSize = 7
modelRunNum :: Int
modelRunNum = 30

main :: IO ()
main = do
    potentialTree <- sequence . subTree treeSize
        . treeFromList . repeat $ (sample distribution :: IO Double)
    print . subTree treeSize $ potentialTree
    print . subTree treeSize $ runModel modelRunNum potentialTree initial
