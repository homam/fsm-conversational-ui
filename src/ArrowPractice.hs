{-# LANGUAGE Arrows #-}

module ArrowPractice
    (
    duck
    )
where

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import qualified Data.List as L
import Data.Maybe
import System.Random
import Debug.Trace (trace)
--

newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

instance Cat.Category Circuit where
  id = Circuit $ \ a -> (Cat.id, a)
  (Circuit cir2) . (Circuit cir1) = Circuit $ \ a ->
    let (cir1', b) = cir1 a
        (cir2', c) = cir2 b
    in  (cir2' Cat.. cir1', c)

instance Arrow Circuit where
  arr f = Circuit $ \ a -> (arr f, f a)
  first (Circuit cir) = Circuit $ \ (b, d) ->
     let (cir', c) = cir b
     in (first cir', (c, d))


-- L.mapAccumL unCircuit cir :: Circuit a c -> t a -> (Circuit a c, t c)
-- runCircuit :: Circuit a b -> [a] -> [b]
runCircuit :: Traversable t => Circuit a b -> t a -> t b
runCircuit cir = snd . L.mapAccumL unCircuit cir
    -- let (cir', x') = cir x
    -- in  x' : runCircuit cir' xs

-- | Accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \ a ->
   let (b, acc') = f a acc
   in  (accum acc' f, b)

-- | Accumulator that outputs the accumulator value.
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\ a acc' -> let acc'' = f a acc' in (acc'', acc''))

total :: Num a => Circuit a a
total = accum' 0 (+)

mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
  t <- total -< value
  n <- total -< 1
  returnA -< t / n

duck :: IO ()
duck = undefined
