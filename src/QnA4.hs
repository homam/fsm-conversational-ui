{-# LANGUAGE GADTs, RankNTypes, TupleSections #-}

module QnA4

where

data Q1i
data Q1o
data Q2i
data Q2o
data Q3i
data Q3o
data Q4i
data Q4o

data Graph s a s' where
  Graph :: (s -> a) -> (s -> a -> s') -> Graph s a s'

runGraph :: Graph s a s' -> s -> (a, s')
runGraph (Graph f q) s = (f s, q s (f s))

(~:>) :: Graph s a s' -> Graph s' a' s'' -> Graph s a' s''
Graph f1 q1 ~:> Graph f2 q2 = Graph (\s -> f2 $ q1 s (f1 s)) (\s a' -> q2 (q1 s (f1 s)) a')

n1 :: Graph () Int String
n1 = Graph (const 42) (const . show)

n2 :: Graph String Bool Int
n2 = Graph ("42"==) (\s b -> if b then length s else -1 * length s)

n3 :: Graph () Bool Int
n3 = n1 ~:> n2

n4 :: Graph () Bool Int
n4 = let (a, s) = runGraph n1 () in undefined


type Input = String
type Prompt = String
type Color = String

data Question a = Question Prompt (Input -> a)
data Link a s s' = Link (Question a) (s -> a -> s')

data Edge sf where
  Edge  :: Link a s s' -> (s' -> a -> Edge sf) -> Edge sf
  Final :: Link a s s' -> (s' -> a -> sf) -> Edge sf

doYouKnowYourSizeQ :: Question Bool
doYouKnowYourSizeQ = Question "Do you know your size?" read

whatIsYourSizeQ :: Question Int
whatIsYourSizeQ = Question "What is your size?" read

whatIsYourWeightQ :: Question Int
whatIsYourWeightQ = Question "What is your weight?" read

whatIsYourHeightQ :: Question Int
whatIsYourHeightQ = Question "What is your height?" read

whatIsYourFavColorQ :: Question Color
whatIsYourFavColorQ = Question "What is your fav color?" id

l5 :: Link Color (Bool, Int) (Bool, Int, Color)
l5 = Link whatIsYourFavColorQ (\(b, i) c -> (b, i, c))

l1 :: Link Bool () Bool
l1 = Link doYouKnowYourSizeQ (const id)

l2 :: Link Int Bool (Bool, Int)
l2 = Link whatIsYourSizeQ (\ b s -> (b, s))

l3 :: Link Int Bool (Bool, Int)
l3 = Link whatIsYourWeightQ (,)

l4 :: Link Int (Bool, Int) (Bool, Int)
l4 = Link whatIsYourHeightQ (\ (b, w) h -> (b, w * h))


e1 = Edge l1 (const $ \ b -> if b then e2 else e3)
e2 = Edge l2 (const $ const ef)
e3 = Edge l3 (const $ const e4)
e4 = Edge l4 (const $ const ef)
ef = Final l5 const
