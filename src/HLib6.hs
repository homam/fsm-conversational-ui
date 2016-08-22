{-# LANGUAGE RankNTypes, GADTs #-}

module HLib6
where
import Control.Category ((>>>))
import Control.Monad ((>=>))

data Node i o = ANode {
  execANode :: i -> Target o
}

data Target o =
    forall o'. Continuation (Node o o')
  | forall o'. Recursion (Node o o')
  | EOD o
  | forall o'. Start (o' -> o) (Node () o')
  | forall o' o''. Fork o' (Node o' o'') (Node o'' o)

askForANumber :: Node () Int
askForANumber = ANode {
  execANode = const (EOD 3)
}

getTwoNumbers :: Node [Int] [Int]
getTwoNumbers = ANode {
  -- execANode = \ s -> if length s < 2 then (3:s, Recursion getTwoNumbers) else (s, EOD)
  execANode = \ s -> if length s < 2 then Start (:s) askForANumber else EOD s
}

sumTwoNumbers :: Node [Int] Int
sumTwoNumbers = ANode {
  execANode = EOD . sum
}

someFunc :: Node () Int
someFunc = ANode {
  execANode = const $ Fork [] getTwoNumbers sumTwoNumbers
}


getOperation :: Node () String
getOperation = ANode {
  -- what do you want to do with these numbers?
  execANode = const $ EOD "+"
}

getTwoNumbersAndOperation :: Node () ([Int], String)
getTwoNumbersAndOperation = ANode {
  -- give us two numbers and an operation
  execANode = const $ Fork [] getTwoNumbers makeOperation
}

makeOperation :: Node [Int] ([Int], String)
makeOperation = ANode {
  -- mempty
  execANode = \ s -> Start (\ b -> (s, b)) getOperation
}

applyOperation :: Node ([Int], String) Int
applyOperation = ANode {
  -- and the result is
  execANode = \ (num, op) -> EOD $ sum num
}

someOperation :: Node () Int
someOperation = ANode {
  execANode = const $ Fork () getTwoNumbersAndOperation applyOperation
}
