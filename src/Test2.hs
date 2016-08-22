{-# LANGUAGE TupleSections, RankNTypes, GADTs #-}

module Test2
where

import Control.Monad.State
type UserInput = String
data NInput = InInt Int | InString String  | InAny () | IntArith (Int, Int, String)
data NOutput = OutInt Int | OutString String
data CState = StInts [Int] | StAny ()
data Node = QNode {
  question :: String,
  exec :: NInput -> UserInput -> NInput
} | ANode {
  auto :: NInput -> (NInput, String)
}

data Edge = Edge {
  node :: Node,
  link :: NInput -> CState -> (Edge, NInput, CState)
} | StartingEdge {
  choose :: (CState, Edge)
} | EndingEdge {
  node :: Node,
  result :: NInput -> CState -> NInput
}

getANumber = QNode {
  question = "Enter a number",
  exec = const $ InInt . read
}

getAnOperation = QNode {
  question = "Enter an operatiorn + / *",
  exec = const InString
}

doOperation = ANode {
  auto = \ (IntArith (a, b, o)) -> let res = (if o == "+" then (+) else (*)) a b in (InInt res, "Your result is " ++ show res)
}

startEdge = StartingEdge {
  choose = (StInts [], getTwoNumbersEdge)
}

getTwoNumbersEdge = Edge {
  node = getANumber,
  link = \ (InInt i) (StInts s) -> (if length s < 2 then getTwoNumbersEdge else getAnOperationLink, InAny (), StInts (i:s))
}

getAnOperationLink = Edge {
  node = getAnOperation,
  link = \ (InString s) (StInts is) -> (doOperationLink, IntArith (head is, last is, s), StAny ())
}

doOperationLink = EndingEdge {
  node = doOperation,
  result = const
}


-- getANumber :: State () Int
-- getANumber = return 3
