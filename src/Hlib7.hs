module HLib7
where

type UserInput = String
data LState = LState
data Target =
    Cont Node
  | Rec Node
  | EOC


data Node = Node {
  exec :: UserInput -> LState -> LState
}
