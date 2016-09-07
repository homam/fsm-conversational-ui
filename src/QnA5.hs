{-# LANGUAGE GADTs, RankNTypes, TupleSections #-}

module QnA5
where


type Input = String
type Prompt = String
type Color = String
type Size = Int
type Weight = Int
type Height = Int

data Question a = Question {
  prompt :: Prompt,
  answer :: Input -> a
}

doYouKnowYourSizeQ :: Question Bool
doYouKnowYourSizeQ = Question "Do you know your size?" read

whatIsYourSizeQ :: Question Size
whatIsYourSizeQ = Question "What is your size?" read

whatIsYourWeightQ :: Question Weight
whatIsYourWeightQ = Question "What is your weight?" read

whatIsYourHeightQ :: Question Height
whatIsYourHeightQ = Question "What is your height?" read

whatIsYourFavColorQ :: Question Color
whatIsYourFavColorQ = Question "What is your fav color?" id

data EdgeId = E1 | E2 | E3 | E4 | Ef deriving (Read, Show)

data Node s a s' = Node {
  question :: Question a,
  process :: s -> a -> s'
}

data Edge s sf where
  Edge  :: EdgeId -> Node s a s' -> (s' -> a -> Edge s' sf) -> Edge s sf
  Final :: EdgeId -> Node s a s' -> (s' -> a -> sf) -> Edge s sf


n1 :: Node () Bool Bool
n1 = Node doYouKnowYourSizeQ (const id)

n2 :: Node Bool Size (Bool, Size)
n2 = Node whatIsYourSizeQ (,)

n3 :: Node Bool Weight (Bool, Weight)
n3 = Node whatIsYourWeightQ (,)

n4 :: Node (Bool, Weight) Height (Bool, Size)
n4 = Node whatIsYourHeightQ (\ (b, w) h -> (b, w * h))

n5 :: Node (Bool, Size) Color (Bool, Size, Color)
n5 = Node whatIsYourFavColorQ (\ (b, i) c -> (b, i, c))

-- These Edges are type checked
e1 = Edge E1 n1 (const $ \ b -> if b then e2 else e3)
e2 = Edge E2 n2 (const $ const ef)
e3 = Edge E3 n3 (const $ const e4)
e4 = Edge E4 n4 (const $ const ef)
ef = Final Ef n5 const

ask :: Edge s sf -> Prompt
ask (Edge _ n _) = prompt $ question n
ask (Final _ n _) = prompt $ question n

respond :: s -> Input -> Edge s sf -> Either sf (s', Edge s' sf)
respond s i (Edge _ n f) =
  let a  = (answer $ question n) i
      s' = process n s a
      n' = f s' a
  in Right undefined --TODO n'

respond s i (Final _ n f) =
  let a  = (answer $ question n) i
      s' = process n s a
  in Left undefined --TODO s'



-- User Interaction:

saveState :: (Show s) => (s, Edge s sf) -> String
saveState (s, Edge eid n _) = show (s, eid)

getEdge :: EdgeId -> Edge s sf
getEdge = undefined --TODO

main' :: String -> Input -> Either sf (s', Edge s' sf)
main' state input =
  let (s, eid) = read state :: ((), EdgeId) --TODO
      edge = getEdge eid
  in respond s input edge
