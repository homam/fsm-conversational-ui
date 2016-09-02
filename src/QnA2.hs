{-# LANGUAGE GADTs, RankNTypes, TupleSections, FlexibleInstances #-}

module QnA2

where

type Prompt = String
type Input = String

-- @i@ is the state and @a@ is the answer to this Question
data Question i a = Question {
  ask :: i -> Prompt,
  answer :: i -> Input -> a
}

-- | @Link i o@ is a linked list of Questions.
-- @i@ is the before state and @o@ is the after state
-- @i -> i'@ transforns the Link's before state @i@ to Question's @i'@
-- @i -> a -> o@ tranforms the answer to the Question to Link's after state
-- @i -> a -> Link o o'@ selects the next Link.
data Link i o  where
  Link :: (i -> a -> o) -> Question i a -> (i -> a -> Link o o') -> Link i o
  Final :: Link i o

-- | Prepend a Question to a Link
(~:>) :: Question i a -> Link (i, a) o -> Link i (i, a)
q ~:> l = q ~?> (const . const) l

-- | Prepend a Question to some Link using link selector funciton
(~?>) :: Question i a -> (i -> a -> Link (i, a) o) -> Link i (i, a)
q ~?> f = Link (,) q f

-- | Create a Question that does not depend on the QnA state
simpleQuestion :: (Prompt -> a) -> Prompt -> Question i a
simpleQuestion r msg = Question {
  ask = const msg,
  answer = const r
}

-- | Extract the leading Question's Prompt from a Link
startLink :: i -> Link i o -> Prompt
startLink i (Link c q l) = ask q i
startLink _ Final = "Done!"

-- | Receive the user's Input to the leading Question and return the after state of the QnA
-- TODO: I want to teturn the after state of the QnA and the next Link in the chain.
endLink :: i -> Link i o -> Input -> o -- TODO: return this tuple: (o, Link o o')
endLink i (Link c q l) ans =
  let a    = answer q i ans
      o    = c i a
      link = l i a -- the next link. TODO: error: variable ‘o'’ would escape its scope
  in o
  -- in link

endLink _ Final _ = error "Cannot go further than the Final Link"


processLink :: Link i o -> i -> IO o
processLink l i = do
  putStrLn $ startLink i l -- prompt the Question
  a <- getLine  -- get the answer
  return $ endLink i l a -- return the input to the next Link (**and the next Link**)


-- Some sample questions

class KnownAge i where
  age :: i -> Int

instance KnownAge (((), String), Int) where
  age (_, a) = a

instance KnownAge ((((), String), Int), String) where
  age ((_, a), _) = a

yourNameQ :: Question i String
yourNameQ = simpleQuestion id "Your name?"

yourAgeQ :: Question i Int
yourAgeQ = simpleQuestion read "Your age?"

yourDrinkQ :: KnownAge i => Question i String
yourDrinkQ = simpleQuestion id "Your drink?"

yourSchool :: KnownAge i => Question i String
yourSchool = simpleQuestion id "Your grad school?"

-- A sample QnA

nameAndDrinkL = yourNameQ ~:> ageAndDrinkL
ageAndDrinkL = yourAgeQ ~?> const (\ i -> if i > 21 then drinkL else if i > 19 then gradL else Final)
drinkL = yourDrinkQ ~:> gradL

gradL :: KnownAge i => Link i (i, String)
gradL = yourSchool ~:> Final


-- | TODO: Linking @processLink@ s with @>>=@ is unnecessary. Because QnA flow is already defined.
main =
      processLink nameAndDrinkL ()
  >>= processLink ageAndDrinkL
  >>= processLink drinkL
  >>= print
