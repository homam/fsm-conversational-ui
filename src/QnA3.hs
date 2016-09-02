{-# LANGUAGE GADTs, RankNTypes, TupleSections, FlexibleInstances, NamedFieldPuns #-}

module QnA1

where

instance Functor (Question i) where
  fmap f Question{ ask, answer } = Question {ask = ask, answer = \ i -> f . answer i}
  fmap f (Always g) = Always $ f . g

-- useless?
instance Applicative (Question i) where
  pure = Always . const
  (Always f) <*> (Always g) = Always $ \ i -> f i (g i)
  (Always f) <*> Question{ ask, answer } = Question { ask = ask, answer = \ i -> f i . answer i }
  Question{ ask, answer } <*> (Always f) = Question { ask = ask, answer = \ i s -> answer i s (f i) }
  Question{ ask=ask', answer=answer' } <*> Question{ ask, answer } =
    Question { ask = \ i -> ask i ++ "\n" ++ ask' i, answer = \ i s -> answer' i s (answer i s) }

type Prompt = String
type Input = String




-- @i@ is the state and @a@ is the answer to this Question
data Question i a = Question {
  ask :: i -> Prompt,
  answer :: i -> Input -> a
} | Always (i -> a)

-- | @Link i o@ is a linked list of Questions.
-- @i@ is the before state and @o@ is the after state
-- @i -> i'@ map QnA state @i@ to Question's input @i'@
-- @i -> a -> o@ tranforms the answer to the Question to Link's after state
-- @i -> a -> Link o o'@ selects the next Link.
data Link where
  Link :: (s -> i) -> (s -> a -> o) -> Question i a -> (s -> a -> Link ) -> Link
  Final :: Link

-- | Prepend a Question to a Link
(~:>) :: Question i a -> Link  -> Link
q ~:> l = q ~?> (const . const) l

-- | Prepend a Question to some Link using link selector funciton
(~?>) :: Question i a -> (i -> a -> Link ) -> Link
q ~?> f = Link id (,) q f


-- | Create a Question that does not depend on the QnA state
simpleQuestion :: (Prompt -> a) -> Prompt -> Question i a
simpleQuestion r msg = Question {
  ask = const msg,
  answer = const r
}

-- | Extract the leading Question's Prompt from a Link
startLink :: i -> Link -> Prompt
startLink i (Link f c q@Question{} _) = ask q (f i)
startLink i (Link f c (Always next) l) =
  let a = next (f i)
  in startLink (c i a) (l i a)
startLink _ Final = "Done!"

-- | Receive the user's Input to the leading Question and return the after state of the QnA
-- TODO: I want to teturn the after state of the QnA and the next Link in the chain.
-- endLink :: i -> Link i o -> Input -> o -- TODO: return this tuple: (o, Link o o')
endLink i (Link f c q l) ans =
  let a    = answer q (f i) ans
      o    = c i a
      link = l i a -- the next link. TODO: error: variable ‘o'’ would escape its scope
  in undefined
  -- in link

endLink _ Final _ = error "Cannot go further than the Final Link"


processLink :: Link -> i -> IO o
processLink l i = do
  putStrLn $ startLink i l -- prompt the Question
  a <- getLine  -- get the answer
  return $ endLink i l a -- return the input to the next Link (**and the next Link**)


-- Some sample questions

class KnownAge i where
  age :: i -> Int

instance KnownAge (((a, b), Int), c) where
  age ((_, a), _) = a

yourNameQ :: Question i String
yourNameQ = simpleQuestion id "Your name?"

yourAgeQ :: Question i Int
yourAgeQ = simpleQuestion read "Your age?"

yourDrinkQ :: Question i Prompt
yourDrinkQ = simpleQuestion id "Your drink?"

yourMinor :: Question i Int
yourMinor = simpleQuestion read "Grad year?"

gradAge :: KnownAge i => Question i Int
gradAge = Question {
  ask = const "Graduation year?",
  answer = \ i a -> 2016 - read a + age i
}

-- A sample QnA

nameAndDrinkL = yourNameQ ~:> ageAndDrinkL
ageAndDrinkL = yourAgeQ ~?> const (\ i -> if i > 21 then drinkL else (Always $ const Nothing) ~:> gradL)
drinkL = fmap Just yourDrinkQ ~:> gradL
gradL = gradAge ~:> Final
