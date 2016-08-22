{-# LANGUAGE AllowAmbiguousTypes, GADTs, RankNTypes #-}
module Test
where

data Func where
  Func :: (A -> B) -> Func

data A = AInt Int | AString String deriving (Show, Read)
data B = BInt Int | BString String deriving (Show, Read)

f1 :: Func
f1 = Func $ \ (AInt i) -> BInt $ i * 2

f2 :: Func
f2 = Func $ \ (AString i) -> BString $ i ++ "!"

getF :: String -> Func
getF "f1" = f1
getF "f2" = f2

exec :: Func -> A -> B
exec (Func f) = f

main1 = print $ exec (getF "f1") (read "AInt 4")
