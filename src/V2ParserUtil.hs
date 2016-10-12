{-# LANGUAGE FlexibleInstances #-}

module V2ParserUtil (parseSuspended, parseStage, ReadParsec(..))
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)

import Debug.Trace (trace)

parseSuspended :: [ParsecT String u Identity t] -> ParsecT String u Identity t
parseSuspended = parsecSuspended . toTryParsec

parsecSuspended :: ParsecT String u Identity t -> ParsecT String u Identity t
parsecSuspended p = string "Suspended" >> char ' ' >> p

parseStage :: Read b => String -> (b -> t) -> ParsecT String u Identity t
parseStage name stager = do
  string name
  char ' '
  t <- many anyToken
  return $ stager $ read t

toTryParsec :: [ParsecT String u Identity t] -> ParsecT String u Identity t
toTryParsec (h:rest) = foldl1 (<|>) (map try rest) <|> h
-- toTryParsec = foldl1 (<|>) . map try

class ReadParsec t where
  parsecRead :: ParsecT String u Identity t

  readsPrec1 :: ReadParsec t => a -> String -> [(t, String)]
  readsPrec1 _ = either (const []) id . parse (parsecRead' parsecRead) ""

-- instance (ReadParsec t) => Read t where
--   readsPrec _ = either (const []) id . parse (parsecRead' parsecRead) ""

parsecRead' :: ParsecT String u Identity t -> ParsecT String u Identity [(t, String)]
parsecRead' parsecRead = do
  a <- parsecRead
  rest <- getInput
  return [(a, rest)]
