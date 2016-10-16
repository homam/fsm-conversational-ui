{-# LANGUAGE FlexibleInstances #-}

module V2ParserUtil (parseSuspended, parseStage, ReadParsec(..))
where

import Text.ParserCombinators.Parsec (string, char, try, many, anyToken, (<|>), getInput, parse)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)

-- | By convenction we expect every Suspended types to be shown as: @Suspended Stage Value@.
parseSuspended :: [ParsecT String u Identity t] -> ParsecT String u Identity t
parseSuspended = parsecSuspended . toTryParsec where

  parsecSuspended :: ParsecT String u Identity t -> ParsecT String u Identity t
  parsecSuspended p = string "Suspended" >> char ' ' >> p

  toTryParsec :: [ParsecT String u Identity t] -> ParsecT String u Identity t
  toTryParsec (h:rest) = foldl1 (<|>) (map try rest) <|> h

-- | Parses @Suspended Stage as@ GADT, for example:
-- @
-- parse (parseStage "AskSize" $ Suspended AskSize) "" "AskSize (False, ())"
-- @
parseStage :: Read b => String -> (b -> t) -> ParsecT String u Identity t
parseStage name stager = do
  string name
  char ' '
  t <- many anyToken
  return $ stager $ read t

-- | Create 'Read' instances of @Suspended@ types by @readsPrec = readsPrecRP@
class ReadParsec t where
  readParsec :: ParsecT String u Identity t

  readsPrecRP :: ReadParsec t => a -> String -> [(t, String)]
  readsPrecRP _ = either (const []) id . parse (readParsec' readParsec) ""

readParsec' :: ParsecT String u Identity t -> ParsecT String u Identity [(t, String)]
readParsec' readParsec = do
  a <- readParsec
  rest <- getInput
  return [(a, rest)]
