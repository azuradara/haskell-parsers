module Main where

import Control.Applicative (Alternative, empty, (<|>))

-- AST to handle possible JSON values
data JSONValue
  = JSONNull
  | JSONBool Bool
  | JSONNumber Integer -- TODO: Cover floats aswell
  | JSONString String
  | JSONArray [JSONValue]
  | JSONObject [(String, JSONValue)] -- O(n) lookup with no deps
  deriving (Show, Eq)

{-
    Parser function generator

    - Parses a value from a string input
    - Returns the value and the remainder of the JSON for chaining
    - Sometimes it doesn't, in which case it returns nothing (for now)

    TODO: implement real error reporting
-}

-- parse :: String -> Either (Int, Int, String) (String, a)
newtype Parser val = Parser
  { parse :: String -> Maybe (String, val)
  }

-- Parser Functor proof
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

-- Parser Applicative proof
instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser parser1) <*> (Parser parser2) = Parser $ \input -> do
    (input', f) <- parser1 input
    (input'', a) <- parser2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser parser1) <|> (Parser parser2) =
    Parser $ \input -> parser1 input <|> parser2 input

jsonNull :: Parser JSONValue
jsonNull = JSONNull <$ parseString "null"

-- Char Parser
parseChar :: Char -> Parser Char
parseChar c =
  Parser f
  where
    f (x : xs)
      | x == c = Just (xs, c)
      | otherwise = Nothing
    f [] = Nothing

-- String Parser
-- [Parser Char] -> Parser [Char]
parseString :: String -> Parser String
parseString = traverse parseChar

jsonBool :: Parser JSONValue
jsonBool = f <$> (parseString "true" <|> parseString "false")
  where
    f "true" = JSONBool True
    f "false" = JSONBool False
    f _ = undefined

-- Ultimate parser
jsonValue :: Parser JSONValue
jsonValue = jsonNull <|> jsonBool

main :: IO ()
main = undefined