module Main where

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

-- JSONNull :: Parser JSONValue
-- JSONNull = undefined

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
-- parseString :: String -> Parser String
-- parseString input = sequenceA . map parseChar

-- JSONValue :: Parser JSONValue
-- JSONValue = undefined

main :: IO ()
main = undefined