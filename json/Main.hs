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

main :: IO ()
main = undefined