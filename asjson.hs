import Data.Array
import Data.List (intercalate)

--Implementing the JSON abstract grammar as a Haskell data type
data JSONValue = JSONObject [(String, JSONValue)] 
               | JSONText String 
               | JSONBoolean Bool 
               | JSONNumber Sign Integer Frac Exp 
               | JSONArray [JSONValue] 
               | JSONNull  
               deriving Show

--Types for JSON Number specification
type Frac = Float
type Sign = String
type Exp = (Sign, Integer)

pp :: JSONValue -> String
pp x = ppi 2  x

ppi :: Int -> JSONValue -> String
ppi _ JSONNull = "null"
ppi _ (JSONText text) = "\"" ++ text ++ "\""
ppi _ (JSONNumber sign int frac exp) = if snd(exp) == 0 then show 1 
                                          else if frac /= 0 then 
                                                  plusMinus sign ++ show int ++ tail(show frac) ++ "E" ++ plusMinus (fst(exp)) ++ show (snd(exp))
                                          else sign ++ show int ++ "e" ++ plusMinus (fst(exp)) ++ show (snd(exp))
ppi _ (JSONBoolean bool) = if bool then "true" else "false"
ppi _ (JSONArray []) = "[]"
ppi i (JSONArray (arr)) = "[\n" ++ indent i
                                ++ intercalate (",\n" ++ indent i) (map (ppi i) arr)  ++ "\n" ++ indent (i-2) ++ "]"

ppi _ (JSONObject []) = "{}"
ppi i (JSONObject objects) = "{\n"
                            ++ indent i 
                            ++ intercalate (",\n" ++ indent i) 
                                           (map(\(key, value) -> quote key ++ ":" ++ ppi (i+2) value) 
                                           (objects) ) 
                            ++ "\n" ++ indent (i-2) ++ "}"

--Function for identation
indent :: Int -> String
indent n = replicate n ' ' 

--Function for quotation
quote :: String -> String
quote s = "\"" ++  s ++ "\""

--Function for determining minus/plus sign
plusMinus :: String -> String
plusMinus sign = if sign == "-" then "-" else ""

--Running the program for a JSONObject
main = do
      let json = JSONObject [ 
                ("ab", (JSONObject [("json", (JSONBoolean True)), ("xml", (JSONBoolean False))])),
                ("b", JSONNull),
                ("cdef", JSONArray [
                  JSONNumber "-" 3 0.3 ("-", 10),
                  JSONNull,
                  JSONBoolean False,
                  JSONText "STRING"
                ])
                 ]
        in putStrLn (pp json)

-- Sample output
{- 
  "ab":{
    "json":true,
    "XML":false
  },
  "b":null,
  "cdef":[
    -3.3E-10,
    null,
    false,
    "STRING"
  ]
} 
-}