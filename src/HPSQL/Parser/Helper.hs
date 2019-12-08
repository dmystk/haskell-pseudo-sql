module HPSQL.Parser.Helper where

import Data.List
import Data.Char

import Util.Parser


-- Make a keyword parser from a string.
-- The parser does not distinguish upper or lower case by SQL spec.
-- |
-- >>> pKeyword "keyword" ["KeYwOrD", "foo", "bar"]
-- [("KeYwOrD",["foo","bar"])]
-- >>> pKeyword "keyword" ["foo"]
-- []
pKeyword :: String -> Parser String
pKeyword s =
    let up = map toUpper
    in  pSatisfy (\t -> up s == up t)


-- Make a parser which is constructed of two keyword.
-- |
-- >>> pIdiom "foo" "bar" ["Foo", "Bar", "Baz"]
-- [("FooBar",["Baz"])]
-- >>> pIdiom "foo" "bar" ["fooo", "bar", "baz"]
-- []
pIdiom :: String -> String -> Parser String
pIdiom s1 s2 = pThen (++) (pKeyword s1) (pKeyword s2)


-- By SQL specification, the head of a variable must be an alphabet
-- and the others must be either alphabet, digit, or underscore.
-- |
-- >>> pVariable ["Alpha_1", "foo", "bar"]
-- [("Alpha_1",["foo","bar"])]
-- >>> pVariable ["Alpha-1"]
-- []
-- >>> pVariable ["1Alpha"]
-- []
-- >>> pVariable ["_Alpha"]
-- []
pVariable :: Parser String
pVariable = pSatisfy isVariable
    where
        isVariable [] = False
        isVariable (c:cs) =
            let isAlphaNumUnder c = isAlphaNum c || c == '_'
            in  isAlpha c && and (map isAlphaNumUnder cs)


-- Support only colon ('), not double colon (").
-- |
-- >>> pString ["'", "foo", "'"]
-- [("foo",[])]
-- >>> pString ["'", ":;+-*/[]`<>", "'", "foo"]
-- [(":;+-*/[]`<>",["foo"])]
-- >>> pString ["'", "foo", "bar"]
-- []
pString :: Parser String
pString = pBetween "'" "'" $ pSatisfy (\_ -> True)


-- Parser for an integer.
-- |
-- >>> pInteger ["12345", "foo"]
-- [(12345,["foo"])]
-- >>> pInteger ["01234", "foo"]
-- []
-- >>> pInteger ["1.234", "foo"]
-- []
pInteger :: Parser Integer
pInteger = pMap toInteger $ pSatisfy isInteger
    where
        toInteger n = read n :: Integer
        isInteger [] = False
        isInteger "0" = True
        isInteger (c:cs) = (isNumber c && not (c == '0')) && and (map isNumber cs)


-- |
-- >>> pComma [",", "foo"]
-- [(",",["foo"])]
pComma :: Parser String
pComma = pLiteral ","


-- |
-- >>> pNull ["null"]
-- [((),[])]
-- >>> pNull ["NULL", "foo"]
-- [((),["foo"])]
-- >>> pNull ["nil"]
-- []
pNull :: Parser ()
pNull = pConstant () (pKeyword "null")


-- |
-- >>> pWithParentheses (pLiteral "foo") ["(", "foo", ")"]
-- [("foo",[])]
-- >>> pWithParentheses (pLiteral "foo") ["(", "fooo", ")", "bar"]
-- []
pWithParentheses :: Parser a -> Parser a
pWithParentheses p = pBetween "(" ")" p

