
module Util.Parser where


import Data.List


type Token = String


type Parser a = [Token] -> [(a, [Token])]


-- Take two parsers and return a parser which return the result of both parsers.
-- This function equals (|) of BNF.
(|.) :: Parser a -> Parser a -> Parser a
(|.) p1 p2 ts = (p1 ts) ++ (p2 ts)


-- Map a parser to another by specified function.
-- |
-- >>> pMap (\_ -> "foo") (pLiteral "bar") ["bar"]
-- [("foo",[])]
-- >>> pMap (\_ -> "foo") (pLiteral "bar") ["bar", "baz"]
-- [("foo",["baz"])]
pMap :: (a -> b) -> Parser a -> Parser b
pMap f p ts = map (\(x, os) -> (f x, os)) (p ts)


-- Make a parser which applies two parsers sequentially.
-- And those parsed results are combined by the argument funciton.
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 ts =
    [ (combine v1 v2, ts2)
        | (v1, ts1) <- p1 ts
        , (v2, ts2) <- p2 ts1 ]


-- Make a parser which applies three parsers sequentially.
-- And those parsed results are combined by the argument funciton.
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 ts =
    [ (combine v1 v2 v3, ts3)
        | (v1, ts1) <- p1 ts
        , (v2, ts2) <- p2 ts1
        , (v3, ts3) <- p3 ts2 ]


-- Make a parser which applies four parsers sequentially.
-- And those parsed results are combined by the argument funciton.
pThen4 :: (a -> b -> c -> d -> e)
            -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 ts =
    [ (combine v1 v2 v3 v4, ts4)
        | (v1, ts1) <- p1 ts
        , (v2, ts2) <- p2 ts1
        , (v3, ts3) <- p3 ts2
        , (v4, ts4) <- p4 ts3 ]


-- Make a parser which applies five parsers sequentially.
-- And those parsed results are combined by the argument funciton.
pThen5 :: (a -> b -> c -> d -> e -> f)
            -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
pThen5 combine p1 p2 p3 p4 p5 ts =
    [ (combine v1 v2 v3 v4 v5, ts5)
        | (v1, ts1) <- p1 ts
        , (v2, ts2) <- p2 ts1
        , (v3, ts3) <- p3 ts2
        , (v4, ts4) <- p4 ts3
        , (v5, ts5) <- p5 ts4 ]


-- Make a parser which read nothing and return the argument as parsed result.
-- |
-- >>> pEmpty "foo" ["bar"]
-- [("foo",["bar"])]
pEmpty :: a -> Parser a
pEmpty x ts = [(x, ts)]


-- Make a parser which returns a specified value as parsed result.
-- |
-- >>> pConstant () (pLiteral "foo") ["foo"]
-- [((),[])]
-- >>> pConstant 1 (pLiteral "foo") ["foo", "bar"]
-- [(1,["bar"])]
pConstant :: a -> Parser b -> Parser a
pConstant x p = pMap (\_ -> x) p


-- Make a parser from a predicate.
-- The parser returns the read string itself as parsed result.
-- |
-- >>> pSatisfy (== "foo") ["foo", "bar"]
-- [("foo",["bar"])]
-- >>> pSatisfy (== "foo") ["bar", "baz"]
-- []
pSatisfy :: (String -> Bool) -> Parser String
pSatisfy _ [] = []
pSatisfy f (t:ts)
    | f t = [(t, ts)]
    | otherwise = []


-- Make a parser which accept the argument string itself.
-- |
-- >>> pLiteral "foo" ["foo", "bar"]
-- [("foo",["bar"])]
-- >>> pLiteral "foo" ["bar", "baz"]
-- []
pLiteral :: String -> Parser String
pLiteral s = pSatisfy (== s)


-- Make a parser which allows omission.
-- |
-- >>> pMaybe (pLiteral "foo") ["foo", "bar"]
-- [(Nothing,["foo","bar"]),(Just "foo",["bar"])]
-- >>> pMaybe (pLiteral "foo") ["bar", "baz"]
-- [(Nothing,["bar","baz"])]
pMaybe :: Parser a -> Parser (Maybe a)
pMaybe p =
    (pEmpty Nothing) |. pMap Just p


-- Make a parser which detects something sandwiched between any words.
-- |
-- >>> pBetween "(" ")" (pLiteral "foo") ["(", "foo", ")"]
-- [("foo",[])]
-- >>> pBetween "(" ")" (pLiteral "foo") ["(", "fooo", ")"]
-- []
pBetween :: String -> String -> Parser a -> Parser a
pBetween s e p =
    pThen3 (\_ x _ -> x) (pLiteral s) p (pLiteral e)


-- Take a parser and return new one which repeat applying it by zero or more times.
-- The parsed result is a list which contains read strings.
-- |
-- >>> pZeroOrMore (pLiteral "foo") ["foo", "bar", "foo"]
-- [([],["foo","bar","foo"]),(["foo"],["bar","foo"])]
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p =
    (pEmpty []) |. pOneOrMore p


-- Take a parser and return new one which repeat applying it by one or more times.
-- The parsed result is a list which contains read strings.
-- |
-- >>> pOneOrMore (pLiteral "foo") ["foo", "foo", "bar", "foo"]
-- [(["foo"],["foo","bar","foo"]),(["foo","foo"],["bar","foo"])]
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p =
    pThen (:) p (pZeroOrMore p)
