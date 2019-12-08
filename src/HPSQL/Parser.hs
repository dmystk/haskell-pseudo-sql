
module HPSQL.Parser
    ( Command (..)
    , parse
    ) where


import Prelude hiding (lex)
import Data.List
import Data.Char

import Util.Parser
import HPSQL.Parser.Helper


data Command
    = Nop
    | CreateDatabase String
    | CreateTable String [Column]
    | DropTable String
    | AlterTableAddColumn String [Column]
    | AlterTableDropColumn String [String]
    deriving (Show)


data Constraint
    = NotNull
    | Unique
    | PrimaryKey
    | ForeignKey
    | Check Expr
    | Default Expr
    deriving (Show)


data Type
    = TypeInteger
    | TypeChar Integer
    | TypeVarChar Integer
    | TypeDate
    deriving (Show)


data Expr
    = ExprNull
    | ExprVar String
    | ExprStr String
    | ExprNum Integer
    | ExprNot Expr       -- NOT
    | ExprOr  Expr Expr  -- OR
    | ExprAnd Expr Expr  -- AND
    | ExprEq  Expr Expr  -- (=)
    | ExprNeq Expr Expr  -- (<>)
    | ExprGT  Expr Expr  -- (>)
    | ExprGTE Expr Expr  -- (>=)
    | ExprLT  Expr Expr  -- (<)
    | ExprLTE Expr Expr  -- (<=)
    | ExprAdd Expr Expr  -- (+)
    | ExprSub Expr Expr  -- (-)
    | ExprMul Expr Expr  -- (/)
    | ExprDiv Expr Expr  -- (/)
    deriving (Show)


type BinOpExpr = Expr -> Expr -> Expr


data PartialExpr
    = NoOp
    | FoundOp BinOpExpr Expr


type Column = (String, Type, [Constraint])


parse :: String -> [([Command], [Token])]
parse = syntax . lex


lex :: String -> [Token]
lex [] = []
lex (c1:c2:cs)
    | [c1, c2] `elem` ["<>", ">=", "<="]
        = [c1, c2] : lex cs
lex (c:cs)
    | isSpace c = lex cs
    | isAlpha c = readWhile (\x -> isAlphaNum x || x == '_')
    | isNumber c = readWhile isNumber
    | c == '\'' = readString
    | otherwise = [c] : lex cs
    where
        readWhile f =
            let (parts, others) = span f cs
            in  (c : parts) : lex others
        readString =
            let (str, others) = break (== c) cs
            in  case others of
                    []     -> [c] : str : []
                    (_:os) -> [c] : str : [c] : (lex os)


syntax :: Parser [Command]
syntax =
    let pQuery
            =  pCreateDatabase
            |. pCreateTable
            |. pDropTable
            |. pAlterTableAddColumn
            |. pAlterTableDropColumn
    in  pOneOrMore (pThen const pQuery $ pLiteral ";")


-- |
-- >>> pCreateDatabase ["create", "database", "myDB", "foo", "bar"]
-- [(CreateDatabase "myDB",["foo","bar"])]
-- >>> pCreateDatabase ["create", "myDB"]
-- []
-- >>> pCreateDatabase ["create", "database", "_foo"]
-- []
pCreateDatabase :: Parser Command
pCreateDatabase =
    pThen (\_ x -> CreateDatabase x) (pIdiom "create" "database") pVariable


-- @todo support constrant after columns
-- |
-- >>> pCreateTable ["create", "table", "foo", "(", "id", "integer", "primary", "key", ")"]
-- [(CreateTable "foo" [("id",TypeInteger,[PrimaryKey])],[])]
-- 
-- >>> pCreateTable ["CREATE", "TABLE", "BAR", "(", "ID", "varchar", ")", "foo"]
-- [(CreateTable "BAR" [("ID",TypeVarChar 1,[])],["foo"])]
-- 
-- >>> pCreateTable ["create", "table", "baz", "(", "id", "integer", "primary", "key", ",", "name", "char", "(", "255", ")", "unique", ")"]
-- [(CreateTable "baz" [("id",TypeInteger,[PrimaryKey]),("name",TypeChar 255,[Unique])],[])]
-- 
-- >>> pCreateTable ["create", "table", "qux", "(", "my_number", "integer", "not", "null", "unique", ")"]
-- [(CreateTable "qux" [("my_number",TypeInteger,[NotNull,Unique])],[])]
pCreateTable :: Parser Command
pCreateTable =
    let pCreateTableStr = pIdiom "create" "table"
    in  pThen3 (\_ s cs -> CreateTable s cs)
            pCreateTableStr pVariable $ pWithParentheses pColumnDefs


-- |
-- >>> pDropTable ["drop", "table", "foo"]
-- [(DropTable "foo",[])]
-- >>> pDropTable ["DROP", "TABLE", "bar"]
-- [(DropTable "bar",[])]
pDropTable :: Parser Command
pDropTable =
    pThen (\_ x -> DropTable x) (pIdiom "drop" "table") pVariable


-- |
-- >>> pAlterTableAddColumn ["alter", "table", "foo", "add", "column", "id", "integer"]
-- [(AlterTableAddColumn "foo" [("id",TypeInteger,[])],[])]
-- >>> pAlterTableAddColumn ["ALTER", "TABLE", "bar", "ADD", "COLUMN", "id", "INTEGER", ",", "age", "CHAR"]
-- [(AlterTableAddColumn "bar" [("id",TypeInteger,[])],[",","age","CHAR"]),(AlterTableAddColumn "bar" [("id",TypeInteger,[]),("age",TypeChar 1,[])],[])]
pAlterTableAddColumn :: Parser Command
pAlterTableAddColumn =
    let pAlterTable = pIdiom "alter" "table"
        pAddColumn = pIdiom "add" "column"
    in  pThen4
            (\_ table _ cols -> AlterTableAddColumn table cols)
            pAlterTable pVariable pAddColumn pColumnDefs


-- |
-- >>> pAlterTableDropColumn ["alter", "table", "foo", "drop", "column", "id"]
-- [(AlterTableDropColumn "foo" ["id"],[])]
-- >>> pAlterTableDropColumn ["ALTER", "TABLE", "bar", "DROP", "COLUMN", "id", ",", "age"]
-- [(AlterTableDropColumn "bar" ["id"],[",","age"]),(AlterTableDropColumn "bar" ["id","age"],[])]
pAlterTableDropColumn :: Parser Command
pAlterTableDropColumn =
    let pAlterTable = pIdiom "alter" "table"
        pDropColumn = pIdiom "drop" "column"
        pColumns = pThen (:) pVariable $ pZeroOrMore
            $ pThen (flip const) pComma pVariable
    in  pThen4
            (\_ table _ cols -> AlterTableDropColumn table cols)
            pAlterTable pVariable pDropColumn pColumns


-- |
-- >>> pColumnDefs ["id", "integer"]
-- [([("id",TypeInteger,[])],[])]
-- 
-- >>> pColumnDefs ["id", "integer", ",", "created_at", "date"]
-- [([("id",TypeInteger,[])],[",","created_at","date"]),([("id",TypeInteger,[]),("created_at",TypeDate,[])],[])]
pColumnDefs :: Parser [Column]
pColumnDefs =
    let pColumnDef = pThen3 (\col typ con -> (col, typ, con)) pVariable pType pConstraint
        pCommaColumnDef = pThen (flip const) pComma pColumnDef
    in  pThen (:) pColumnDef $ pZeroOrMore pCommaColumnDef


-- |
-- >>> pType ["integer"]
-- [(TypeInteger,[])]
-- >>> pType ["date","foo"]
-- [(TypeDate,["foo"])]
-- >>> pType ["char"]
-- [(TypeChar 1,[])]
-- >>> pType ["varchar", "(", "255", ")"]
-- [(TypeVarChar 1,["(","255",")"]),(TypeVarChar 255,[])]
pType :: Parser Type
pType
    =  pSingleType TypeInteger  "integer"
    |. pVarLenType TypeChar     "char"
    |. pVarLenType TypeVarChar  "varchar"
    |. pSingleType TypeDate     "date"
        where
            pLengthOption = pEmpty 1 |. pWithParentheses pInteger
            pVarLenType t s = pThen (\_ n -> t n) (pKeyword s) pLengthOption
            pSingleType t s = pConstant t (pKeyword s)


-- To support to specify constraints by any order, this parser connives at the duplication.
-- Because supporting both makes code complex unbelievably.
-- |
-- >>> pConstraint ["foreign", "key", "unique"]
-- [([],["foreign","key","unique"]),([ForeignKey],["unique"]),([ForeignKey,Unique],[])]
-- 
-- >>> pConstraint ["unique", "foreign", "key"]
-- [([],["unique","foreign","key"]),([Unique],["foreign","key"]),([Unique,ForeignKey],[])]
-- 
---- >>> pConstraint ["unique", "unique"]
---- []
pConstraint :: Parser [Constraint]
pConstraint
    = pZeroOrMore
        $  pNotNull
        |. pUnique
        |. pPrimaryKey
        |. pForeignKey
        |. pCheck
        |. pDefault

-- |
-- >>> pNotNull ["not", "null"]
-- [(NotNull,[])]
pNotNull :: Parser Constraint
pNotNull = pConstant NotNull $ pIdiom "not" "null"

-- |
-- >>> pUnique ["Unique"]
-- [(Unique,[])]
pUnique :: Parser Constraint
pUnique = pConstant Unique $ pKeyword "unique"

-- |
-- >>> pPrimaryKey ["PRIMARY", "KEY"]
-- [(PrimaryKey,[])]
pPrimaryKey :: Parser Constraint
pPrimaryKey = pConstant PrimaryKey $ pIdiom "primary" "key"

-- |
-- >>> pForeignKey ["foreign", "key"]
-- [(ForeignKey,[])]
pForeignKey :: Parser Constraint
pForeignKey = pConstant ForeignKey $ pIdiom "foreign" "key"

-- |
-- >>> pCheck ["check", "(", "foo", ">=", "1", ")"]
-- [(Check (ExprGTE (ExprVar "foo") (ExprNum 1)),[])]
pCheck :: Parser Constraint
pCheck = pThen (\_ expr -> Check expr) (pKeyword "check") $ pWithParentheses pExpr

-- |
-- >>> pDefault ["default", "12"]
-- [(Default (ExprNum 12),[])]
-- >>> pDefault ["default", "'", "foo", "'"]
-- [(Default (ExprStr "foo"),[])]
pDefault :: Parser Constraint
pDefault = pThen (\_ expr -> Default expr) (pKeyword "default") pExpr


-- The following parsers are according to this BNF.
-- These express the priority of the operations.
--
--      <expr>  ::= <expr1>
--      <expr1> ::= <expr2> "or" <expr1>
--                | <expr2>
--      <expr2> ::= <expr3> "and" <expr2>
--                | <expr3>
--      <expr3> ::= "not" <expr3>
--                | <expr4>
--      <expr4> ::= <expr5> <relop> <expr5>
--                | <expr5>
--      <relop> ::= "=" | "<>" | ">" | ">=" | "<" | "<="
--      <expr5> ::= <expr5> "+" <expr6>
--                | <expr5> "-" <expr6>
--                | <expr6>
--      <expr6> ::= <expr6> "*" <expr7>
--                | <expr6> "/" <expr7>
--                | <expr7>
--      <expr7> ::= <var>
--                | <num>
--                | "(" <expr1> ")"
pExpr :: Parser Expr
pExpr = pExpr1

pExpr1 :: Parser Expr
pExpr1 = pRightAssociativeExpr [(ExprOr, pKeyword "or")] pExpr2 pExpr1

pExpr2 :: Parser Expr
pExpr2 = pRightAssociativeExpr [(ExprAnd, pKeyword "and")] pExpr3 pExpr2

pExpr3 :: Parser Expr
pExpr3 =
    let pNot p = pThen (\_ expr -> ExprNot expr) (pKeyword "not") p
    in  pNot pExpr3 |. pExpr4

pExpr4 :: Parser Expr
pExpr4 =
    let relOps =
            [ (ExprEq , pLiteral "=" )
            , (ExprNeq, pLiteral "<>")
            , (ExprGT , pLiteral ">" )
            , (ExprGTE, pLiteral ">=")
            , (ExprLT , pLiteral "<" )
            , (ExprLTE, pLiteral "<=")
            ]
    in  pRightAssociativeExpr relOps pExpr5 pExpr5

pExpr5 :: Parser Expr
pExpr5 =
    let ops = [(ExprAdd, pLiteral "+"), (ExprSub, pLiteral "-")]
    in  pLeftAssociativeExpr ops pExpr6 pExpr6

pExpr6 :: Parser Expr
pExpr6 =
    let ops = [(ExprMul, pLiteral "*"), (ExprDiv, pLiteral "/")]
    in  pLeftAssociativeExpr ops pExpr7 pExpr7

pExpr7 :: Parser Expr
pExpr7
    =  pConstant ExprNull pNull
    |. pMap ExprVar pVariable
    |. pMap ExprVar pTableColumn
    |. pMap ExprStr pString
    |. pMap ExprNum pInteger
    |. pWithParentheses pExpr1
        where
            pTableColumn = pThen3 (\x y z -> x ++ y ++ z) pVariable (pLiteral ".") pVariable


pRightAssociativeExpr :: [(BinOpExpr, Parser a)] -> Parser Expr -> Parser Expr -> Parser Expr
pRightAssociativeExpr binOps pLeft pRight =
    let assembleOp e1 NoOp = e1
        assembleOp e1 (FoundOp op e2) = op e1 e2
        -- pPartial parses a pair of a bin-op symbol and an expression like [">=", "1"].
        pPartial binOp pSymbol = pThen (\_ expr -> FoundOp binOp expr) pSymbol pRight
        -- And pPartialBinOps is same as (pPartial op1 sym1 |. pPartial op2 sym2 |. ... |. pEmpty NoOp).
        pPartialBinOps = foldl (\p tup -> uncurry pPartial tup |. p) (pEmpty NoOp) binOps
    in  pThen assembleOp pLeft pPartialBinOps

pLeftAssociativeExpr :: [(BinOpExpr, Parser a)] -> Parser Expr -> Parser Expr -> Parser Expr
pLeftAssociativeExpr (bo:bos) pLeft pRight =
    let pPartial binOp pSymbol = pThen (\_ expr -> (binOp, expr)) pSymbol pRight
        -- Simply passing the function itself as pLeft for right associative method fails by endless loop.
        -- So first we make a list of bin-op and terms by pZeroOrMore and then apply foldl to it.
        pPartialBinOps = pZeroOrMore $
            foldl (\p tup -> uncurry pPartial tup |. p) (uncurry pPartial bo) bos
    in  pMap (uncurry $ foldl (\e1 (op, e2) -> op e1 e2)) $
            pThen (\expr tups -> (expr, tups)) pLeft pPartialBinOps
