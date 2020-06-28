{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import qualified Control.Monad.State.Strict as S
import Data.Char
import Text.ParserCombinators.ReadP hiding (many, many1)

-- The AST
type Block = [Statement]
data Statement
    = PrintStr String
    | PrintExpr Expr
    | Input String
    | Let String Expr
    | While Cond Block
    | If Cond Block
    deriving Show
data UnaryOp = Plus | Minus deriving (Show, Enum, Bounded)
data BinaryOp = Add | Sub | Mul | Div deriving (Show, Enum, Bounded)
data Expr
    = Var String
    | Num Double
    | UnaryOp UnaryOp Expr
    | BinaryOp Expr BinaryOp Expr
    deriving Show
data Comp = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual deriving (Show, Enum, Bounded)
data Cond = Cond Expr Comp Expr deriving Show

unaryOps = \case Plus -> "+"; Minus -> "-"
binaryOps = \case Add -> "+"; Sub -> "-"; Mul -> "*"; Div -> "/"
compOps = \case Equal -> "=="; NotEqual -> "!="; Less -> "<"; LessEqual -> "<="; Greater -> ">"; GreaterEqual -> ">="

-- The parser
ws = (munch \c -> c == ' ' || c == '\t') *> pure ()
ws1 = (munch1 \c -> c == ' ' || c == '\t') *> pure ()
nl = (ws *> munch1 \c -> c == ' ' || c == '\t' || c == '\n') *> pure ()
pProgram = pBlock <* eof
pBlock = many pStatement
pStatement = (pPrint <|> pInput <|> pLet <|> pWhile <|> pIf) <* (nl <|> eof)
pPrint = string "PRINT" *> ws1 *> ((PrintStr <$> pString) <|> (PrintExpr <$> pExpr))
pInput = string "INPUT" *> ws1 *> (Input <$> pIdentifier)
pLet = Let <$> (string "LET" *> ws1 *> pIdentifier <* ws <* string "=" <* ws) <*> pExpr
pWhile = While <$> (string "WHILE" *> ws1 *> pCond <* ws1 <* string "REPEAT" <* nl) <*> (pBlock <* ws <* string "ENDWHILE")
pIf = If <$> (string "IF" *> ws1 *> pCond <* ws1 <* string "THEN" <* nl) <*> (pBlock <* ws <* string "ENDIF")
pString = char '"' *> (concat <$> many (munch1 (\c -> isPrint c && c /= '\\' && c /= '"') <|> escape)) <* char '"'
    where escape = choice [string x' *> pure x' | x <- ['\\', '"', '\'', 'n', 'r', 'b', 't', 'f', 'a', 'v'], let x' = '\\' : x : "" ] 
pExpr = term <|> (BinaryOp <$> term <*> (ws *> addOrSub <* ws) <*> pExpr)
    where
    term = unary <|> (BinaryOp <$> unary <*> (ws *> mulOrDiv <* ws) <*> term)
    unary = primary <|> (UnaryOp <$> (ws *> plusOrMinus <* ws) <*> primary)
    primary = (Var <$> pIdentifier) <|> (Num <$> number)
    addOrSub = op '+' Add <|> op '-' Sub
    mulOrDiv = op '*' Mul <|> op '/' Div
    plusOrMinus = op '+' Plus <|> op '-' Minus
    op a b = ws *> char a *> pure b <* ws
    number = read <$> munch1 isDigit
pCond = Cond <$> pExpr <*> (ws *> op <* ws) <*> pExpr
    where op = choice [string (compOps x) *> pure x | x <- [minBound..]]
pIdentifier = (:) <$> satisfy isLetter <*> munch \c -> isLetter c || isDigit c

-- The code emitter
eVar var = S.gets (var `elem`) >>= \case
    True -> pure ""
    False -> do
        S.modify (var:)
        pure $ "double " <> var <> ";\n"
eProgram prog = do
    emittedBlock <- eBlock prog
    pure $ "#include <stdio.h>\nint main(){\n" <> emittedBlock <> "  return 0;\n}"
eBlock statements = do
    state <- S.get
    emittedStatements <- mapM eStatement statements
    S.put state
    pure $ unlines $ fmap ("  " <>) $ lines $ concat emittedStatements
eStatement = \case
    PrintStr str -> pure $ "puts(\"" <> str <> "\");\n";
    PrintExpr expr -> pure $ "printf(\"%f\\n\", (double)" <> eExpr expr <> ");\n"
    Input var -> (<>) <$> eVar var <*> pure ("scanf(\"%lf\", &" <> var <> ");\n")
    Let var expr -> (<>) <$> eVar var <*> pure (var <> "=" <> eExpr expr <> ";\n")
    While cond block -> conditionalBlock "while" cond block
    If cond block -> conditionalBlock "if" cond block
    where
    conditionalBlock name cond block = do
        emittedBlock <- eBlock block
        pure $ concat [name, " (", eCond cond, ") {\n", emittedBlock, "}\n"]
eExpr = \case
    Var x -> x
    Num x -> show x
    UnaryOp op x -> parens $ unaryOps op <> eExpr x
    BinaryOp x op y -> parens $ eExpr x <> binaryOps op <> eExpr y
    where parens x = concat ["(", x, ")"]
eCond (Cond x cmp y) = eExpr x <> compOps cmp <> eExpr y

-- Main program logic
go str = case readP_to_S pProgram str of
    [] -> error "Invalid program, though I can't tell you why"
    [(prog, [])] -> putStrLn $ S.evalState (eProgram prog) []
    x -> error $ "Something went terribly wrong during parsing: " <> show x
main = getContents >>= go
