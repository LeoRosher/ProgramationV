
module ExpressionV2 where
import Data.Char

p = 1000000007

data Expression = Add Expression Expression
                | Sub Expression Expression
                | TermExpr Term
                deriving Show

data Term = Mul Term Term
          | Div Term Term
          | FactorTerm Factor
          deriving Show

data Factor = NumFactor Double
            | NegFactor Factor
            | ParenExpr Expression
            deriving Show

evalExp :: Expression -> Double
evalExp (Add e1 e2) = evalExp e1 + evalExp e2
evalExp (Sub e1 e2) = evalExp e1 - evalExp e2
evalExp (TermExpr t) = evalTerm t

evalTerm :: Term -> Double
evalTerm (Mul t1 t2) = evalTerm t1 * evalTerm t2
evalTerm (Div t1 t2) = evalTerm t1 / evalTerm t2
evalTerm (FactorTerm f) = evalFac f

evalFac :: Factor -> Double
evalFac (NumFactor num) = num
evalFac (NegFactor f) = -evalFac f
evalFac (ParenExpr e) = evalExp e


tokenize :: String -> [String]
tokenize "" = []
tokenize ('+':xs) = "+" : tokenize xs
tokenize ('-':xs) = "-" : tokenize xs
tokenize ('*':xs) = "*" : tokenize xs
tokenize ('/':xs) = "/" : tokenize xs
tokenize ('(':xs) = "(" : tokenize xs
tokenize (')':xs) = ")" : tokenize xs
tokenize str@(x:xs)
    | isDigit x = let (num, rest) = span isDigit str
                  in num : tokenize rest
    | isSpace x = tokenize xs
    | otherwise = [[x]]

parseExpression :: [String] -> (Expression, [String])
parseExpression tokens = case parseTerm tokens of
    (termExpr, "+":remainingTokens) -> let (nextTerm, rest) = parseExpression remainingTokens
                                       in (Add (TermExpr termExpr) nextTerm, rest)
    (termExpr, "-":remainingTokens) -> let (nextTerm, rest) = parseExpression remainingTokens
                                       in (Sub (TermExpr termExpr) nextTerm, rest)
    (termExpr, remainingTokens)     -> (TermExpr termExpr, remainingTokens)

parseTerm :: [String] -> (Term, [String])
parseTerm tokens = case parseFactor tokens of
    (factorTerm, "*":remainingTokens) -> let (nextFactor, rest) = parseTerm remainingTokens
                                         in (Mul (FactorTerm factorTerm) nextFactor, rest)
    (factorTerm, "/":remainingTokens) -> let (nextFactor, rest) = parseTerm remainingTokens
                                         in (Div (FactorTerm factorTerm) nextFactor, rest)
    (factorTerm, remainingTokens)     -> (FactorTerm factorTerm, remainingTokens)

parseFactor :: [String] -> (Factor, [String])
parseFactor ("(":tokens) = let (expr, rest) = parseExpression tokens
                            in (ParenExpr expr, tail rest)
parseFactor ("+":tokens) = parseFactor tokens
parseFactor ("-":tokens) = let (factorTerm, rest) = parseFactor tokens
                            in (NegFactor factorTerm, rest)
parseFactor (num:tokens) = (NumFactor (read num), tokens)

parseExpressionString :: String -> Expression
parseExpressionString str = case parseExpression (tokenize str) of
    (expr, []) -> expr



analize :: String -> Int
analize x = mod (round (evalExp (parseExpressionString x))) p





e0 = "22*79-21"
e1 = "4/-2/2+8"
e2 = "55+3-45*33-25"
e3 = "4/-2/(2+8)"

t0 = Sub (TermExpr (Mul (FactorTerm (NumFactor 22)) (FactorTerm (NumFactor 79)))) (TermExpr (FactorTerm (NumFactor 21)))