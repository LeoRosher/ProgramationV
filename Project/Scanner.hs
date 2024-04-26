{-# LANGUAGE UndecidableInstances #-}
module Scanner where

import Data.Char

type Col = Int
type Line = Int 
type Value = String
type Input = String

data Token = Token Type Value Line Col

data Type = String 
          | OpenBlock
          | EndBlock
          | Keyword
          | EndSlide
          | Error
          | Comment
        deriving(Eq, Ord)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show String = "String: "
    show OpenBlock = "OpenBlock: "
    show EndBlock = "EndBlock: "
    show Keyword = "Keyword: "
    show Error = "Error: "
    show EndSlide = "EndSlide: "

instance (Eq Type) => (Eq Token) where
    (Token String _ _ _) == (Token String s2 _ _) = True
    (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
    (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
    (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
    (Token Error k1 _ _) == (Token Error k2 _ _) = k1 == k2
    (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
    (Token t1 s1 _ _ ) == (Token t2 s2 _ _ ) = t1 == t2 && s1 == s2

instance Ord Token where
    compare x y | x == y = EQ
                | x <= y = LT
                | otherwise = GT
    (Token t1 s1 _ _ ) <= (Token t2 s2 _ _ ) = t1 < t2 || (t1 == t2 && s1 <= s2)

scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan ('\n':xs) l c = scan xs (l+1) 1
scan ('!':xs) l c = Token Keyword "!" l c : scan xs l (c+1)
scan ('{':xs) l c = Token OpenBlock "{" l c : scan xs l (c+1)
scan ('}':xs) l c = Token EndBlock "}" l c : scan xs l (c+1)
scan ('#':'#':'#':'#':'#':'#':xs) l c = Token Keyword "######" l c : scan xs l (c+1)
scan ('#':'#':'#':'#':'#':xs) l c = Token Keyword "#####" l c : scan xs l (c+1)
scan ('#':'#':'#':'#':xs) l c = Token Keyword "####" l c : scan xs l (c+1)
scan ('#':'#':'#':xs) l c = Token Keyword "###" l c : scan xs l (c+1)
scan ('#':'#':xs) l c = Token Keyword "##" l c : scan xs l (c+1)
scan ('#':xs) l c = Token Keyword "#" l c : scan xs l (c+1)
scan ('*':'*':'*':xs) l c = Token Keyword "***" l c : scan xs l (c+1)
scan ('*':'*':xs) l c = Token Keyword "**" l c : scan xs l (c+1)
scan ('*':xs) l c = Token Keyword "*" l c : scan xs l (c+1)
scan ('-':'-':'-':xs) l c = Token EndSlide "---" l c : scan xs l (c+1)
scan ('-':xs) l c = Token Keyword "-" l c : scan xs l (c+1)
scan ('@':xs) l c = Token Keyword "@" l c : scan xs l (c+1)
scan ('&':xs) l c = Token Keyword "&" l c : scan xs l (c+1)
scan ('+':xs) l c = Token Keyword "+" l c : scan xs l (c+1)
scan (';':xs) l c = scan (dropWhile (/= '\n') xs) (l + 1) 1
scan str@(x:xs) l c
    | x == '\n' = scan xs (l+1) 1
    | isAlphaNumOrSpace x = let (string, rest) = span isAlphaNumOrSpace (x:xs)
                   in Token String string l c : scan rest l (c + length string)
    | otherwise = Token Error (show x) l c : scan xs l (c+1)
    
isAlphaNumOrSpace :: Char -> Bool
isAlphaNumOrSpace '\n' = False
isAlphaNumOrSpace c = isAlphaNum c || isSpace c || charInList c "!@#$%^&()-_+=|\\/{}[];:',.<>?\""

charInList :: Char -> [Char] -> Bool
charInList _ [] = False
charInList c (x:xs)
    | c == x = True
    | otherwise = charInList c xs

generateHashtags :: Char -> String
generateHashtags '#' = replicate (digitToInt '#') '#'
generateHashtags _ = ""

scanHeader :: Char -> Line -> Col -> [Token]
scanHeader '#' l c = [Token Keyword (generateHashtags '#') l c]
scanHeader _ _ _ = []


