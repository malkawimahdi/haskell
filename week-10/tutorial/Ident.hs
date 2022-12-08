module Ident where

import Parser 

parseIdent :: String -> [String]
parseIdent = parseAll ident

ident :: Parser String
ident = 
    (:) <$> letter <*> many (letter <|> digit)
