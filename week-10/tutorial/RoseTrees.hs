module RoseTrees where

import Parser
import Ident

type Term = (RTree String)

data RTree a = RNode a [RTree a]
    deriving (Show)

term :: Parser Term
term = 
    (\string -> RNode string [])<$> ident <|>
    RNode <$> ident <* char '(' <*> sepBy1 term (char ',') <* char ')'
    -- (\string terms -> RNode string terms) <$> ident <* char '(' <*> sepBy1 term (char ',') <* char ')'

parseRTree :: String -> [Term]
parseRTree = parseAll term
