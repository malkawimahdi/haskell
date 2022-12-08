module Command where

import Parser 

-- Command type, has five(5) constructors, whereby 4 have nothing. Steps, contains an int. 
data Command = TurnLeft | TurnRight | Forward Int | PenUp | PenDown
    deriving (Show)

command :: Parser Command
command =
    TurnLeft <$ string "LEFT" <|>
    TurnRight <$ string "RIGHT" <|>
    Forward <$ string "FORWARD" <* space <*> int <|>
    PenUp <$ string "PEN UP" <|> 
    PenDown <$ string "PEN DOWN"

parseCommand :: String -> [Command]
parseCommand = parseAll command

ident :: Parser String
ident = 
    (:) <$> letter <*> many (letter <|> digit)


