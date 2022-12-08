module Particles where

import Parser 

-- type defines a point as a synonym for an existing type.
data Point = Point Int Int
    deriving (Eq, Ord, Show)

data Particle = Particle Point Point
    deriving (Show)

type System = [Particle]

particle :: Parser Particle
particle =
    Particle <$ string "position=" <* char '<' <*> point <* char '>' <* space <* string "velocity=" <* char '<' <*> point <* char '>'

point :: Parser Point
point = Point <$> int <* char ',' <*> int 

parseParticles :: String -> System
parseParticles = concatMap (parseAll particle) . lines

move :: Particle -> Particle
move (Particle (Point x1 y1) (Point x2 y2)) = Particle (Point (x1+x2) (y1+y2)) (Point x2 y2)

states :: System -> [System]
states particles = iterate (map move) particles

test1, test2 :: Particle
test1 = Particle (Point 9 1) (Point 0 2)
test2 = Particle (Point 9 1) (Point 1 1)