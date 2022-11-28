module RiverCrossing where

import Search

import Data.Set (Set)
import qualified Data.Set as Set 

-- A farmer is on a riverbank with a wolf, a goat and a cabbage, and wants to transport all three to the other side of the river in his boat.
    -- The boat can only be operated by the farmer, with room for at most one of the three items. 
        -- In his absence, the wolf would eat the goat, and the goat would eat the cabbage.

-- type synonym for Bool.
type Bank = Bool

-- New Data Type, called State, with a constructor called state, containing 
data State = State Bank Bank Bank Bank
    deriving (Eq, Ord, Show)

-- startState contains the starting positions, where everything is on the near bank of the river.
startState :: State
startState = State False False False False

-- goalState contains the ending positions, where everything is on the far bank of the river.
goalState :: State
goalState = State True True True True

-- safe takes a State as input and outputs a Bool. This represents where a State is safe, ie nothing will be eaten. 
safe :: State -> Bool
safe (State farmer wolf goat cabbage) = (goat == farmer) || (goat == True)

crossings :: State -> Set State
crossings (State farmer wolf goat cabbage) = 
    Set.fromList $ filter safe $
        [State (cross farmer) wolf goat cabbage] ++
        [State (cross farmer) (cross wolf) goat cabbage | wolf == farmer] ++
        [State (cross farmer) wolf (cross goat) cabbage | goat == farmer] ++
        [State (cross farmer) wolf goat (cross cabbage) | cabbage == farmer]

cross :: Bank -> Bank
cross = not