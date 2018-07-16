module Check (evaluate, Difficulty(..), Dice(..), IntTriple, Outcome(..)) where
  
import Prelude

import Data.Array (filter, length, zip)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3)


--data Edition = Four | Five
--data Traits = None | FesteMatrix | WildeMagie | Spruchhemmung

type IntTriple = Tuple3 Int Int Int
type Stats = { attributes :: IntTriple
             , skill :: Int
             }
type Adjustments = { attributeModifier :: Int
                   , points :: Int
                   }

newtype Difficulty = Difficulty Int
newtype Dice = Dice IntTriple

data Outcome = Success Int  -- quality; must be >=1 and <=skill
             | Failure
instance showOutcome :: Show Outcome where
  show (Success quality) = "Success(" <> show quality <> ")"
  show Failure = "Failure"

evaluate :: Stats -> Difficulty -> Dice -> Outcome
evaluate stats difficulty dice = fromMaybe' thunkedRegularOutcome (specialOutcome stats.skill dice)
  where
    thunkedRegularOutcome = const $ regularOutcome stats (applyDifficultyEdition4 stats difficulty) dice

applyDifficultyEdition4 :: Stats -> Difficulty -> Adjustments
applyDifficultyEdition4 { attributes, skill } (Difficulty difficulty) = { attributeModifier, points }
  where
    ease = skill - difficulty
    attributeModifier = if ease < 0 then ease else 0
    points = if ease < 0 then 0 else ease

regularOutcome :: Stats -> Adjustments -> Dice -> Outcome
regularOutcome { attributes, skill } { attributeModifier, points } (Dice dice) = if usedPoints > points
                                                                                 then Failure
                                                                                 else success (points - usedPoints)
  where
    reducedAttributes = map (_ + attributeModifier) (toArray attributes)
    comparisions = map (\(Tuple attr die) -> die - attr) (zip reducedAttributes (toArray dice))
    exceedings = filter (_ > 0) comparisions
    usedPoints = sum exceedings
    success leftoverPoints = Success (max 1 (min skill leftoverPoints))

specialOutcome :: Int -> Dice -> Maybe Outcome
specialOutcome skill (Dice diceTuple) = if atLeastTwo 1 then Just (Success skill)
                                        else if atLeastTwo 20 then Just Failure else Nothing
  where
    diceArray = toArray diceTuple
    atLeastTwo pips = length (filter (\x -> x == pips) diceArray) > 2

toArray :: IntTriple -> Array Int
toArray (Tuple x (Tuple y (Tuple z _))) = [ x, y, z ]
