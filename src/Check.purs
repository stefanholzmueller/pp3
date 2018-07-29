module Check (
  evaluate,
  evaluateToString,
  Difficulty(..),
  Dice(..),
  Outcome(..)
) where
  
import Prelude

import Data.Array (filter, length, zip)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Tuple (Tuple(..))


--data Edition = Four | Five
--data Traits = FesteMatrix | WildeMagie | Spruchhemmung

type Stats = { attributes :: Array Int
             , skill :: Int
             }
type Adjustments = { attributeModifier :: Int
                   , points :: Int
                   }

newtype Difficulty = Difficulty Int
newtype Dice = Dice (Array Int)

data Outcome = Success Int  -- quality; must be >=1 and <=skill
             | Failure
derive instance eqOutcome :: Eq Outcome
instance showOutcome :: Show Outcome where
  show (Success quality) = "Success(quality=" <> show quality <> ")"
  show Failure = "Failure"


evaluateToString :: Stats -> Difficulty -> Dice -> String
evaluateToString stats difficulty dice =
  toString $ evaluate stats difficulty dice
  where
    toString outcome = case outcome of
      Success quality -> "Gelungen mit " <> show quality <> " übrigen Punkt" <> if quality == 1 then "" else "en"
      Failure         -> "Misslungen"

evaluate :: Stats -> Difficulty -> Dice -> Outcome
evaluate stats difficulty dice =
  fromMaybe' thunkedRegularOutcome (specialOutcome stats.skill dice)
  where
    thunkedRegularOutcome = const $ regularOutcome stats (applyDifficultyEdition4 stats difficulty) dice

applyDifficultyEdition4 :: Stats -> Difficulty -> Adjustments
applyDifficultyEdition4 { attributes, skill } (Difficulty difficulty) =
  { attributeModifier, points }
  where
    ease = skill - difficulty
    attributeModifier = if ease < 0 then ease else 0
    points = if ease < 0 then 0 else ease

regularOutcome :: Stats -> Adjustments -> Dice -> Outcome
regularOutcome { attributes, skill } { attributeModifier, points } (Dice dice) =
  if usedPoints > points
  then Failure
  else success (points - usedPoints)
  where
    reducedAttributes = map (_ + attributeModifier) attributes
    comparisions = map (\(Tuple attr die) -> die - attr) (zip reducedAttributes dice)
    exceedings = filter (_ > 0) comparisions
    usedPoints = sum exceedings
    success leftoverPoints = Success (max 1 (min skill leftoverPoints))

specialOutcome :: Int -> Dice -> Maybe Outcome
specialOutcome skill (Dice dice) =
  if atLeastTwo 1 then Just (Success skill)
  else if atLeastTwo 20 then Just Failure else Nothing
  where
    atLeastTwo pips = length (filter (\x -> x == pips) dice) > 2
