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
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Tuple (Tuple(..))


--data Edition = Four | Five
--data Traits = FesteMatrix | WildeMagie | Spruchhemmung

type Stats = { attributes :: Array Int
             , skill :: Int
             }
type AdjustedStats = { attributes :: Array Int
                     , points :: Int
                     , skill :: Int
                     }

newtype Difficulty = Difficulty Int
newtype Dice = Dice (Array Int)

data Outcome = Success Int  -- quality; must be >=1 (and <=skill if positive)
             | Failure
derive instance eqOutcome :: Eq Outcome
derive instance genericOutcome:: Generic Outcome _
instance showOutcome :: Show Outcome where
  show = genericShow


-- TODO: move to bot code
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
    thunkedRegularOutcome = const $ regularOutcome (applyDifficultyEdition4 stats difficulty) dice

applyDifficultyEdition4 :: Stats -> Difficulty -> AdjustedStats
applyDifficultyEdition4 { attributes, skill } (Difficulty difficulty) =
  { attributes: reducedAttributes, points, skill }
  where
    ease = skill - difficulty
    attributeModifier = if ease < 0 then ease else 0
    points = if ease < 0 then 0 else ease
    reducedAttributes = map (_ + attributeModifier) attributes

regularOutcome :: AdjustedStats -> Dice -> Outcome
regularOutcome { attributes, points, skill } (Dice dice) =
  if usedPoints > points
  then Failure
  else success (points - usedPoints)
  where
    comparisions = map (\(Tuple attr die) -> die - attr) (zip attributes dice)
    exceedings = filter (_ > 0) comparisions
    usedPoints = sum exceedings
    success leftoverPoints = Success (max 1 (min skill leftoverPoints))

specialOutcome :: Int -> Dice -> Maybe Outcome
specialOutcome skill (Dice dice) =
  if atLeastTwo 1 then Just (Success (max 1 skill))
  else if atLeastTwo 20 then Just Failure else Nothing
  where
    atLeastTwo pips = length (filter (_ == pips) dice) >= 2
