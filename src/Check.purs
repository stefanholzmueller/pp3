module Check (
  calculate,
  evaluate,
  evaluateToString,
  Difficulty(..),
  Dice(..),
  Outcome(..)
) where
  
import Prelude

import Data.Array (filter, group', length, range, zip)
import Data.Array.NonEmpty as NEA
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
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

data Outcome = Failure
             | Success Int  -- quality; must be >=1 (and <=skill if positive)
derive instance eqOutcome :: Eq Outcome
derive instance ordOutcome :: Ord Outcome
derive instance genericOutcome:: Generic Outcome _
instance showOutcome :: Show Outcome where
  show = genericShow

type Histogram = Array (Tuple Outcome Int) -- first Failure, then Success ascending
type Calculation = { averageQualityExcludingFailures :: Number
                   , averageQualityIncludingFailures :: Number
                   , chanceForSuccess :: Number
                   , histogram :: Histogram
                   }


calculate :: Stats -> Difficulty -> Calculation
calculate stats difficulty =
  { averageQualityExcludingFailures, averageQualityIncludingFailures, chanceForSuccess, histogram }
  where
    histogram = calculateHistogram stats difficulty
    averageQualityIncludingFailures = toNumber sumOfQualities / 8000.0
    averageQualityExcludingFailures = toNumber sumOfQualities / toNumber successCount
    sumOfQualities = sum $ map multiplyQuality histogram
    multiplyQuality (Tuple Failure _) = 0
    multiplyQuality (Tuple (Success q) n) = q * n
    chanceForSuccess = toNumber successCount / 8000.0
    successCount = sum $ map countSuccesses histogram
    countSuccesses (Tuple Failure _) = 0
    countSuccesses (Tuple (Success _) n) = n

calculateHistogram :: Stats -> Difficulty -> Histogram
calculateHistogram stats difficulty =
  map toTuple $ group' outcomes
  where
    toTuple nonEmptyOutcomes = Tuple (NEA.head nonEmptyOutcomes) (NEA.length nonEmptyOutcomes)
    outcomes = map (evaluateAdjusted adjustedStats) diceCombinations
    adjustedStats = applyDifficultyEdition4 stats difficulty
    diceCombinations = do
      d1 <- range 1 20
      d2 <- range 1 20
      d3 <- range 1 20
      pure $ Dice [ d1, d2, d3 ]

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
  evaluateAdjusted (applyDifficultyEdition4 stats difficulty) dice

evaluateAdjusted :: AdjustedStats -> Dice -> Outcome
evaluateAdjusted adjustedStats dice =
  fromMaybe' thunkedRegularOutcome (specialOutcome adjustedStats.skill dice)
  where
    thunkedRegularOutcome = const $ regularOutcome adjustedStats dice

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
