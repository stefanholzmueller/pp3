module Test.Main where

import Prelude

import Check (Dice(..), Difficulty(..), Outcome(..), evaluate)
import Control.Monad.Gen (chooseInt)
import Effect (Effect)
import Test.StrongCheck (class Arbitrary, Result, annotate, assert, quickCheck, (===))
import Test.StrongCheck.Gen (suchThat, vectorOf)

main :: Effect Unit
main = do
  -- regular outcomes
  assert ((evaluate { attributes: [11, 12, 13], skill: 7 } (Difficulty 3) (Dice [1, 2, 3])) === Success 4)
  assert ((evaluate { attributes: [11, 12, 13], skill: 0 } (Difficulty 0) (Dice [11, 12, 13])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: 0 } (Difficulty 0) (Dice [15, 15, 15])) === Failure)
  assert ((evaluate { attributes: [11, 12, 13], skill: 4 } (Difficulty 4) (Dice [10, 10, 10])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-2)) (Dice [10, 10, 10])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-2)) (Dice [11, 12, 16])) === Failure)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-2)) (Dice [5, 5, 5])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-2)) (Dice [5, 5, 5])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty 0) (Dice [10, 10, 10])) === Failure)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty 0) (Dice [10, 15, 10])) === Failure)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty 2) (Dice [10, 9, 7])) === Failure)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty 8) (Dice [14, 3, 18])) === Failure)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty 8) (Dice [4, 3, 5])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: 10 } (Difficulty (-10)) (Dice [19, 19, 19])) === Failure)
  assert ((evaluate { attributes: [11, 12, 13], skill: 6 } (Difficulty 0) (Dice [7, 3, 6])) === Success 6)
  assert ((evaluate { attributes: [11, 12, 13], skill: 20 } (Difficulty (-10)) (Dice [13, 12, 11])) === Success 20)
  assert ((evaluate { attributes: [11, 12, 13], skill: 20 } (Difficulty (-10)) (Dice [16, 16, 16])) === Success 18)
  -- special outcomes
  assert ((evaluate { attributes: [11, 12, 13], skill: 4 } (Difficulty 0) (Dice [1, 1, 1])) === Success 4)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-5)) (Dice [1, 1, 1])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty (-5)) (Dice [1, 9, 1])) === Success 3)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty 10) (Dice [1, 9, 1])) === Success 3)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty 7) (Dice [1, 1, 1])) === Success 3)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty (-2)) (Dice [1, 1, 1])) === Success 3)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-2)) (Dice [1, 1, 1])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-2)) (Dice [1, 1, 1])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: 5 } (Difficulty 5) (Dice [1, 9, 1])) === Success 5)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty 5) (Dice [1, 9, 1])) === Success 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: 20 } (Difficulty (-5)) (Dice [20, 20, 20])) === Failure)
  assert ((evaluate { attributes: [11, 12, 13], skill: 20 } (Difficulty 5) (Dice [15, 20, 20])) === Failure)
  -- quality of success is always >=1 (and <=skill)
  quickCheck randomOutcome
  where
    randomOutcome :: SuccessfulCheck -> Result
    randomOutcome (SuccessfulCheck check) = let outcome = evaluateCheck check
                                                quality = case outcome of
                                                            Success q -> q
                                                            Failure -> 0
                                                correct = if check.skill > 0 then quality >= 1 && quality <= check.skill else quality == 1
                                            in annotate correct ("quality must be >=1 (and <=skill), but was " <> show quality <> "\n" <> show check)
    evaluateCheck check = evaluate { attributes: check.attributes, skill: check.skill } (Difficulty check.difficulty) (Dice check.dice)

newtype SuccessfulCheck = SuccessfulCheck { attributes :: Array Int
                                          , skill :: Int
                                          , difficulty :: Int
                                          , dice :: Array Int
                                          }
instance arbitrarySuccessfulCheck :: Arbitrary SuccessfulCheck where
  arbitrary =
    let genCheck = do attributes <- vectorOf 3 (chooseInt 7 17)
                      skill <- chooseInt (-2) 18
                      difficulty <- chooseInt (-7) 15
                      dice <- vectorOf 3 (chooseInt 1 20)
                      pure $ SuccessfulCheck { attributes, skill, difficulty, dice }
        evaluateCheck (SuccessfulCheck check) = evaluate { attributes: check.attributes, skill: check.skill } (Difficulty check.difficulty) (Dice check.dice)
        isSuccess outcome = case outcome of
          Success _ -> true
          Failure   -> false
    in suchThat genCheck (\check -> isSuccess $ evaluateCheck check)
