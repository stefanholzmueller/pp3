module Test.Main where

import Prelude

import Check (Dice(..), Difficulty(..), Outcome(..), evaluate)
import Control.Monad.Gen (chooseInt)
import Effect (Effect)
import Test.StrongCheck (class Arbitrary, arbitrary, assert, (===))

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
  assert ((evaluate { attributes: [11, 12, 13], skill: 4 } (Difficulty 0) (Dice [1, 1, 1])) === AutomaticSuccess 4)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-5)) (Dice [1, 1, 1])) === AutomaticSuccess 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty (-5)) (Dice [1, 9, 1])) === AutomaticSuccess 3)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty 10) (Dice [1, 9, 1])) === AutomaticSuccess 3)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty 7) (Dice [1, 1, 1])) === AutomaticSuccess 3)
  assert ((evaluate { attributes: [11, 12, 13], skill: 3 } (Difficulty (-2)) (Dice [1, 1, 1])) === AutomaticSuccess 3)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-2)) (Dice [1, 1, 1])) === AutomaticSuccess 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty (-2)) (Dice [1, 1, 1])) === AutomaticSuccess 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: 5 } (Difficulty 5) (Dice [1, 9, 1])) === AutomaticSuccess 5)
  assert ((evaluate { attributes: [11, 12, 13], skill: (-2) } (Difficulty 5) (Dice [1, 9, 1])) === AutomaticSuccess 1)
  assert ((evaluate { attributes: [11, 12, 13], skill: 20 } (Difficulty (-5)) (Dice [20, 20, 20])) === AutomaticFailure)
  assert ((evaluate { attributes: [11, 12, 13], skill: 20 } (Difficulty 5) (Dice [15, 20, 20])) === AutomaticFailure)
  -- quality of success is always >=1 and <=skill
  -- TODO

--newtype RandomDifficulty = Difficulty Int
--instance arbitraryDifficulty :: Arbitrary RandomDifficulty where
--  arbitrary = chooseInt (-7) 20