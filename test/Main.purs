module Test.Main where

import Prelude

import Check (Dice(..), Difficulty(..), Outcome(..), evaluate)
import Effect (Effect)
import Test.StrongCheck (assert, (===))

main :: Effect Unit
main = do
  assert ((evaluate { attributes: [11, 12, 13], skill: 7 } (Difficulty 3) (Dice [1, 2, 3])) === Success 4)
