#!/usr/bin/env stack
-- stack script --resolver lts-8.12 --package conduit-combinators
import Conduit

main = do -- Pure operations: summing numbers.
  print $ runConduitPure $ yieldMany [1..10] .| sumC
