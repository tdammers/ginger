module Main where

import Test.Tasty

import Text.Ginger.SimulationTests (simulationTests)
import Text.Ginger.PropertyTests (propertyTests)

main = defaultMain allTests

allTests :: TestTree
allTests =
    testGroup "All Tests"
        [ simulationTests
        , propertyTests
        ]
