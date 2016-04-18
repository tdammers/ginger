module Main where

import Test.Tasty

import Text.Ginger.SimulationTests (simulationTests)

main = defaultMain allTests

allTests :: TestTree
allTests =
    testGroup "All Tests"
        [ simulationTests
        ]
