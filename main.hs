{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

import GHC.TypeLits

type Signal = Bool
data Gate (inputs :: Nat) where
  Gate :: ([Signal] -> Signal) -> Gate inputs

gateAnd :: Gate 2
gateAnd = Gate (\[x,y] -> x && y)

gateOr :: Gate 2
gateOr = Gate (\[x,y] -> x || y)

notGate :: Gate 1
notGate = Gate(\[x] -> not x)

applyGate :: Gate n -> [Signal] -> Signal
applyGate (Gate g) inputs = g inputs

main :: IO ()
main = putStrLn "Hello, world!"
