type WireId = Int
type GateId = Int
type Signal = Bool

data GateType =
  Not
  | Or
  | And
  | Xor
  | Nor
  | Nand deriving (Show, Eq)

data Gate = Gate { gateId :: GateId, gateType :: GateType } deriving (Show, Eq)

data Wire = Wire {
  wireId :: WireId,
  fromGate :: GateId,
  toGate :: GateId,
  signal :: Signal
} deriving (Show, Eq)

data Circuit = Circuit { gates :: [Gate], wires :: [Wire] } deriving (Show, Eq)

main :: IO ()
main = do
  let g1 = Gate { gateId = 1, gateType = And }
      g2 = Gate { gateId = 2, gateType = Not }

      wire_g1_g2 = Wire {
        wireId = 1,
        fromGate = 1,  -- Output from g1 (And)
        toGate = 2,    -- Input to g2 ( Not )
        signal = False --  Supply to circuit
      }

      nandCircuit = Circuit {
        gates = [g1, g2],
        wires = [wire_g1_g2]
      }

  print nandCircuit

  
