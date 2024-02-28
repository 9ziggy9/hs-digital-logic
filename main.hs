import Data.List (find)
import Data.Maybe (catMaybes, fromJust)

type WireId = Int
type GateId = Int
type Signal = Bool

data GateType =
  Input
  | Output
  | Not
  | Or
  | And
  | Xor
  | Nor
  | Nand deriving (Show, Eq)

data Gate = Gate { gateId :: GateId, gateType :: GateType } deriving (Show, Eq)

data Wire = Wire {
  wireId   :: WireId,
  fromGate :: GateId,
  toGate   :: GateId,
  signal   :: Signal
} deriving (Show, Eq)

-- Representation of directed graph.
data Circuit = Circuit {
  gates   :: [Gate],
  wires   :: [Wire]
} deriving (Show, Eq)

-- Function to evaluate a gate given its type and input signals
evalGate :: GateType -> [Signal] -> Signal
evalGate Not  [a]    = not a
evalGate And  [a, b] = a && b
evalGate Or   [a, b]  = a || b
evalGate Xor  [a, b] = a /= b
evalGate Nor  [a, b] = not (a || b)
evalGate Nand [a, b] = not (a && b)
evalGate _ _         = error "Invalid input"

test_evalGate :: IO ()
test_evalGate = do
  print (evalGate Not [True])
  print (evalGate And [True, False])
  print (evalGate Or  [True, False])
  print (evalGate Xor [True, False])

test_circuit :: IO ()
test_circuit = do
  let in1 = Gate { gateId = 1, gateType = Input }
      in2 = Gate { gateId = 2, gateType = Input }
  let out1 = Gate { gateId = 3, gateType = Output }
      out2 = Gate { gateId = 4, gateType = Output }
  print Circuit {
    gates = [in1, in2, out1, out2],
    wires = []
  }

main :: IO ()
main = do
  test_evalGate
  test_circuit

  
