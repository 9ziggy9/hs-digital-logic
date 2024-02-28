import Data.List (find)
import Data.Maybe (catMaybes, fromJust)

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
  wireId   :: WireId,
  fromGate :: Maybe GateId,
  toGate   :: Maybe GateId,
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

evaluateCircuit :: Circuit -> Circuit
evaluateCircuit circuit = circuit { wires = updatedWires }
  where
    -- Find input signals for each gate
    inputSignals gate = map signal $ filter (\wire -> toGate wire == Just (gateId gate)) (wires circuit)
    
    -- Evaluate signal for each gate
    gateOutputSignals = map (\gate -> (gateId gate, evalGate (gateType gate) (inputSignals gate))) (gates circuit)
    
    -- Update signal in wires based on gate outputs
    updatedWires = map (updateWireSignal gateOutputSignals) (wires circuit)
    
    -- Update wire signal if it's an output from a gate
    updateWireSignal gateOutputs wire =
      case fromGate wire of
        Just gateId -> case lookup gateId gateOutputs of
                        Just newSignal -> wire { signal = newSignal }
                        Nothing -> wire
        Nothing -> wire

test_evalGate :: IO ()
test_evalGate = do
  print (evalGate Not [True])
  print (evalGate And [True, False])
  print (evalGate Or  [True, False])
  print (evalGate Xor [True, False])

test_circuit :: IO ()
test_circuit = do
  let g1  = Gate { gateId = 1, gateType = And }
  let in1 = Wire { wireId = 1, signal = True,  toGate   = Just 1, fromGate = Nothing }
      in2 = Wire { wireId = 2, signal = True,  toGate   = Just 1, fromGate = Nothing }
  let out = Wire { wireId = 3, signal = False, fromGate = Just 1, toGate   = Nothing }
  let testCircuit = Circuit {
        gates = [g1],
        wires = [in1, in2, out]
  }
  print testCircuit
  print (evaluateCircuit testCircuit)

main :: IO ()
main = do
  test_evalGate
  test_circuit

  
