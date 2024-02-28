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
