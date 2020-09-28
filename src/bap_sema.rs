use crate::bitvec::BitVec;
use crate::resolve::Operation;

struct BaseInstruction {
    id: String,
    llvm_mc_id: Option<String>,
    input: Vec<(String, VarType)>,
    ops: Operation
}

enum VarType {
    Bit,
    Bitv(u16)
}

struct Variable {
    name: String,
    ty: VarType
}

struct Constant {
    value: BitVec,
    ty: VarType
}

