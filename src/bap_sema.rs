use crate::bitvec::BitVec;

struct BaseInstruction {
    id: String,
    llvm_mc_id: Option<String>,

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

