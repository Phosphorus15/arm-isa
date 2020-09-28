/*
 * In many cases, we should determine if a variable could be taken as constant
 * for example, in BAP lifter's perspective, only register values are not statically determined (that are vars)
 * which means the rest could (and maybe should) be properly decoded and get optimized
 *
 * This `env.rs` file defines a set of environment assets to help the resolution process
 * determines when and how a variable or function should be encoded
 */

use crate::resolve::TypeUnresolved;
use std::rc::Rc;

pub struct Env {

}

// local environment for instruction
pub struct DefEnv {
    base: Rc<Env>
}

impl From<Rc<Env>> for DefEnv {
    fn from(global: Rc<Env>) -> Self {
        Self {
            base: global
        }
    }
}

pub struct GlobalVar {
    id: String,
    ty: TypeUnresolved,
}

pub struct GlobalFunc {
    id: String,
}

// which are always parameters
pub struct LocalVal {
    id: String,
    ty: TypeUnresolved,
}

pub struct GlobalEnum {
    variants: Vec<String>
}
