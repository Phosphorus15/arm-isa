use std::str::FromStr;

pub struct BitVec {
    vec: Vec<bool>
}

impl BitVec {
    pub fn data(&self) -> &Vec<bool> {
        &self.vec
    }

    pub fn data_mut(&mut self) -> &mut Vec<bool> {
        &mut self.vec
    }
}

impl Into<Vec<bool>> for BitVec {
    fn into(self) -> Vec<bool> {
        self.vec
    }
}

impl FromStr for BitVec {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.chars().any(|it| it != '0' && it != '1') {
            Err(())
        } else {
            Ok(
                BitVec {
                    vec: s.chars().map(|it| it == '1').collect()
                }
            )
        }
    }
}