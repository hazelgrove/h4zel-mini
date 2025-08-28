
pub type Position = u8;

pub enum Constructor {
    Zero,
    Plus 
}

impl Constructor {
    pub fn arity(c : &Constructor) -> Position {
        match c {
            Constructor::Zero => 0,
            Constructor::Plus => 2
        }
    }
}