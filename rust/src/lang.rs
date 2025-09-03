
pub type Position = u8;


#[derive(PartialEq, Clone)]
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