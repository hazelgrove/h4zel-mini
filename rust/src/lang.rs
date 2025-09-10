pub type Position = u8;

#[derive(PartialEq, Clone, Copy)]
pub enum Constructor {
    Zero,
    Plus 
}

impl Constructor {
    pub fn arity(&self) -> Position {
        match self {
            Constructor::Zero => 0,
            Constructor::Plus => 2
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Constructor::Zero => "Zero".to_string(),
            Constructor::Plus => "Plus".to_string()
        }
    }
}