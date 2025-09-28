pub type Position = u8;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Clone, Serialize, Deserialize)]
pub enum Constructor{
    Num, 
    Zero,
    Plus,
    Pair,
    Fun,
    Ap,
    Let,
    Identifier(String),
}

impl Constructor {
    pub fn arity(&self) -> Position {
        match self {
            Constructor::Num => 0,
            Constructor::Zero => 0,
            Constructor::Plus => 2,
            Constructor::Pair => 2,
            Constructor::Fun => 2,
            Constructor::Ap => 2,
            Constructor::Let => 3,
            Constructor::Identifier(_) => 0,
        }
    }

    pub fn _to_string(&self) -> String {
        match self {
            Constructor::Num => "Num".to_string(),
            Constructor::Zero => "Zero".to_string(),
            Constructor::Plus => "Plus".to_string(),
            Constructor::Pair => "Pair".to_string(),
            Constructor::Fun => "Fun".to_string(),
            Constructor::Ap => "Ap".to_string(),
            Constructor::Let => "Let".to_string(),
            Constructor::Identifier(x) => "Identifier-".to_string() + x,
        }
    }
}