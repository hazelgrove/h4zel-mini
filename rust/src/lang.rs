pub type Position = u8;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Clone, Serialize, Deserialize)]
pub enum Constructor{
    Zero,
    Plus,
    Pair,
    Fun,
    Ap,
    Let,
}

impl Constructor {
    pub fn arity(&self) -> Position {
        match self {
            Constructor::Zero => 0,
            Constructor::Plus => 2,
            Constructor::Pair => 2,
            Constructor::Fun => 2,
            Constructor::Ap => 2,
            Constructor::Let => 3,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Constructor::Zero => "Zero",
            Constructor::Plus => "Plus",
            Constructor::Pair => "Pair",
            Constructor::Fun => "Fun",
            Constructor::Ap => "Ap",
            Constructor::Let => "Let",
        }.to_string()
    }
}