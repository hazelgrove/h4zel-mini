pub type Position = u8;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Clone, Serialize, Deserialize)]
pub enum Constructor{
    // Types
    Typ,
    Num,
    // Expressions/Patterns
    Zero,
    Plus,
    Prod,
    Pair,
    Arrow,
    Fun,
    Ap,
    Asc,
    Let,
    Identifier(String),
    // Projector wrapper: Proj(projector_type, child)
    Proj,
    // Projector types
    Structural,  // nullary - default expanded view
    Collapsed,   // nullary - collapsed view
    Labeled,     // arity 1 - stores a label (child 0 is the label term)
    Canvas,      // arity 1 - visual graph view (child 0 is position list)
    // Position map constructors for Canvas projector
    // Stores node positions as: PosCons(nodeIdent, x, y, rest) where nodeIdent/x/y are Identifiers
    PosNil,      // arity 0 - empty position list
    PosCons,     // arity 4 - (nodeIdent, x, y, tail)
    // Cursor: wraps the selected term/location in the world tree
    // Cursor(identity, content) where:
    //   - position 0: identity (Identifier with user ID)
    //   - position 1: content (wrapped term, or empty if selecting a hole)
    Cursor,      // arity 2 - (identity, content)
}

impl Constructor {
    pub fn arity(&self) -> Position {
        match self {
            Constructor::Typ => 0,
            Constructor::Num => 0,
            Constructor::Zero => 0,
            Constructor::Plus => 2,
            Constructor::Prod => 2,
            Constructor::Pair => 2,
            Constructor::Arrow => 2,
            Constructor::Fun => 2,
            Constructor::Ap => 2,
            Constructor::Asc => 2,
            Constructor::Let => 3,
            Constructor::Identifier(_) => 0,
            // Proj has 2 children: projector type and the wrapped term
            Constructor::Proj => 2,
            // Projector types
            Constructor::Structural => 0,
            Constructor::Collapsed => 0,
            Constructor::Labeled => 1,  // child 0 stores the label
            Constructor::Canvas => 1,   // child 0 stores position list
            Constructor::PosNil => 0,
            Constructor::PosCons => 4,  // nodeIdent, x, y, tail
            Constructor::Cursor => 2,   // identity, content
        }
    }

    // pub fn _to_string(&self) -> String {
    //     match self {
    //         Constructor::Num => "Num".to_string(),
    //         Constructor::Zero => "Zero".to_string(),
    //         Constructor::Plus => "Plus".to_string(),
    //         Constructor::Prod => "Prod".to_string(),
    //         Constructor::Pair => "Pair".to_string(),
    //         Constructor::Fun => "Fun".to_string(),
    //         Constructor::Ap => "Ap".to_string(),
    //         Constructor::Let => "Let".to_string(),
    //         Constructor::Identifier(x) => "Identifier-".to_string() + x,
    //     }
    // }
}