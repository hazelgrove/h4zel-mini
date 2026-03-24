use serde::{Deserialize, Serialize};

/// Language constructors — every grove node has one.
/// Serialization matches RustTypes.tsx (serde default: unit variants → strings,
/// tuple variants → `{ VariantName: data }`).
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Constructor {
    // Types
    Typ,
    Num,
    // Expressions
    Zero,
    Plus,
    Pair,
    Arrow,
    Fun,
    Asc,
    Ap,
    Let,
    // Types (also used as type constructors in Synthetic TypeRefs)
    Prod,
    // Multi-sort
    Identifier(String),
    // Projector system
    Proj,
    Structural,
    Collapsed,
    Labeled,
    Canvas,
    PosNil,
    PosCons,
    // Cursor
    Cursor,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Sort {
    Type,
    Pattern,
    Expression,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum GroveConstructor {
    Root,
    Lang(Constructor),
}

impl Constructor {
    pub fn arity(&self) -> u8 {
        use Constructor::*;
        match self {
            Typ | Num | Zero | Identifier(_) | Structural | Collapsed | PosNil => 0,
            Labeled | Canvas => 1,
            Plus | Prod | Pair | Arrow | Fun | Asc | Ap | Proj | Cursor => 2,
            Let => 3,
            PosCons => 4,
        }
    }

    /// Sorts in which this constructor is valid.
    pub fn valid_sorts(&self) -> &'static [Sort] {
        use Constructor::*;
        match self {
            Typ | Num | Prod | Arrow => &[Sort::Type],
            Zero | Plus | Fun | Ap | Let => &[Sort::Expression],
            Pair | Asc | Identifier(_) => &[Sort::Expression, Sort::Pattern],
            // Transparent wrappers and metadata have no sort constraint
            _ => &[],
        }
    }

    /// Expected sort of the child at `position`, given the parent's own sort.
    pub fn child_sort(&self, position: u8, parent_sort: Option<&Sort>) -> Option<Sort> {
        use Constructor::*;
        match (self, position) {
            (Plus, 0 | 1) => Some(Sort::Expression),
            (Ap, 0 | 1) => Some(Sort::Expression),
            (Prod, 0 | 1) | (Arrow, 0 | 1) => Some(Sort::Type),
            (Pair, 0 | 1) => parent_sort.cloned(),
            (Fun, 0) => Some(Sort::Pattern),
            (Fun, 1) => Some(Sort::Expression),
            (Asc, 0) => parent_sort.cloned(),
            (Asc, 1) => Some(Sort::Type),
            (Let, 0) => Some(Sort::Pattern),
            (Let, 1 | 2) => Some(Sort::Expression),
            // Transparent wrappers: position 0 is metadata (no sort), position 1 inherits
            (Proj | Cursor, 0) => None,
            (Proj | Cursor, 1) => parent_sort.cloned(),
            _ => None,
        }
    }

    /// True for Proj and Cursor — type-transparent wrappers.
    pub fn is_transparent(&self) -> bool {
        matches!(self, Constructor::Proj | Constructor::Cursor)
    }

    /// Display string for rendering.
    pub fn display_name(&self) -> &str {
        use Constructor::*;
        match self {
            Typ => "Typ",
            Num => "Num",
            Zero => "Zero",
            Plus => "Plus",
            Prod => "Prod",
            Pair => "Pair",
            Arrow => "Arrow",
            Fun => "Fun",
            Asc => "Asc",
            Ap => "Ap",
            Let => "Let",
            Identifier(_) => "Identifier",
            Proj => "Proj",
            Structural => "Structural",
            Collapsed => "Collapsed",
            Labeled => "Labeled",
            Canvas => "Canvas",
            PosNil => "PosNil",
            PosCons => "PosCons",
            Cursor => "Cursor",
        }
    }
}

impl GroveConstructor {
    pub fn arity(&self) -> u8 {
        match self {
            GroveConstructor::Root => 1,
            GroveConstructor::Lang(c) => c.arity(),
        }
    }

    pub fn constructor(&self) -> Option<&Constructor> {
        match self {
            GroveConstructor::Root => None,
            GroveConstructor::Lang(c) => Some(c),
        }
    }

    pub fn child_sort(&self, position: u8, parent_sort: Option<&Sort>) -> Option<Sort> {
        match self {
            GroveConstructor::Root => {
                if position == 0 {
                    Some(Sort::Expression)
                } else {
                    None
                }
            }
            GroveConstructor::Lang(c) => c.child_sort(position, parent_sort),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arities() {
        assert_eq!(Constructor::Typ.arity(), 0);
        assert_eq!(Constructor::Plus.arity(), 2);
        assert_eq!(Constructor::Let.arity(), 3);
        assert_eq!(Constructor::PosCons.arity(), 4);
        assert_eq!(Constructor::Identifier("x".into()).arity(), 0);
        assert_eq!(GroveConstructor::Root.arity(), 1);
    }

    #[test]
    fn test_sort_validity() {
        assert!(Constructor::Num.valid_sorts().contains(&Sort::Type));
        assert!(!Constructor::Num.valid_sorts().contains(&Sort::Expression));
        assert!(Constructor::Identifier("x".into()).valid_sorts().contains(&Sort::Expression));
        assert!(Constructor::Identifier("x".into()).valid_sorts().contains(&Sort::Pattern));
    }

    #[test]
    fn test_child_sorts() {
        assert_eq!(
            Constructor::Fun.child_sort(0, Some(&Sort::Expression)),
            Some(Sort::Pattern)
        );
        assert_eq!(
            Constructor::Fun.child_sort(1, Some(&Sort::Expression)),
            Some(Sort::Expression)
        );
        // Pair inherits parent sort
        assert_eq!(
            Constructor::Pair.child_sort(0, Some(&Sort::Pattern)),
            Some(Sort::Pattern)
        );
        // Transparent: position 0 has no sort
        assert_eq!(Constructor::Proj.child_sort(0, Some(&Sort::Expression)), None);
        // Transparent: position 1 inherits
        assert_eq!(
            Constructor::Proj.child_sort(1, Some(&Sort::Expression)),
            Some(Sort::Expression)
        );
    }

    #[test]
    fn test_serde_constructor() {
        assert_eq!(serde_json::to_string(&Constructor::Num).unwrap(), "\"Num\"");
        assert_eq!(
            serde_json::to_string(&Constructor::Identifier("x".into())).unwrap(),
            "{\"Identifier\":\"x\"}"
        );
        assert_eq!(
            serde_json::to_string(&GroveConstructor::Root).unwrap(),
            "\"Root\""
        );
        assert_eq!(
            serde_json::to_string(&GroveConstructor::Lang(Constructor::Num)).unwrap(),
            "{\"Lang\":\"Num\"}"
        );
    }
}
