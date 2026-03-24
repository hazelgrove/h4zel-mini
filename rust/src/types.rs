use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::grove::Grove;
use crate::lang::{Constructor, Sort};

/// A reference to a type — either a grove node (Surface), a computed type
/// (Synthetic), or the gradual unknown type.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeRef {
    /// Points to a grove node that IS a type (e.g., a user-written annotation).
    Surface(Uuid),
    /// A type computed by the type checker, not in the forest.
    Synthetic(Constructor, Vec<TypeRef>),
    /// The gradual type `?` — consistent with everything.
    Unknown,
}

/// Type attributes tracked per site (term or location).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeAttribute {
    pub sort: Option<Sort>,
    pub ana: Option<TypeRef>,
    pub syn: Option<TypeRef>,
    pub marks: Vec<Mark>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Mark {
    SortInconsistent(Sort, Sort),
    TypeInconsistent(TypeRef, TypeRef),
}

impl Default for TypeAttribute {
    fn default() -> Self {
        TypeAttribute {
            sort: None,
            ana: None,
            syn: None,
            marks: Vec::new(),
        }
    }
}

impl TypeRef {
    // ── Matching ─────────────────────────────────────────────────────────────

    /// Resolve a Surface type by reading the grove structure.
    /// Produces a Synthetic or Unknown type with no Surface references.
    pub fn resolve(&self, grove: &Grove) -> TypeRef {
        match self {
            TypeRef::Surface(node_id) => {
                let node = match grove.node(*node_id) {
                    Some(n) => n,
                    None => return TypeRef::Unknown,
                };
                let constructor = match node.constructor.constructor() {
                    Some(c) => c.clone(),
                    None => return TypeRef::Unknown, // Root — not a type
                };
                // Only type constructors produce meaningful types
                match &constructor {
                    Constructor::Num | Constructor::Typ => {
                        TypeRef::Synthetic(constructor, vec![])
                    }
                    Constructor::Arrow | Constructor::Prod => {
                        let children: Vec<TypeRef> = (0..node.arity)
                            .map(|pos| {
                                let loc = crate::grove::Location {
                                    node: *node_id,
                                    position: pos,
                                };
                                let child_ids = grove.live_children_at(&loc);
                                match child_ids.first() {
                                    Some(&child_id) => TypeRef::Surface(child_id).resolve(grove),
                                    None => TypeRef::Unknown,
                                }
                            })
                            .collect();
                        TypeRef::Synthetic(constructor, children)
                    }
                    // Transparent wrappers: resolve through them
                    c if c.is_transparent() => {
                        let content_loc = crate::grove::Location {
                            node: *node_id,
                            position: 1,
                        };
                        match grove.live_children_at(&content_loc).first() {
                            Some(&child_id) => TypeRef::Surface(child_id).resolve(grove),
                            None => TypeRef::Unknown,
                        }
                    }
                    _ => TypeRef::Unknown,
                }
            }
            TypeRef::Synthetic(c, children) => {
                let resolved: Vec<TypeRef> = children.iter().map(|ch| ch.resolve(grove)).collect();
                TypeRef::Synthetic(c.clone(), resolved)
            }
            TypeRef::Unknown => TypeRef::Unknown,
        }
    }

    /// Decompose as Arrow → (domain, codomain). Returns (Unknown, Unknown) if not an Arrow.
    pub fn match_arrow(&self, grove: &Grove) -> (TypeRef, TypeRef) {
        let resolved = self.resolve(grove);
        match resolved {
            TypeRef::Synthetic(Constructor::Arrow, ref children) if children.len() == 2 => {
                (children[0].clone(), children[1].clone())
            }
            TypeRef::Unknown => (TypeRef::Unknown, TypeRef::Unknown),
            _ => (TypeRef::Unknown, TypeRef::Unknown),
        }
    }

    /// Decompose as Prod → (left, right). Returns (Unknown, Unknown) if not a Prod.
    pub fn match_prod(&self, grove: &Grove) -> (TypeRef, TypeRef) {
        let resolved = self.resolve(grove);
        match resolved {
            TypeRef::Synthetic(Constructor::Prod, ref children) if children.len() == 2 => {
                (children[0].clone(), children[1].clone())
            }
            TypeRef::Unknown => (TypeRef::Unknown, TypeRef::Unknown),
            _ => (TypeRef::Unknown, TypeRef::Unknown),
        }
    }

    // ── Consistency (gradual typing) ─────────────────────────────────────────

    /// Check type consistency. Unknown is consistent with everything.
    pub fn consistent_resolved(&self, other: &TypeRef) -> bool {
        match (self, other) {
            (TypeRef::Unknown, _) | (_, TypeRef::Unknown) => true,
            (TypeRef::Synthetic(c1, ch1), TypeRef::Synthetic(c2, ch2)) => {
                c1 == c2
                    && ch1.len() == ch2.len()
                    && ch1
                        .iter()
                        .zip(ch2.iter())
                        .all(|(a, b)| a.consistent_resolved(b))
            }
            // Surface types should be resolved before consistency check
            _ => true,
        }
    }

    /// Check consistency, resolving Surface types first.
    pub fn consistent(&self, other: &TypeRef, grove: &Grove) -> bool {
        self.resolve(grove).consistent_resolved(&other.resolve(grove))
    }

    // ── Display ──────────────────────────────────────────────────────────────

    pub fn display(&self, grove: &Grove) -> String {
        let resolved = self.resolve(grove);
        match resolved {
            TypeRef::Unknown => "?".to_string(),
            TypeRef::Surface(_) => "?".to_string(), // shouldn't happen after resolve
            TypeRef::Synthetic(ref c, ref children) => match c {
                Constructor::Num => "ℕ".to_string(),
                Constructor::Typ => "□".to_string(),
                Constructor::Arrow if children.len() == 2 => {
                    format!(
                        "({} → {})",
                        children[0].display(grove),
                        children[1].display(grove)
                    )
                }
                Constructor::Prod if children.len() == 2 => {
                    format!(
                        "({} × {})",
                        children[0].display(grove),
                        children[1].display(grove)
                    )
                }
                _ => format!("{}", c.display_name()),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consistency() {
        let num = TypeRef::Synthetic(Constructor::Num, vec![]);
        let unk = TypeRef::Unknown;
        let arrow = TypeRef::Synthetic(
            Constructor::Arrow,
            vec![num.clone(), num.clone()],
        );

        assert!(num.consistent_resolved(&num));
        assert!(num.consistent_resolved(&unk));
        assert!(unk.consistent_resolved(&arrow));
        assert!(!num.consistent_resolved(&arrow));
    }

    #[test]
    fn test_match_arrow() {
        let grove = Grove::new();
        let num = TypeRef::Synthetic(Constructor::Num, vec![]);
        let arrow = TypeRef::Synthetic(
            Constructor::Arrow,
            vec![num.clone(), TypeRef::Unknown],
        );

        let (dom, cod) = arrow.match_arrow(&grove);
        assert_eq!(dom, num);
        assert_eq!(cod, TypeRef::Unknown);

        // Non-arrow returns Unknown
        let (dom, cod) = num.match_arrow(&grove);
        assert_eq!(dom, TypeRef::Unknown);
        assert_eq!(cod, TypeRef::Unknown);
    }

    #[test]
    fn test_display() {
        let grove = Grove::new();
        assert_eq!(TypeRef::Unknown.display(&grove), "?");
        assert_eq!(
            TypeRef::Synthetic(Constructor::Num, vec![]).display(&grove),
            "ℕ"
        );
        assert_eq!(
            TypeRef::Synthetic(
                Constructor::Arrow,
                vec![
                    TypeRef::Synthetic(Constructor::Num, vec![]),
                    TypeRef::Synthetic(Constructor::Num, vec![]),
                ]
            )
            .display(&grove),
            "(ℕ → ℕ)"
        );
    }
}
