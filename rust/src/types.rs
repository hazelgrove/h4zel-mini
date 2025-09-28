use std::process::Child;
use std::rc::Rc;
use std::collections::HashMap;
use std::vec;

use crate::blossom::TermLocation;
use crate::grove;
use crate::lang;
use crate::forest;
use crate::lang::Position;

use forest::Term;
use forest::TermNode;
use forest::TermSite;
use forest::Constructor;
use forest::GroveConstructor;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone, PartialEq, Serialize, Deserialize)]
struct SyntheticType {
    constructor : lang::Constructor,
    children : Vec<TypeLocation>
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Surface(Term),
    Synthetic(SyntheticType)
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeLocation {
    Unknown,
    Surface(TermLocation),
    Synthetic(SyntheticType),
}


#[derive(PartialEq)]
enum Mark {
    Wrong,
    Bad,
    Silly,
    Dumb
}

#[derive(PartialEq)]
pub struct TypeAttribute {
    pub syn: Option<TypeLocation>, 
    pub ana: Option<TypeLocation>, 
    pub marks : Vec<Mark>
}

impl TypeAttribute {
    pub fn new() -> TypeAttribute {
        TypeAttribute { syn: None, ana: None, marks: vec![] }
    }
}

pub fn constructor_of_type(forest : &forest::State, t : &Type) -> forest::Constructor {
    match t {
        Type::Synthetic(t) => forest::Constructor::Constructor(grove::Constructor::Lang(t.constructor.clone())),
        Type::Surface(t) => forest.constructor_of_term(t),
    }
}

pub fn children_of_type_location(forest : &forest::State, tl : &TypeLocation) -> Vec<Type> {
    match tl {
        TypeLocation::Unknown => vec![],
        TypeLocation::Synthetic(t) => vec![Type::Synthetic(t.clone())],
        TypeLocation::Surface(tl) => { forest.children_of_term_location(tl).iter().map(|c| Type::Surface(*c)).collect() },
    }
}

pub fn children_of_type(forest : &forest::State, t : &Type) -> Vec<TypeLocation> {
    match t {
        Type::Surface(t) => { forest.children_of_term(t).iter().map(|tl| TypeLocation::Surface(*tl)).collect() }, 
        Type::Synthetic(t) => t.children.clone(),
    }
}

fn dirty_parent(parent : Option<TermLocation>) -> Vec<TermSite> {
    match parent {
        None => vec![],
        Some(tl) => vec![TermSite::Location(tl)]
    }
}

fn dirty_children(children : Vec<TermLocation>) -> Vec<TermSite> {
    children.iter().map(|child| TermSite::Location(*child)).collect()
}

fn get_ana(type_map : &HashMap<TermSite, TypeAttribute>, s : &TermSite) -> Option<TypeLocation> {
    type_map.get(s).and_then(|a| a.ana.clone())
}

fn get_syn(type_map : &HashMap<TermSite, TypeAttribute>, s : &TermSite) -> Option<TypeLocation> {
    type_map.get(s).and_then(|a| a.syn.clone())
}

fn ana_of_parent(type_map : &HashMap<TermSite, TypeAttribute>, parent : Option<TermLocation>) -> Option<TypeLocation> {
    let parent = parent?;
    get_ana(type_map, &TermSite::Location(parent))
}

fn default() -> (TypeAttribute, Vec<TermSite>) {
    (TypeAttribute::new(), vec![])
}

fn num() -> TypeLocation{
    TypeLocation::Synthetic( SyntheticType { constructor: lang::Constructor::Num, children: vec![] })
}

fn match_prod(forest : &forest::State, t : Option<TypeLocation>) -> (Option<TypeLocation>, Option<TypeLocation>) {
    let default = (Some(TypeLocation::Unknown), Some(TypeLocation::Unknown));
    match t {
        None => (None, None),
        Some(TypeLocation::Unknown) => default,
        Some(TypeLocation::Synthetic(t)) => {
            match t.constructor {
                lang::Constructor::Prod => {
                    let cs = t.children;
                    (Some(cs[0].clone()), Some(cs[1].clone()))
                },
                _ => default,
            }
        },
        Some(TypeLocation::Surface(tl)) => {
            let ts = forest.children_of_term_location(&tl);
            if ts.len() != 1 { return default };
            let child = ts[0];
            match forest.constructor_of_term(&child) {
                Constructor::Constructor(grove::Constructor::Lang(lang::Constructor::Prod)) => {
                    let cs = forest.children_of_term(&child);
                    (Some(TypeLocation::Surface(cs[0])), Some(TypeLocation::Surface(cs[1])))
                },
                _ => default
            }
        }
    }
}

fn compute_ana(forest : &forest::State, term_ana : Option<TypeLocation>, term_node : TermNode, term_constructor : Constructor, position : Position) -> Option<TypeLocation> {
    match term_constructor {
        Constructor::Reference(_) => panic!("impossible: nullary term with child location"),
        Constructor::Constructor(grove::Constructor::Root) => Some(TypeLocation::Unknown),
        Constructor::Constructor(grove::Constructor::Lang(c)) => {
            match c {
                lang::Constructor::Num |
                lang::Constructor::Zero |
                lang::Constructor::Identifier(_) => panic!("impossible: nullary term with child location"),
                lang::Constructor::Prod => Some(TypeLocation::Unknown),
                lang::Constructor::Plus => Some(num()),
                lang::Constructor::Pair => {
                    let ts = match_prod(forest, term_ana);
                    if position == 0 { ts.0 } else { ts.1 }
                },
                lang::Constructor::Fun => Some(TypeLocation::Unknown),
                lang::Constructor::Asc => {
                    if position == 0 {
                        Some(TypeLocation::Surface(TermLocation {node : term_node, position : 1}))
                    } else {
                        Some(TypeLocation::Unknown)
                    }
                },
                lang::Constructor::Ap => Some(TypeLocation::Unknown),
                lang::Constructor::Let => Some(TypeLocation::Unknown),
            }
        }
    }
}

fn term_node_of_term(term : Term) -> TermNode {
    match term {
        Term::Node(n) => n,
        Term::Reference(_) => panic!("impossible: nullary term with child location"),
    }
}

pub fn correct_type(forest : &forest::State, type_map : &HashMap<TermSite, TypeAttribute>, s : TermSite) -> (TypeAttribute, Vec<TermSite>) {
    match s {
        TermSite::Location(tl) => {
            let parent = tl.term();
            let children = forest.children_of_term_location(&tl);

            let term_ana = get_ana(type_map, &TermSite::Term(parent));
            let term_constructor = forest.constructor_of_term(&parent);
            let term_node = term_node_of_term(parent);
            let ana = compute_ana(forest, term_ana, term_node, term_constructor, tl.position);

            // this could be join
            let syn = if children.len() == 1 {
                get_syn(type_map, &TermSite::Term(children[0]))
            } else {
                Some(TypeLocation::Unknown)
            };

            let mut dirties = vec![TermSite::Term(parent)];
            for child in children {
                dirties.push(TermSite::Term(child));
            }
            
            let a = TypeAttribute {
                syn : syn,
                ana : ana,
                marks : vec![]
            };
            (a, dirties)
        },
        TermSite::Term(t) => {
            match forest.constructor_of_term(&t) {
                Constructor::Reference(_) => { 
                    let parent = forest.unique_parent_of_term(&t);
                    let a = TypeAttribute {
                        syn : Some(TypeLocation::Unknown),
                        ana : ana_of_parent(type_map, parent),
                        marks : vec![]
                    };
                    (a, dirty_parent(parent))
                },
                Constructor::Constructor(GroveConstructor::Root) => {
                    let children = forest.children_of_term(&t);
                    let child = children[0];
                    let syn = get_syn(type_map, &TermSite::Location(child));
                    let a = TypeAttribute {
                        syn : syn,
                        ana : Some(TypeLocation::Unknown),
                        marks : vec![]
                    };
                    (a, vec![TermSite::Location(child)])
                }
                Constructor::Constructor(GroveConstructor::Lang(c)) => {
                    let parent = forest.unique_parent_of_term(&t);
                    match c {
                        lang::Constructor::Num => {
                            default()
                        }
                        lang::Constructor::Zero => {
                            let a = TypeAttribute {
                                syn : Some(num()),
                                ana : ana_of_parent(type_map, parent),
                                marks : vec![]
                            };
                            (a, dirty_parent(parent))
                        },
                        lang::Constructor::Plus => {
                            let mut dirties = dirty_children(forest.children_of_term(&t));
                            dirties.append(&mut dirty_parent(parent));
                            let a = TypeAttribute {
                                syn : Some(num()),
                                ana : ana_of_parent(type_map, parent),
                                marks : vec![]
                            };
                            (a, dirties)
                        },
                        lang::Constructor::Prod => default(),
                        lang::Constructor::Pair => {
                            let mut dirties = dirty_children(forest.children_of_term(&t));
                            dirties.append(&mut dirty_parent(parent));
                            let children = forest.children_of_term(&t);
                            let syn = 
                                if let (Some(syn1), Some(syn2)) = (
                                    get_syn(type_map, &TermSite::Location(children[0])),
                                    get_syn(type_map, &TermSite::Location(children[1]))
                                ) {
                                    let type_children : Vec<TypeLocation> = vec![ syn1, syn2 ];
                                    Some(TypeLocation::Synthetic(SyntheticType {constructor : lang::Constructor::Prod, children : type_children}))
                                } else {
                                    None
                                };
                            let a = TypeAttribute {
                                syn : syn,
                                ana : ana_of_parent(type_map, parent),
                                marks : vec![]
                            };
                            (a, dirties)
                        },
                        lang::Constructor::Fun => default(),
                        lang::Constructor::Ap => default(),
                        lang::Constructor::Asc => {
                            let mut dirties = dirty_children(forest.children_of_term(&t));
                            dirties.append(&mut dirty_parent(parent));
                            let a = TypeAttribute {
                                syn : Some(TypeLocation::Surface(TermLocation {node: term_node_of_term(t), position: 1})),
                                ana : ana_of_parent(type_map, parent),
                                marks : vec![]
                            };
                            (a, dirties)
                        },
                        lang::Constructor::Let => default(),
                        lang::Constructor::Identifier(_x) => default(),
                    }
                }
            }
        }
    }
}