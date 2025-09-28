use std::process::Child;
use std::rc::Rc;
use std::collections::HashMap;
use std::vec;

use crate::blossom::TermLocation;
use crate::grove;
use crate::lang;
use crate::forest;

use forest::Term;
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

pub fn correct_type(forest : &forest::State, type_map : &HashMap<TermSite, TypeAttribute>, s : TermSite) -> (TypeAttribute, Vec<TermSite>) {
    match s {
        TermSite::Location(tl) => {
            default()
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
                        lang::Constructor::Num => default(),
                        lang::Constructor::Zero => {
                            let a = TypeAttribute {
                                syn : Some(TypeLocation::Synthetic( SyntheticType { constructor: lang::Constructor::Num, children: vec![] })),
                                ana : ana_of_parent(type_map, parent),
                                marks : vec![]
                            };
                            (a, dirty_parent(parent))
                        },
                        lang::Constructor::Plus=> default(),
                        lang::Constructor::Pair=> default(),
                        lang::Constructor::Fun=> default(),
                        lang::Constructor::Ap=> default(),
                        lang::Constructor::Let=> default(),
                        lang::Constructor::Identifier(_x)=> default(),
                    }
                }
            }
        }
    }
}