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

#[derive(PartialEq, Clone, Serialize, Deserialize)]
pub enum Sort {
    Type,
    Pattern,
    Expression
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct SyntheticType {
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


#[derive(PartialEq, Clone, Serialize, Deserialize)]
pub enum Mark {
    Wrong,
    Bad,
    Silly,
    Dumb,
    SortInconsistent(Sort, Sort),
    TypeInconsistent(TypeLocation, TypeLocation)
}

#[derive(PartialEq)]
pub struct TypeAttribute {
    pub sort : Option<Sort>,
    pub syn: Option<TypeLocation>, 
    pub ana: Option<TypeLocation>, 
    pub marks : Vec<Mark>
}

impl TypeAttribute {
    pub fn new() -> TypeAttribute {
        TypeAttribute { sort: None, syn: None, ana: None, marks: vec![] }
    }

    // used to stop propagation. could improve using hashing.
    pub fn equivalent(self : &TypeAttribute, t : &TypeAttribute) -> bool {
        self == t
    }
}

pub fn constructor_of_type(forest : &forest::State, t : &Type) -> forest::Constructor {
    match t {
        Type::Synthetic(t) => forest::Constructor::Constructor(grove::Constructor::Lang(t.constructor.clone())),
        Type::Surface(t) => forest.constructor_of_term(t),
    }
}

pub fn constructor_of_type_location(forest : &forest::State, t : &TypeLocation) -> Option<forest::Constructor> {
    match t {
        TypeLocation::Unknown => None,
        TypeLocation::Synthetic(t) => Some(forest::Constructor::Constructor(grove::Constructor::Lang(t.constructor.clone()))),
        TypeLocation::Surface(tl) => {
            let c = forest.children_of_term_location(tl);
            if c.len() != 1 { None } else {
                Some(forest.constructor_of_term(&c[0]))
            }
        },
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

fn get_ana(type_map : &HashMap<TermSite, TypeAttribute>, s : &TermSite) -> (Option<Sort>, Option<TypeLocation>) {
    match type_map.get(s) {
        None => (None, None), 
        Some(a) => (a.sort.clone(), a.ana.clone())
    }
}

fn ana_of_parent(type_map : &HashMap<TermSite, TypeAttribute>, parent : Option<TermLocation>) -> (Option<Sort>, Option<TypeLocation>) {
    match parent {
        None => (None, None),
        Some(parent) => get_ana(type_map, &TermSite::Location(parent))
    }
}

fn get_syn(type_map : &HashMap<TermSite, TypeAttribute>, s : &TermSite) -> Option<TypeLocation> {
    type_map.get(s).and_then(|a| a.syn.clone())
}

fn const_type(c : lang::Constructor) -> TypeLocation{
    TypeLocation::Synthetic( SyntheticType { constructor: c, children: vec![] })
}

fn match_bin_constructor(forest : &forest::State, c : lang::Constructor, t : Option<TypeLocation>) -> (Option<TypeLocation>, Option<TypeLocation>) {
    let default = (Some(TypeLocation::Unknown), Some(TypeLocation::Unknown));
    match t {
        None => (None, None),
        Some(TypeLocation::Unknown) => default,
        Some(TypeLocation::Synthetic(t)) => {
            if t.constructor == c {
                let cs = t.children;
                (Some(cs[0].clone()), Some(cs[1].clone()))
            } else { default }
        },
        Some(TypeLocation::Surface(tl)) => {
            let ts = forest.children_of_term_location(&tl);
            if ts.len() != 1 { return default };
            let child = ts[0];
            match forest.constructor_of_term(&child) {
                Constructor::Constructor(grove::Constructor::Lang(found_c)) if found_c == c => {
                    let cs = forest.children_of_term(&child);
                    (Some(TypeLocation::Surface(cs[0])), Some(TypeLocation::Surface(cs[1])))
                },
                _ => default
            }
        }
    }
}

fn match_prod(forest : &forest::State, t : Option<TypeLocation>) -> (Option<TypeLocation>, Option<TypeLocation>) {
    match_bin_constructor(forest, lang::Constructor::Prod, t)
}

fn match_arrow(forest : &forest::State, t : Option<TypeLocation>) -> (Option<TypeLocation>, Option<TypeLocation>) {
    match_bin_constructor(forest, lang::Constructor::Arrow, t)
}

fn compute_ana(forest : &forest::State, type_map : &HashMap<TermSite, TypeAttribute>, term_sort : Option<Sort>, term_ana : Option<TypeLocation>, term_node : TermNode, term_constructor : Constructor, position : Position) -> (Option<Sort>, Option<TypeLocation>) {
    match term_constructor {
        Constructor::Reference(_) => panic!("impossible: nullary term with child location"),
        Constructor::Constructor(grove::Constructor::Root) => (Some(Sort::Expression), Some(TypeLocation::Unknown)),
        Constructor::Constructor(grove::Constructor::Lang(c)) => {
            match c {
                lang::Constructor::Typ |
                lang::Constructor::Num |
                lang::Constructor::Zero |
                lang::Constructor::Identifier(_) => panic!("impossible: nullary term with child location"),
                lang::Constructor::Prod => (Some(Sort::Type), Some(const_type(lang::Constructor::Typ))),
                lang::Constructor::Plus => (Some(Sort::Expression), Some(const_type(lang::Constructor::Num))),
                lang::Constructor::Pair => {
                    let ts = match_prod(forest, term_ana);
                    let ana = if position == 0 { ts.0 } else { ts.1 };
                    let sort = match term_sort {
                        None => None,
                        Some(Sort::Type) => None,
                        Some(Sort::Pattern) => Some(Sort::Pattern),
                        Some(Sort::Expression) => Some(Sort::Expression),
                    };
                    (sort, ana)
                },
                lang::Constructor::Arrow => (Some(Sort::Type), Some(const_type(lang::Constructor::Typ))),
                lang::Constructor::Fun => {
                    let ts = match_arrow(forest, term_ana);
                    if position == 0 { (Some(Sort::Pattern), ts.0) } else { (Some(Sort::Expression), ts.1) }
                },
                lang::Constructor::Asc => {
                    if position == 0 {
                        let sort = match term_sort {
                            None => None,
                            Some(Sort::Type) => None,
                            Some(Sort::Pattern) => Some(Sort::Pattern),
                            Some(Sort::Expression) => Some(Sort::Expression),
                        };
                        (sort, Some(TypeLocation::Surface(TermLocation {node : term_node, position : 1})))
                    } else {
                        (Some(Sort::Type), Some(const_type(lang::Constructor::Typ)))
                    }
                },
                lang::Constructor::Ap => {
                    if position == 0 { 
                        let ana = match term_ana {
                            Some(ana) => ana, 
                            None => TypeLocation::Unknown
                        };
                        (Some(Sort::Expression), Some(TypeLocation::Synthetic(SyntheticType { constructor: lang::Constructor::Arrow, children: vec![TypeLocation::Unknown, ana] })))
                    }
                    else { 
                        let syn1 = get_syn(type_map, &TermSite::Location(TermLocation {node : term_node, position : 0}));
                        (Some(Sort::Expression), match_arrow(forest, syn1).0)
                    }
                },
                lang::Constructor::Let => {
                    if position == 0 { (Some(Sort::Pattern), Some(TypeLocation::Unknown)) }
                    else if position == 1 { 
                        (Some(Sort::Expression), get_syn(type_map, &TermSite::Location(TermLocation {node : term_node, position : 0})))
                    }
                    else { (Some(Sort::Expression), term_ana) }
                },
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

fn resovle_sort(expected_sort : Option<Sort>, allowed_sorts : Vec<Sort>) -> (Sort, Vec<Mark>) {
    match expected_sort {
        None => (allowed_sorts[0].clone(), vec![]),
        Some(expected_sort) => 
            if allowed_sorts.contains(&expected_sort) {
                (expected_sort, vec![])
            } else {
                let sort = allowed_sorts[0].clone();
                (sort.clone(), vec![Mark::SortInconsistent(expected_sort, sort)])
            }
    }
}

fn consistent_type(forest : &forest::State, ana : &Type, syn : &Type) -> bool {
    let c1 = constructor_of_type(forest, ana);
    let c2 = constructor_of_type(forest, syn);
    if c1 != c2 { false } else {
        let children1 = children_of_type(forest, ana);
        let children2 = children_of_type(forest, syn);
        if children1.len() != children2.len() { false } else {
            children1.iter().zip(children2.iter()).all(|(c1, c2)| consistent(forest, c1, c2))
        }
    }
}

fn consistent(forest : &forest::State, ana : &TypeLocation, syn : &TypeLocation) -> bool {
        let c1 = constructor_of_type_location(forest, ana);
        let c2 = constructor_of_type_location(forest, syn);
        match (c1, c2) {
            (None, _) => true,
            (_, None) => true,
            (Some(c1), Some(c2)) => {
                if c1 != c2 { false } else {
                    let children1 = children_of_type_location(forest, ana);
                    let children2 = children_of_type_location(forest, syn);
                    if children1.len() != children2.len() { false } else {
                        children1.iter().zip(children2.iter()).all(|(c1, c2)| consistent_type(forest, c1, c2))
                    }
                }
            }
        }
}

fn consist_marks(forest : &forest::State, ana : &Option<TypeLocation>, syn : &Option<TypeLocation>) -> Vec<Mark> {
    match (ana, syn) {
        (None, _) => vec![],
        (_, None) => vec![],
        (Some(ana), Some(syn)) => {
            if consistent(forest, &ana, &syn) {
                vec![]
            } else {
                vec![Mark::TypeInconsistent(ana.clone(), syn.clone())]
            }
        }
    }
}

fn compute_syn(forest : &forest::State, c : lang::Constructor, t : Term, expected_sort : Option<Sort>, children_syns : Vec<Option<TypeLocation>>) -> (Vec<Sort>, Option<TypeLocation>) {
    match c {
        lang::Constructor::Typ => (vec![Sort::Type], Some(const_type(lang::Constructor::Typ))),
        lang::Constructor::Num => (vec![Sort::Type], Some(const_type(lang::Constructor::Typ))),
        lang::Constructor::Zero => (vec![Sort::Expression], Some(const_type(lang::Constructor::Num))),
        lang::Constructor::Plus => (vec![Sort::Expression], Some(const_type(lang::Constructor::Num))),
        lang::Constructor::Prod => (vec![Sort::Type], Some(const_type(lang::Constructor::Typ))),
        lang::Constructor::Pair => {
            let syn = 
                if let (Some(syn1), Some(syn2)) = (&children_syns[0], &children_syns[1]) {
                    Some(TypeLocation::Synthetic(SyntheticType {
                        constructor: lang::Constructor::Prod, 
                        children: vec![syn1.clone(), syn2.clone()]
                    }))
                } else { None };
            (vec![Sort::Expression, Sort::Pattern], syn)
        },
        lang::Constructor::Arrow => (vec![Sort::Type], Some(const_type(lang::Constructor::Typ))),
        lang::Constructor::Fun => {
            let syn = 
                if let (Some(syn1), Some(syn2)) = (&children_syns[0], &children_syns[1]) {
                    Some(TypeLocation::Synthetic(SyntheticType {
                        constructor: lang::Constructor::Arrow,
                        children: vec![syn1.clone(), syn2.clone()]
                    }))
                } else { None };
            (vec![Sort::Expression], syn)
        },
        lang::Constructor::Ap => (vec![Sort::Expression], match_arrow(forest, children_syns[0].clone()).1), 
        lang::Constructor::Asc => (vec![Sort::Expression, Sort::Pattern], Some(TypeLocation::Surface(TermLocation {node: term_node_of_term(t), position: 1}))), 
        lang::Constructor::Let => (vec![Sort::Expression], children_syns[2].clone()),
        lang::Constructor::Identifier(_) => {
            let syn = 
                if expected_sort == Some(Sort::Pattern) { 
                    Some(TypeLocation::Unknown)
                } else {  None };
            (vec![Sort::Expression, Sort::Pattern], syn)
        }, 
    }
}

pub fn correct_type(forest : &forest::State, type_map : &HashMap<TermSite, TypeAttribute>, s : TermSite) -> (TypeAttribute, Vec<TermSite>) {
    match s {
        TermSite::Location(tl) => {
            let parent = tl.term();
            let children = forest.children_of_term_location(&tl);

            let (term_sort, term_ana) = get_ana(type_map, &TermSite::Term(parent));
            let term_constructor = forest.constructor_of_term(&parent);
            let term_node = term_node_of_term(parent);
            let (sort, ana) = compute_ana(forest, type_map, term_sort, term_ana, term_node, term_constructor, tl.position);

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
                sort : sort,
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
                    let (sort, ana) = ana_of_parent(type_map, parent);
                    let a = TypeAttribute {
                        sort : sort,
                        syn : Some(TypeLocation::Unknown),
                        ana : ana,
                        marks : vec![]
                    };
                    (a, dirty_parent(parent))
                },
                Constructor::Constructor(GroveConstructor::Root) => {
                    let children = forest.children_of_term(&t);
                    let child = children[0];
                    let syn = get_syn(type_map, &TermSite::Location(child));
                    let a = TypeAttribute {
                        sort : Some(Sort::Expression),
                        syn : syn,
                        ana : Some(TypeLocation::Unknown),
                        marks : vec![]
                    };
                    (a, vec![TermSite::Location(child)])
                }
                Constructor::Constructor(GroveConstructor::Lang(c)) => {
                    let parent = forest.unique_parent_of_term(&t);
                    let (expected_sort, ana) = ana_of_parent(type_map, parent);
                    let children = forest.children_of_term(&t);
                    let mut default_dirties = dirty_children(children.clone());
                    default_dirties.append(&mut dirty_parent(parent));

                    let children_syns = children.iter().map(|child| get_syn(type_map, &TermSite::Location(*child))).collect();
                    let (allowed_sorts, syn) = compute_syn(forest, c, t, expected_sort.clone(), children_syns);
                    let (sort, mut sort_marks) = resovle_sort(expected_sort.clone(), allowed_sorts);
                    let mut consist_marks = consist_marks(forest, &ana, &syn);
                    let mut marks = vec![];
                    marks.append(&mut sort_marks);
                    marks.append(&mut consist_marks);
                    let a = TypeAttribute {
                        sort : Some(sort),
                        syn : syn,
                        ana : ana,
                        marks : marks
                    };
                    (a, default_dirties)
                }
            }
        }
    }
}