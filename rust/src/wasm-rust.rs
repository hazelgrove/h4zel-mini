mod lang;
mod grove;
mod forest;
mod blossom;
mod order;
mod types;

use serde_wasm_bindgen;
use serde::Serialize;
use serde::de::DeserializeOwned;
use wasm_bindgen::prelude::*;
use js_sys::Array;

extern crate console_error_panic_hook;
use std::panic;

#[wasm_bindgen]
pub struct WasmState {
    blossom: blossom::State,
}

#[wasm_bindgen]
impl WasmState {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmState {
        panic::set_hook(Box::new(console_error_panic_hook::hook));
        WasmState {
            blossom: blossom::State::new(),
        }
    }

    // These helpers panic on serialization errors. This is intentional:
    // - TypeScript is the authoritative source; malformed data indicates a TS bug
    // - console_error_panic_hook provides clear error messages
    // - For production, consider returning Result<JsValue, JsError> instead

    fn from_js<T: DeserializeOwned>(v: JsValue) -> T {
        serde_wasm_bindgen::from_value(v).unwrap()
    }

    fn to_js<T: Serialize>(t: &T) -> JsValue {
        serde_wasm_bindgen::to_value(t).unwrap()
    }

    // =====================================================
    // Patch application
    // =====================================================

    pub fn apply_patch(&mut self, patchjs: JsValue) {
        let patch = Self::from_js(patchjs);
        self.blossom.apply_patch(patch)
    }

    // =====================================================
    // Blossom actions (UpdateStep, OpenReference, etc.)
    // =====================================================

    pub fn apply_blossom_action(&mut self, action_js: JsValue) {
        let action: blossom::Action = Self::from_js(action_js);
        self.blossom.apply_action(action);
    }

    // =====================================================
    // Tree structure queries
    // =====================================================

    pub fn root_location(&self) -> JsValue {
        Self::to_js(&self.blossom.root_term_location())
    }

    pub fn constructor_of_term(&self, t: JsValue) -> JsValue {
        let t: blossom::Term = Self::from_js(t);
        let c = self.blossom.constructor_of_term(&t);
        Self::to_js(&c)
    }

    pub fn children_of_term(&self, tjs: JsValue) -> Array {
        let t: blossom::Term = Self::from_js(tjs);
        let cs = self.blossom.children_of_term(&t);
        let array = Array::new();
        for l in cs { array.push(&Self::to_js(&l)); }
        array
    }

    pub fn children_of_location(&self, tljs: JsValue) -> Array {
        let tl: blossom::TermLocation = Self::from_js(tljs);
        let ts = self.blossom.children_of_term_location(&tl);
        let array = Array::new();
        for t in ts.iter() { array.push(&Self::to_js(t)); }
        array
    }

    // =====================================================
    // Type system queries
    // =====================================================

    pub fn ana_of_term(&self, tjs: JsValue) -> JsValue {
        let t: blossom::Term = Self::from_js(tjs);
        let ana = self.blossom.types_of_site(&forest::TermSite::Term(t)).and_then(|a| a.ana.clone());
        Self::to_js(&ana)
    }

    pub fn syn_of_term(&self, tjs: JsValue) -> JsValue {
        let t: blossom::Term = Self::from_js(tjs);
        let syn = self.blossom.types_of_site(&forest::TermSite::Term(t)).and_then(|a| a.syn.clone());
        Self::to_js(&syn)
    }

    pub fn ana_of_location(&self, tjs: JsValue) -> JsValue {
        let t: blossom::TermLocation = Self::from_js(tjs);
        let ana = self.blossom.types_of_site(&forest::TermSite::Location(t)).and_then(|a| a.ana.clone());
        Self::to_js(&ana)
    }

    pub fn syn_of_location(&self, tjs: JsValue) -> JsValue {
        let t: blossom::TermLocation = Self::from_js(tjs);
        let syn = self.blossom.types_of_site(&forest::TermSite::Location(t)).and_then(|a| a.syn.clone());
        Self::to_js(&syn)
    }

    pub fn marks_of_term(&self, tjs: JsValue) -> JsValue {
        let t: blossom::Term = Self::from_js(tjs);
        let marks = self.blossom.types_of_site(&forest::TermSite::Term(t)).map(|a| a.marks.clone());
        Self::to_js(&marks)
    }

    pub fn sort_of_term(&self, tjs: JsValue) -> JsValue {
        let t: blossom::Term = Self::from_js(tjs);
        let sort = self.blossom.types_of_site(&forest::TermSite::Term(t)).map(|a| a.sort.clone());
        Self::to_js(&sort)
    }

    pub fn constructor_of_type(&self, tjs: JsValue) -> JsValue {
        let t: types::Type = Self::from_js(tjs);
        let c = self.blossom.constructor_of_type(&t);
        Self::to_js(&c)
    }

    pub fn children_of_type(&self, tjs: JsValue) -> Array {
        let t: types::Type = Self::from_js(tjs);
        let tcs: Vec<types::TypeLocation> = self.blossom.children_of_type(&t);
        let array = Array::new();
        for tc in tcs { array.push(&Self::to_js(&tc)); }
        array
    }

    pub fn children_of_type_location(&self, tjs: JsValue) -> Array {
        let t: types::TypeLocation = Self::from_js(tjs);
        let tcs: Vec<types::Type> = self.blossom.children_of_type_location(&t);
        let array = Array::new();
        for tc in tcs { array.push(&Self::to_js(&tc)); }
        array
    }

    pub fn is_dirty_term(&self, tjs: JsValue) -> bool {
        let t: blossom::Term = Self::from_js(tjs);
        self.blossom.is_dirty(&forest::TermSite::Term(t))
    }

    pub fn is_dirty_location(&self, tjs: JsValue) -> bool {
        let t: blossom::TermLocation = Self::from_js(tjs);
        self.blossom.is_dirty(&forest::TermSite::Location(t))
    }

    // =====================================================
    // Constructor queries
    // =====================================================

    pub fn arity_of_constructor(&self, constructor_js: JsValue) -> u8 {
        let constructor: lang::Constructor = Self::from_js(constructor_js);
        constructor.arity()
    }

    // =====================================================
    // Patch creation
    // =====================================================

    pub fn new_patch_node(&self, constructor_js: JsValue) -> JsValue {
        let constructor: lang::Constructor = Self::from_js(constructor_js);
        let pn = grove::PatchNode::new(constructor);
        Self::to_js(&pn)
    }

    pub fn new_patch_location(&self, node_js: JsValue, position: u8) -> JsValue {
        let node: grove::PatchNode = Self::from_js(node_js);
        let pl = grove::PatchLocation::new(node, position);
        Self::to_js(&pl)
    }

    pub fn connection_patch(&self, source_js: JsValue, destination_js: JsValue) -> JsValue {
        let source: blossom::PatchLocation = Self::from_js(source_js);
        let destination: blossom::PatchNode = Self::from_js(destination_js);
        let patch = self.blossom.connection_patch(source, destination);
        Self::to_js(&patch)
    }

    pub fn deletion_patch(&self, edge_js: JsValue) -> JsValue {
        let edge: blossom::Edge = Self::from_js(edge_js);
        let patch = self.blossom.deletion_patch(edge);
        Self::to_js(&patch)
    }

    pub fn patch_location_of_location(&self, location_js: JsValue) -> JsValue {
        let location: blossom::Location = Self::from_js(location_js);
        let pl = self.blossom.patch_location_of_location(location);
        Self::to_js(&pl)
    }

    pub fn patch_node_of_node(&self, node_js: JsValue) -> JsValue {
        let node: blossom::Node = Self::from_js(node_js);
        let pn = self.blossom.patch_node_of_node(node);
        Self::to_js(&pn)
    }

    // =====================================================
    // Tree navigation (for TypeScript controller)
    // =====================================================

    pub fn source_of_term_edge(&self, te_js: JsValue) -> JsValue {
        let te: blossom::TermEdge = Self::from_js(te_js);
        let source = self.blossom.source_of_term_edge(&te);
        Self::to_js(&source)
    }

    pub fn edge_children_of_term_location(&self, tl_js: JsValue) -> Array {
        let tl: blossom::TermLocation = Self::from_js(tl_js);
        let edges = self.blossom.edge_children_of_term_location(&tl);
        let array = Array::new();
        for e in edges { array.push(&Self::to_js(&e)); }
        array
    }

    pub fn num_children_of_term_location(&self, tl_js: JsValue) -> u8 {
        let tl: blossom::TermLocation = Self::from_js(tl_js);
        self.blossom.num_children_of_term_location(&tl)
    }

    pub fn num_children_of_term_node(&self, tn_js: JsValue) -> u8 {
        let tn: blossom::TermNode = Self::from_js(tn_js);
        self.blossom.num_children_of_term_node(&tn)
    }

    pub fn node_destination_of_term_edge(&self, te_js: JsValue) -> JsValue {
        let te: blossom::TermEdge = Self::from_js(te_js);
        let dest = self.blossom.node_destination_of_term_edge(te);
        Self::to_js(&dest)
    }

    pub fn unique_parent_of_term_node(&self, tn_js: JsValue) -> JsValue {
        let tn: blossom::TermNode = Self::from_js(tn_js);
        let parent = self.blossom.unique_parent_of_term_node(&tn);
        Self::to_js(&parent)
    }

    pub fn unique_parent_edge_of_term(&self, t_js: JsValue) -> JsValue {
        let t: blossom::Term = Self::from_js(t_js);
        let parent = self.blossom.unique_parent_edge_of_term(&t);
        Self::to_js(&parent)
    }

    pub fn right_sibling_of_term_edge(&self, te_js: JsValue) -> JsValue {
        let te: blossom::TermEdge = Self::from_js(te_js);
        let sibling = self.blossom.right_sibling_of_term_edge(&te);
        Self::to_js(&sibling)
    }

    pub fn right_sibling_of_term_location(&self, tl_js: JsValue) -> JsValue {
        let tl: blossom::TermLocation = Self::from_js(tl_js);
        let sibling = self.blossom.right_sibling_of_term_location(&tl);
        Self::to_js(&sibling)
    }

    pub fn destination_of_edge(&self, edge_js: JsValue) -> JsValue {
        let edge: blossom::Edge = Self::from_js(edge_js);
        let dest = self.blossom.destination_of_edge(&edge);
        Self::to_js(&dest)
    }

    pub fn source_of_edge(&self, edge_js: JsValue) -> JsValue {
        let edge: blossom::Edge = Self::from_js(edge_js);
        let source = self.blossom.source_of_edge(&edge);
        Self::to_js(&source)
    }

    pub fn edge_children_of_location(&self, location_js: JsValue) -> Array {
        let location: blossom::Location = Self::from_js(location_js);
        let edges = self.blossom.edge_children_of_location(&location);
        let array = Array::new();
        for e in edges { array.push(&Self::to_js(&e)); }
        array
    }

    pub fn num_children_of_location(&self, location_js: JsValue) -> u8 {
        let location: blossom::Location = Self::from_js(location_js);
        self.blossom.num_children_of_location(&location)
    }
}
