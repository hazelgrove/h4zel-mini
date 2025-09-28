mod lang;
mod grove;
mod forest;
mod blossom;
mod controller;
mod order;
mod types;

use serde_wasm_bindgen;
use serde::Serialize;
use serde::de::DeserializeOwned;
use wasm_bindgen::prelude::*;
use js_sys::Array;

extern crate console_error_panic_hook;
use std::panic;

use crate::types::TypeAttribute;

#[wasm_bindgen]
pub struct WasmState {
    controller: controller::State,
}

#[wasm_bindgen]
impl WasmState {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmState {
        panic::set_hook(Box::new(console_error_panic_hook::hook));
        WasmState {
            controller: controller::State::new(),
        }
    }

    fn from_js<T: DeserializeOwned>(v: JsValue) -> T {
        serde_wasm_bindgen::from_value(v).unwrap()
    }

    fn to_js<T: Serialize>(t: &T) -> JsValue {
        serde_wasm_bindgen::to_value(t).unwrap()
    }

    fn apply_serial_action_patches(&mut self, action: JsValue) -> Vec<grove::Patch> {
        self.controller.apply_action(Self::from_js(action))
    }

    pub fn apply_serial_action(&mut self, action: JsValue) -> Array {
        let patches = self.apply_serial_action_patches(action);
        let array = Array::new();
        for p in patches { array.push(&Self::to_js(&p)); }
        array
    }

    pub fn apply_patch(&mut self, patchjs: JsValue) {
        let patch = Self::from_js(patchjs);
        self.controller.apply_patch(patch)
    }

    pub fn move_to_location(&mut self, tljs: JsValue) {
        let tl = Self::from_js(tljs);
        self.controller.apply_action(controller::Action::MoveToLocation(tl));
    }

    pub fn move_to_term(&mut self, tjs: JsValue) {
        let t = Self::from_js(tjs);
        self.controller.apply_action(controller::Action::MoveToTerm(t));
    }

    pub fn root_location(&self) -> JsValue {
        Self::to_js(&self.controller.root_term_location())
    }

    pub fn constructor_of_term(&self, t : JsValue) -> JsValue {
        let t = &Self::from_js(t);
        let c = controller::State::constructor_of_term(&self.controller, t);
        Self::to_js(&c)
    }

    pub fn ana_of_term(&mut self, tjs: JsValue) -> JsValue {
        let t = Self::from_js(tjs);
        let ana = self.controller.types_of_site(&forest::TermSite::Term(t)).and_then(|a| a.ana.clone());
        Self::to_js(&ana)
    }

    pub fn syn_of_term(&mut self, tjs: JsValue) -> JsValue {
        let t = Self::from_js(tjs);
        let syn = self.controller.types_of_site(&forest::TermSite::Term(t)).and_then(|a| a.syn.clone());
        Self::to_js(&syn)
    }

    pub fn constructor_of_type(&self, tjs : JsValue) -> JsValue {
        let t = &Self::from_js(tjs);
        let c = self.controller.constructor_of_type(t);
        Self::to_js(&c)
    }
    
    pub fn children_of_type(&self, tjs : JsValue) -> Array {
        let t = Self::from_js(tjs);
        let tcs : Vec<types::TypeLocation> = self.controller.children_of_type(&t);
        let array = Array::new();
        for tc in tcs {  array.push(&Self::to_js(&tc)); }
        array
    }
    
    pub fn children_of_type_location(&self, tjs : JsValue) -> Array {
        let t = Self::from_js(tjs);
        let tcs : Vec<types::Type> = self.controller.children_of_type_location(&t);
        let array = Array::new();
        for tc in tcs {  array.push(&Self::to_js(&tc)); }
        array
    }

    pub fn size_of_term(&mut self, tjs: JsValue) -> Option<u32> {
        let t = Self::from_js(tjs);
        self.controller.nodecount_of_term(&t).copied()
    }

    pub fn size_of_location(&mut self, tjs: JsValue) -> Option<u32> {
        let tl : blossom::TermLocation = Self::from_js(tjs);
        self.controller.nodecount_of_location(&tl).copied()
    }

    // outputs an array of locations 
    pub fn children_of_term(&self, tjs : JsValue) -> Array {
        let t = Self::from_js(tjs);
        let cs = self.controller.children_of_term(&t);
        // let cs = controller::State::children_of_term(&self.controller, &t);
        let array = Array::new();
        for l in cs {  array.push(&Self::to_js(&l)); }
        array
    }

    // outputs an array of terms 
    pub fn children_of_location(&self, tljs : JsValue) -> Array {
        let tl = Self::from_js(tljs);
        let ts = self.controller.children_of_term_location(&tl);
        let array = Array::new();
        for t in ts.iter() {  array.push(&Self::to_js(t)); }
        array
    }

    pub fn cursor_at_term(&self, tjs : JsValue) -> bool {
        let t = Self::from_js(tjs);
        self.controller.cursor_at_term(t)
    }

    pub fn cursor_almost_at_term(&self, tjs : JsValue) -> bool {
        let t = Self::from_js(tjs);
        self.controller.cursor_almost_at_term(t)
    }

    pub fn cursor_at_location(&self, tljs : JsValue) -> bool {
        let tl = Self::from_js(tljs);
        self.controller.cursor_at_location(tl)
    }

    pub fn cursor_almost_at_location(&self, tljs : JsValue) -> bool {
        let tl = Self::from_js(tljs);
        self.controller.cursor_almost_at_location(tl)
    }

    pub fn clipboard_at_term(&self, tjs : JsValue) -> bool {
        let t = Self::from_js(tjs);
        self.controller.clipboard_at_term(t)
    }

    pub fn clipboard_at_location(&self, tljs : JsValue) -> bool {
        let tl = Self::from_js(tljs);
        self.controller.clipboard_at_location(tl)
    }

    pub fn is_dirty_term(&mut self, tjs: JsValue) -> bool {
        let t : blossom::Term = Self::from_js(tjs);
        self.controller.is_dirty(&forest::TermSite::Term(t))
    }

    pub fn is_dirty_location(&mut self, tjs: JsValue) -> bool {
        let t : blossom::TermLocation = Self::from_js(tjs);
        self.controller.is_dirty(&forest::TermSite::Location(t))
    }
}