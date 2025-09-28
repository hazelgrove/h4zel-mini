mod lang;
mod grove;
mod forest;
mod blossom;
mod controller;

use serde_wasm_bindgen;
use wasm_bindgen::prelude::*;
use js_sys::Array;

extern crate console_error_panic_hook;
use std::panic;

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

    fn term_of_js(t : JsValue) -> controller::Term {
        serde_wasm_bindgen::from_value(t).unwrap()
    }

    fn js_of_term(t : &controller::Term) -> JsValue {
        serde_wasm_bindgen::to_value(t).unwrap()
    }

    fn location_of_js(l : JsValue) -> controller::TermLocation {
        serde_wasm_bindgen::from_value(l).unwrap()
    }

    fn js_of_location(tl : &controller::TermLocation) -> JsValue {
        serde_wasm_bindgen::to_value(tl).unwrap()
    }

    fn action_of_js(a : JsValue) -> controller::Action {
        serde_wasm_bindgen::from_value(a).unwrap()
    }

    fn js_of_action(a : &controller::Action) -> JsValue {
        serde_wasm_bindgen::to_value(a).unwrap()
    }

    fn patch_of_js(p : JsValue) -> controller::Patch {
        serde_wasm_bindgen::from_value(p).unwrap()
    }

    fn js_of_patch(p : &controller::Patch) -> JsValue {
        serde_wasm_bindgen::to_value(p).unwrap()
    }

    fn apply_action_patches(&mut self, action: &str) -> Vec<grove::Patch> {
        match action.strip_prefix("text_insert-") {
            None => (),
            Some(x) => { return self.controller.apply_action(controller::Action::TextInsert(x.to_string())) }
        };
        match action {
            "delete" => self.controller.apply_action(controller::Action::Delete),
            "insert_zero" => self.controller.apply_action(controller::Action::Insert(lang::Constructor::Zero)),
            "wrap_left_plus" => self.controller.apply_action(controller::Action::WrapLeft(lang::Constructor::Plus)),
            "wrap_left_pair" => self.controller.apply_action(controller::Action::WrapLeft(lang::Constructor::Pair)),
            "wrap_left_fun" => self.controller.apply_action(controller::Action::WrapLeft(lang::Constructor::Fun)),
            "wrap_left_ap" => self.controller.apply_action(controller::Action::WrapLeft(lang::Constructor::Ap)),
            "wrap_left_let" => self.controller.apply_action(controller::Action::WrapLeft(lang::Constructor::Let)),
            "wrap_left_times" => { panic!("unimplemented") },
            "move_up" => self.controller.apply_action(controller::Action::Move(controller::Direction::Up)),
            "move_down" => self.controller.apply_action(controller::Action::Move(controller::Direction::Down)),
            "move_right" => self.controller.apply_action(controller::Action::Move(controller::Direction::Right)),
            "cut" => self.controller.apply_action(controller::Action::Cut),
            "paste" => self.controller.apply_action(controller::Action::Paste),
            "text_backspace" => self.controller.apply_action(controller::Action::TextBackspace),
            "update" => self.controller.apply_action(controller::Action::BlossomAction(blossom::Action::UpdateStep)),
            "all_updates" => self.controller.apply_action(controller::Action::BlossomAction(blossom::Action::AllUpdateSteps)),
            _ => { panic!("unrecognized action string") },
        }
    }

    fn apply_serial_action_patches(&mut self, action: JsValue) -> Vec<grove::Patch> {
        self.controller.apply_action(Self::action_of_js(action))
    }

    pub fn apply_action(&mut self, action: &str) -> Array {
        let patches = self.apply_action_patches(action);
        // print!();
        let array = Array::new();
        for p in patches {
            array.push(&Self::js_of_patch(&p));
        }
        array
    }

    pub fn apply_serial_action(&mut self, action: JsValue) -> Array {
        let patches = self.apply_serial_action_patches(action);
        // print!();
        let array = Array::new();
        for p in patches {
            array.push(&Self::js_of_patch(&p));
        }
        array
    }

    pub fn apply_patch(&mut self, patchjs: JsValue) {
        let patch = Self::patch_of_js(patchjs);
        self.controller.apply_patch(patch)
    }

    pub fn move_to_location(&mut self, tljs: JsValue) {
        let tl = Self::location_of_js(tljs);
        self.controller.apply_action(controller::Action::MoveToLocation(tl));
    }

    pub fn move_to_term(&mut self, tjs: JsValue) {
        let t = Self::term_of_js(tjs);
        self.controller.apply_action(controller::Action::MoveToTerm(t));
    }

    pub fn root_location(&self) -> JsValue {
        Self::js_of_location(&self.controller.root_term_location())
    }

    // pub fn root_terms(&self) -> Array {
    //     let ts = self.controller.root_terms();
    //     let array = Array::new();
    //     for t in ts {
    //         array.push(&Self::js_of_term(&t));
    //     }
    //     array
    // }

    pub fn constructor_of_term(&self, t : JsValue) -> String {
        let t = Self::term_of_js(t);
        controller::State::constructor_of_term(&self.controller, t).to_string()
    }

    pub fn is_dirty(&mut self, tjs: JsValue) -> bool {
        let t = Self::term_of_js(tjs);
        self.controller.is_dirty(&t)
    }

    pub fn size_of_term(&mut self, tjs: JsValue) -> Option<u32> {
        let t = Self::term_of_js(tjs);
        self.controller.nodecount_of_term(&t).copied()
    }

    // outputs an array of locations 
    pub fn children_of_term(&self, tjs : JsValue) -> Array {
        let t = Self::term_of_js(tjs);
        let cs = self.controller.children_of_term(&t);
        // let cs = controller::State::children_of_term(&self.controller, &t);
        let array = Array::new();
        for l in cs {
            array.push(&Self::js_of_location(&l));
        }
        array
    }

    // outputs an array of terms 
    pub fn children_of_location(&self, tljs : JsValue) -> Array {
        let tl = Self::location_of_js(tljs);
        let ts = self.controller.children_of_term_location(&tl);
        let array = Array::new();
        for t in ts.iter() {
            array.push(&Self::js_of_term(t));
        }
        array
    }

    pub fn cursor_at_term(&self, tjs : JsValue) -> bool {
        let t = Self::term_of_js(tjs);
        self.controller.cursor_at_term(t)
    }

    pub fn cursor_at_location(&self, tljs : JsValue) -> bool {
        let tl = Self::location_of_js(tljs);
        self.controller.cursor_at_location(tl)
    }

    pub fn clipboard_at_term(&self, tjs : JsValue) -> bool {
        let t = Self::term_of_js(tjs);
        self.controller.clipboard_at_term(t)
    }

    pub fn clipboard_at_location(&self, tljs : JsValue) -> bool {
        let tl = Self::location_of_js(tljs);
        self.controller.clipboard_at_location(tl)
    }
}