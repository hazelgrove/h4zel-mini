mod lang;
mod grove;
mod forest;
mod blossom;
mod controller;

use serde_wasm_bindgen;
use wasm_bindgen::prelude::*;
use js_sys::Array;

use crate::blossom::TermLocation;

#[wasm_bindgen]
pub struct WasmState {
    controller: controller::State,
}

#[wasm_bindgen]
impl WasmState {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmState {
        WasmState {
            controller: controller::State::new(),
        }
    }

    pub fn apply_action(&mut self, action: &str) {
        match action {
            "delete" => self.controller.apply_action(controller::Action::Delete),
            "insert_zero" => self.controller.apply_action(controller::Action::Insert(lang::Constructor::Zero)),
            "wrap_left_plus" => self.controller.apply_action(controller::Action::WrapLeft(lang::Constructor::Plus)),
            "wrap_left_times" => {},
            "move_up" => self.controller.apply_action(controller::Action::Move(controller::Direction::Up)),
            "move_down" => self.controller.apply_action(controller::Action::Move(controller::Direction::Down)),
            "move_right" => self.controller.apply_action(controller::Action::Move(controller::Direction::Right)),
            "cut" => self.controller.apply_action(controller::Action::Cut),
            "paste" => self.controller.apply_action(controller::Action::Paste),
            _ => { panic!("unrecognized action string") },
        }
    }

    fn term_of_js(t : JsValue) -> controller::Term {
        serde_wasm_bindgen::from_value(t).unwrap()
    }

    fn location_of_js(l : JsValue) -> controller::TermLocation {
        serde_wasm_bindgen::from_value(l).unwrap()
    }

    fn js_of_term(t : &controller::Term) -> JsValue {
        serde_wasm_bindgen::to_value(t).unwrap()
    }

    fn js_of_location(tl : &controller::TermLocation) -> JsValue {
        serde_wasm_bindgen::to_value(tl).unwrap()
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

}