mod lang;
mod grove;
mod controller;

use serde_wasm_bindgen;
use wasm_bindgen::prelude::*;
use js_sys::Array;

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

    fn term_of_js(t : JsValue) -> grove::Term {
        serde_wasm_bindgen::from_value(t).unwrap()
    }

    fn location_of_js(l : JsValue) -> grove::Location {
        serde_wasm_bindgen::from_value(l).unwrap()
    }

    fn js_of_term(t : &grove::Term) -> JsValue {
        serde_wasm_bindgen::to_value(t).unwrap()
    }

    fn js_of_location(l : &grove::Location) -> JsValue {
        serde_wasm_bindgen::to_value(l).unwrap()
    }

    pub fn top_root(&self) -> JsValue {
        let l = controller::State::top_root(&self.controller);
        Self::js_of_location(&l)
    }


    pub fn constructor_of_term(&self, t : JsValue) -> String {
        let t = Self::term_of_js(t);
        controller::State::constructor_of_term(&self.controller, t).to_string()
    }

    // outputs an array of locations 
    pub fn children_of_term(&self, t : JsValue) -> Array {
        let t = Self::term_of_js(t);
        let n = *t.to_node();
        let num_children = controller::State::num_children_of_term(&self.controller, &t);
        let array = Array::new();
        for position in 0..num_children {
            let l = grove::Location { node : n, position : position };
            array.push(&Self::js_of_location(&l));
        }
        array
    }

    // outputs an array of terms 
    pub fn children_of_location(&self, t : JsValue) -> Array {
        let l = Self::location_of_js(t);
        let ts = controller::State::children_of_location(&self.controller, &l);
        let array = Array::new();
        for t in ts.iter() {
            array.push(&Self::js_of_term(t));
        }
        array
    }

    pub fn cursor_at_term(&self, t : JsValue) -> bool {
        let t = Self::term_of_js(t);
        controller::State::cursor_at_term(&self.controller, t)
    }

    pub fn cursor_at_location(&self, l : JsValue) -> bool {
        let l = Self::location_of_js(l);
        controller::State::cursor_at_location(&self.controller, l)
    }

}