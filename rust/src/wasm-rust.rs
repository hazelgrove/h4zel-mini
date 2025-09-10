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

    pub fn root(&self) -> String {
        controller::State::root(&self.controller).to_string()
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

    pub fn constructor_of_node(&self, s : String) -> String {
        let n = grove::Node::of_string(&s);
        controller::State::constructor_of_node(&self.controller, &n).to_string()
    }

    // outputs an array of arrays of node id strings
    pub fn children(&self, s : String) -> Array {
        let n = grove::Node::of_string(&s);
        let css = controller::State::children_of_node(&self.controller, &n);
        let outer = Array::new();
        for cs in css.iter() {
            let inner = Array::new();
            for c in cs.iter() {
                inner.push(&JsValue::from_str(&grove::Node::to_string(c)));
            }
            outer.push(&inner);
        }
        outer
    }

    pub fn cursor(&self) -> JsValue {
        let c = self.controller.cursor();
        serde_wasm_bindgen::to_value(&c).unwrap()
    }

}