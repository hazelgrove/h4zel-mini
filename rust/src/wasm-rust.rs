mod lang;
mod grove;
mod controller;
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
            "wrap_left_plus" => {},
            "wrap_left_times" => {},
            "move_up" => {},
            "move_down" => {},
            "move_right" => {},
            "copy" => {},
            "paste" => {},
            _ => {},
        }
    }

    pub fn constructor(&self, s : String) -> String {
        let n = grove::Node::of_string(&s);
        match controller::State::constructor_of_node(&self.controller, &n) {
            grove::Constructor::Root => "ROOT".to_string(),
            grove::Constructor::Lang(c) => todo!()
        }
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

}