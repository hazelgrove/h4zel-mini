mod lang;
mod grove;
mod controller;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn bruh() -> usize {
    println!("bruh");
    10
}

#[wasm_bindgen]
pub fn main(){
    let s = &mut controller::State::init();
    controller::State::apply_action(s, controller::Action::Cut);
    println!("Hello, world!");
}
