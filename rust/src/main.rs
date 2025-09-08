mod lang;
mod grove;
mod controller;

fn main() {
    let s = &mut controller::State::init();
    controller::State::apply_action(s, controller::Action::Cut);
    println!("Hello, world!");
}
