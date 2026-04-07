pub mod blossom;
pub mod controller;
pub mod grove;
pub mod lang;
pub mod forest;
pub mod order;
pub mod render;
pub mod scenario;
pub mod sync_scenario;
pub mod types;

use serde::{Deserialize, Serialize};
use uuid::Uuid;
use wasm_bindgen::prelude::*;

use blossom::Blossom;
use controller::Controller;
use forest::Forest;
use grove::{Grove, Patch, Site};
use lang::{Constructor, GroveConstructor};

// ── Action type (matches RustTypes.tsx) ──────────────────────────────────────

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ForestAction {
    OpenReference(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BlossomAction {
    ForestAction(ForestAction),
    AllUpdateSteps,
    UpdateStep,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Action {
    BlossomAction(BlossomAction),
    WrapLeft(Constructor),
    WrapRight(Constructor),
    Insert(Constructor),
    Delete,
    Move(Direction),
    Cut,
    Paste,
    MoveToLocation(MoveToLocationData),
    MoveToTerm(MoveToTermData),
    TextInsert(String),
    TextBackspace,
    WrapWithProjector(Constructor),
    CanvasDrag(CanvasDragData),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MoveToLocationData {
    pub node: String,
    pub position: u8,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CanvasDragData {
    /// The Canvas node ID (child of Proj[0]).
    pub canvas: String,
    pub positions: Vec<CanvasPos>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CanvasPos {
    pub node_id: String,
    pub x: f64,
    pub y: f64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MoveToTermData {
    pub id: String,
}

// ── Helpers ──────────────────────────────────────────────────────────────────

/// Apply patches to grove, update forest intervals, and mark blossom dirty.
fn apply_patches_to_state(
    patches: &[Patch],
    grove: &mut Grove,
    forest: &mut Forest,
    blossom: &mut Blossom,
) {
    for patch in patches {
        let dirty = grove.apply_patch(patch);
        forest.update(&dirty, grove);
        for site in &dirty {
            blossom.mark_dirty(forest.tree_site_of(site), forest);
        }
    }
}

// ── WASM State ───────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub struct HazelState {
    grove: Grove,
    forest: Forest,
    blossom: Blossom,
    controller: Controller,
}

#[wasm_bindgen]
impl HazelState {
    #[wasm_bindgen(constructor)]
    pub fn new() -> HazelState {
        console_error_panic_hook::set_once();
        // Forest starts uninitialized — genesis() will init it
        HazelState {
            grove: Grove::new(),
            forest: Forest::empty(),
            blossom: Blossom::new(),
            controller: Controller::new(),
        }
    }

    /// Create the Root node for a new document.
    pub fn genesis(&mut self) -> JsValue {
        if self.grove.root_id.is_some() {
            return serde_wasm_bindgen::to_value(&Vec::<Patch>::new())
                .unwrap_or(JsValue::NULL);
        }

        let root_id = Uuid::new_v4();
        self.grove.nodes.insert(
            root_id,
            grove::GroveNode {
                id: root_id,
                constructor: GroveConstructor::Root,
                arity: 1,
            },
        );
        self.grove.root_id = Some(root_id);

        // Initialize forest with root intervals
        self.forest = Forest::init(root_id);

        self.blossom.mark_dirty(
            self.forest.tree_site_of(&Site::Loc(grove::Location { node: root_id, position: 0 })),
            &self.forest,
        );
        self.blossom.mark_dirty(
            self.forest.tree_site_of(&Site::Term(root_id)),
            &self.forest,
        );

        serde_wasm_bindgen::to_value(&Vec::<Patch>::new()).unwrap_or(JsValue::NULL)
    }

    /// Apply a single patch from Automerge.
    pub fn apply_patch(&mut self, patch_js: JsValue) {
        let patch: Patch = match serde_wasm_bindgen::from_value(patch_js) {
            Ok(p) => p,
            Err(e) => {
                web_sys::console::error_1(&format!("Failed to parse patch: {:?}", e).into());
                return;
            }
        };

        if patch.source.node.constructor == GroveConstructor::Root {
            if self.grove.root_id.is_none() {
                self.grove.root_id = Some(patch.source.node.id);
                self.forest = Forest::init(patch.source.node.id);
            }
        }
        if patch.destination.constructor == GroveConstructor::Root {
            if self.grove.root_id.is_none() {
                self.grove.root_id = Some(patch.destination.id);
                self.forest = Forest::init(patch.destination.id);
            }
        }

        let dirty = self.grove.apply_patch(&patch);
        self.forest.update(&dirty, &self.grove);
        for site in &dirty {
            self.blossom.mark_dirty(self.forest.tree_site_of(site), &self.forest);
        }
    }

    /// Apply multiple patches (batch).
    pub fn apply_patches(&mut self, patches_js: JsValue) {
        let patches: Vec<Patch> = match serde_wasm_bindgen::from_value(patches_js) {
            Ok(p) => p,
            Err(e) => {
                web_sys::console::error_1(&format!("Failed to parse patches: {:?}", e).into());
                return;
            }
        };
        apply_patches_to_state(&patches, &mut self.grove, &mut self.forest, &mut self.blossom);
    }

    /// Initialize cursor. Returns patches as JSON.
    pub fn init_cursor(&mut self, session_id: &str) -> JsValue {
        let patches = self.controller.init_cursor(session_id, &self.grove);
        apply_patches_to_state(&patches, &mut self.grove, &mut self.forest, &mut self.blossom);
        serde_wasm_bindgen::to_value(&patches).unwrap_or(JsValue::NULL)
    }

    /// Process a user action. Returns generated patches as JSON.
    pub fn perform_action(&mut self, action_js: JsValue) -> JsValue {
        let action: Action = match serde_wasm_bindgen::from_value(action_js) {
            Ok(a) => a,
            Err(e) => {
                web_sys::console::error_1(&format!("Failed to parse action: {:?}", e).into());
                return serde_wasm_bindgen::to_value(&Vec::<Patch>::new())
                    .unwrap_or(JsValue::NULL);
            }
        };

        let mut patches = self.dispatch_action(&action);
        apply_patches_to_state(&patches, &mut self.grove, &mut self.forest, &mut self.blossom);

        // Auto-advance after WrapLeft/WrapRight
        if matches!(&action, Action::WrapLeft(_) | Action::WrapRight(_)) {
            self.blossom.update_all(&self.grove, &self.forest);
            let advance = self.controller.auto_advance_down(&self.grove);
            apply_patches_to_state(&advance, &mut self.grove, &mut self.forest, &mut self.blossom);
            patches.extend(advance);
        }

        self.blossom.update_all(&self.grove, &self.forest);
        serde_wasm_bindgen::to_value(&patches).unwrap_or(JsValue::NULL)
    }

    pub fn update_all(&mut self) {
        self.blossom.update_all(&self.grove, &self.forest);
    }

    pub fn render(&self) -> JsValue {
        let tree = render::render_tree(&self.grove, &self.forest, &self.blossom, &self.controller);
        serde_wasm_bindgen::to_value(&tree).unwrap_or(JsValue::NULL)
    }

    pub fn cursor_info(&self) -> JsValue {
        let info = render::cursor_info(&self.grove, &self.forest, &self.blossom, &self.controller);
        serde_wasm_bindgen::to_value(&info).unwrap_or(JsValue::NULL)
    }

    pub fn has_dirty(&self) -> bool {
        !self.blossom.is_dirty_empty()
    }
}

impl HazelState {
    fn dispatch_action(&mut self, action: &Action) -> Vec<Patch> {
        match action {
            Action::Move(Direction::Up) => self.controller.move_up(&self.grove),
            Action::Move(Direction::Down) => self.controller.move_down(&self.grove),
            Action::Move(Direction::Left) => self.controller.move_left(&self.grove),
            Action::Move(Direction::Right) => self.controller.move_right(&self.grove),
            Action::WrapLeft(c) => self.controller.wrap_left(c.clone(), &self.grove),
            Action::WrapRight(c) => self.controller.wrap_right(c.clone(), &self.grove),
            Action::Insert(c) => self.controller.insert(c.clone(), &self.grove),
            Action::Delete => self.controller.delete(&self.grove),
            Action::Cut => self.controller.cut(&self.grove),
            Action::Paste => self.controller.paste(&self.grove),
            Action::TextInsert(ch) => self.controller.text_insert(ch, &self.grove),
            Action::TextBackspace => self.controller.text_backspace(&self.grove),
            Action::WrapWithProjector(c) => {
                self.controller.wrap_with_projector(c.clone(), &self.grove)
            }
            Action::MoveToTerm(data) => {
                if let Ok(id) = Uuid::parse_str(&data.id) {
                    self.controller.move_to_term(id, &self.grove)
                } else {
                    Vec::new()
                }
            }
            Action::MoveToLocation(data) => {
                if let Ok(node_id) = Uuid::parse_str(&data.node) {
                    self.controller.move_to_location(
                        grove::Location {
                            node: node_id,
                            position: data.position,
                        },
                        &self.grove,
                    )
                } else {
                    Vec::new()
                }
            }
            Action::BlossomAction(BlossomAction::UpdateStep) => {
                self.blossom.update_step(&self.grove, &self.forest);
                Vec::new()
            }
            Action::BlossomAction(BlossomAction::AllUpdateSteps) => {
                self.blossom.update_all(&self.grove, &self.forest);
                Vec::new()
            }
            Action::CanvasDrag(data) => {
                if let Ok(canvas_id) = Uuid::parse_str(&data.canvas) {
                    self.controller
                        .canvas_drag(canvas_id, &data.positions, &self.grove)
                } else {
                    Vec::new()
                }
            }
            Action::BlossomAction(BlossomAction::ForestAction(_)) => Vec::new(),
        }
    }
}
