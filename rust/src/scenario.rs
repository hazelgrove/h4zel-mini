//! Test scenario framework for simulating browser-like interactions.
//!
//! Wraps Grove + Blossom + Controller directly (no WASM boundary),
//! providing a high-level API that mirrors what a user does in the browser.

use std::collections::HashSet;
use uuid::Uuid;

use crate::blossom::Blossom;
use crate::controller::Controller;
use crate::grove::{Grove, Location, Patch};
use crate::lang::{Constructor, GroveConstructor};
use crate::render::{self, CursorInfo, RenderNode};

/// A test scenario with initialized state (root + cursor).
pub struct Scenario {
    pub grove: Grove,
    pub blossom: Blossom,
    pub controller: Controller,
}

impl Scenario {
    /// Create a fresh scenario with genesis root and initialized cursor.
    pub fn new() -> Self {
        let mut grove = Grove::new();
        let blossom = Blossom::new();
        let controller = Controller::new();

        // Genesis: create root node directly
        let root_id = Uuid::new_v4();
        grove.nodes.insert(
            root_id,
            crate::grove::GroveNode {
                id: root_id,
                constructor: GroveConstructor::Root,
                arity: 1,
            },
        );
        grove.root_id = Some(root_id);

        let mut s = Scenario {
            grove,
            blossom,
            controller,
        };

        // Init cursor
        let patches = s.controller.init_cursor("test-session", &s.grove);
        s.apply_patches(&patches);
        s.blossom.update_all(&s.grove);

        s
    }

    // ── Action dispatch ──────────────────────────────────────────────────────

    pub fn apply_patches(&mut self, patches: &[Patch]) {
        for patch in patches {
            let dirty = self.grove.apply_patch(patch);
            for site in dirty {
                self.blossom.mark_dirty(site);
            }
        }
    }

    /// Perform an action (same logic as HazelState::perform_action).
    pub fn act(&mut self, action: &crate::Action) -> Vec<Patch> {
        use crate::{Action, BlossomAction, Direction};

        let mut patches = match action {
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
                    self.controller
                        .move_to_location(
                            Location {
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
                self.blossom.update_step(&self.grove);
                Vec::new()
            }
            Action::BlossomAction(BlossomAction::AllUpdateSteps) => {
                self.blossom.update_all(&self.grove);
                Vec::new()
            }
            Action::CanvasDrag(data) => {
                if let Ok(canvas_id) = Uuid::parse_str(&data.canvas) {
                    self.controller.canvas_drag(canvas_id, &data.positions, &self.grove)
                } else {
                    Vec::new()
                }
            }
            Action::BlossomAction(BlossomAction::ForestAction(_)) => Vec::new(),
        };

        self.apply_patches(&patches);

        // Auto-advance after WrapLeft/WrapRight
        if matches!(action, Action::WrapLeft(_) | Action::WrapRight(_)) {
            self.blossom.update_all(&self.grove);
            let advance = self.controller.auto_advance_down(&self.grove);
            self.apply_patches(&advance);
            patches.extend(advance);
        }

        self.blossom.update_all(&self.grove);
        patches
    }

    // ── Shorthand actions ────────────────────────────────────────────────────

    pub fn insert(&mut self, c: Constructor) -> &mut Self {
        self.act(&crate::Action::Insert(c));
        self
    }

    pub fn delete(&mut self) -> &mut Self {
        self.act(&crate::Action::Delete);
        self
    }

    pub fn wrap_left(&mut self, c: Constructor) -> &mut Self {
        self.act(&crate::Action::WrapLeft(c));
        self
    }

    pub fn wrap_right(&mut self, c: Constructor) -> &mut Self {
        self.act(&crate::Action::WrapRight(c));
        self
    }

    pub fn up(&mut self) -> &mut Self {
        self.act(&crate::Action::Move(crate::Direction::Up));
        self
    }

    pub fn down(&mut self) -> &mut Self {
        self.act(&crate::Action::Move(crate::Direction::Down));
        self
    }

    pub fn left(&mut self) -> &mut Self {
        self.act(&crate::Action::Move(crate::Direction::Left));
        self
    }

    pub fn right(&mut self) -> &mut Self {
        self.act(&crate::Action::Move(crate::Direction::Right));
        self
    }

    pub fn text(&mut self, s: &str) -> &mut Self {
        for ch in s.chars() {
            self.act(&crate::Action::TextInsert(ch.to_string()));
        }
        self
    }

    pub fn backspace(&mut self) -> &mut Self {
        self.act(&crate::Action::TextBackspace);
        self
    }

    pub fn cut(&mut self) -> &mut Self {
        self.act(&crate::Action::Cut);
        self
    }

    pub fn paste(&mut self) -> &mut Self {
        self.act(&crate::Action::Paste);
        self
    }

    pub fn wrap_projector(&mut self, c: Constructor) -> &mut Self {
        self.act(&crate::Action::WrapWithProjector(c));
        self
    }

    /// Click on a term node by its UUID.
    pub fn click_term(&mut self, id: Uuid) -> &mut Self {
        self.act(&crate::Action::MoveToTerm(crate::MoveToTermData {
            id: id.to_string(),
        }));
        self
    }

    /// Click on a hole/location.
    pub fn click_hole(&mut self, node_id: Uuid, position: u8) -> &mut Self {
        self.act(&crate::Action::MoveToLocation(
            crate::MoveToLocationData {
                node: node_id.to_string(),
                position,
            },
        ));
        self
    }

    // ── Queries ──────────────────────────────────────────────────────────────

    pub fn cursor_info(&self) -> CursorInfo {
        render::cursor_info(&self.grove, &self.blossom, &self.controller)
    }

    /// Helper: get the top-level constructor name of a rendered type.
    pub fn render_node_constructor(node: &RenderNode) -> Option<&str> {
        match node {
            RenderNode::Term { constructor, .. } => Some(constructor.as_str()),
            RenderNode::Hole { .. } => None,
            _ => None,
        }
    }

    /// Shorthand: get the syn type's constructor name from cursor info.
    pub fn cursor_syn_name(&self) -> Option<String> {
        let info = self.cursor_info();
        info.syn.as_ref().and_then(|n| Self::render_node_constructor(n).map(|s| s.to_string()))
    }

    /// Shorthand: get the ana type's constructor name from cursor info.
    pub fn cursor_ana_name(&self) -> Option<String> {
        let info = self.cursor_info();
        info.ana.as_ref().and_then(|n| Self::render_node_constructor(n).map(|s| s.to_string()))
    }

    pub fn render(&self) -> RenderNode {
        render::render_tree(&self.grove, &self.blossom, &self.controller)
    }

    pub fn cursor_state(&self) -> Option<crate::controller::CursorState> {
        self.controller.cursor_state(&self.grove)
    }

    /// Get the cursor's current content node ID.
    pub fn cursor_content_id(&self) -> Option<Uuid> {
        self.cursor_state().and_then(|cs| cs.content)
    }

    /// Get the constructor name of the cursor's content.
    pub fn cursor_content_name(&self) -> Option<String> {
        let content_id = self.cursor_content_id()?;
        let node = self.grove.node(content_id)?;
        Some(match &node.constructor {
            GroveConstructor::Root => "Root".to_string(),
            GroveConstructor::Lang(c) => match c {
                Constructor::Identifier(s) => format!("Identifier({})", s),
                other => other.display_name().to_string(),
            },
        })
    }

    /// Get the cursor's current location.
    pub fn cursor_location(&self) -> Option<Location> {
        self.cursor_state().map(|cs| cs.cursor_location)
    }

    /// Get the cursor node ID.
    pub fn cursor_node_id(&self) -> Option<Uuid> {
        self.controller.cursor_node
    }

    /// Get the root node ID.
    pub fn root_id(&self) -> Uuid {
        self.grove.root_id.unwrap()
    }

    // ── Assertions ───────────────────────────────────────────────────────────

    /// Assert that no node in the grove has a self-referencing edge.
    pub fn assert_no_self_edges(&self) {
        for edge in self.grove.edges.values() {
            if edge.sign == crate::grove::Sign::Live {
                assert_ne!(
                    edge.source.node, edge.destination,
                    "Self-edge detected: node {} has edge to itself",
                    edge.source.node
                );
            }
        }
    }

    /// Assert no unicycles exist in the grove (among reachable nodes).
    pub fn assert_no_cycles(&self) {
        let root_id = self.grove.root_id.unwrap();
        let mut visited = HashSet::new();
        self.check_cycles_from(root_id, &mut visited);
    }

    fn check_cycles_from(&self, node_id: Uuid, visited: &mut HashSet<Uuid>) {
        if !visited.insert(node_id) {
            panic!("Cycle detected: revisited node {}", node_id);
        }
        if let Some(node) = self.grove.node(node_id) {
            for pos in 0..node.arity {
                let loc = Location {
                    node: node_id,
                    position: pos,
                };
                for child_id in self.grove.live_children_at(&loc) {
                    // References (2+ parents) are OK — just don't recurse into them
                    if self.grove.live_parent_edge_ids(child_id).len() <= 1 {
                        self.check_cycles_from(child_id, visited);
                    }
                }
            }
        }
        visited.remove(&node_id);
    }

    /// Assert the render tree can be produced without panic/infinite recursion.
    pub fn assert_renders_ok(&self) {
        let tree = self.render();
        // Also verify it's serializable (catches recursive objects)
        let json = serde_json::to_string(&tree);
        assert!(json.is_ok(), "Render tree not serializable: {:?}", json.err());
    }

    /// Assert the cursor is in a valid state.
    pub fn assert_cursor_valid(&self) {
        let cs = self.cursor_state();
        assert!(cs.is_some(), "Cursor state should be derivable");
        let cs = cs.unwrap();

        // Cursor node should exist
        assert!(
            self.grove.node(cs.cursor_node).is_some(),
            "Cursor node should exist in grove"
        );

        // Cursor should have exactly one parent
        assert_eq!(
            self.grove.live_parent_edge_ids(cs.cursor_node).len(),
            1,
            "Cursor node should have exactly one parent"
        );

        // Cursor's parent location should be valid
        assert!(
            self.grove.node(cs.cursor_location.node).is_some(),
            "Cursor parent node should exist"
        );

        // Content (if any) should exist and be at Cursor[1]
        if let Some(content_id) = cs.content {
            assert!(
                self.grove.node(content_id).is_some(),
                "Cursor content node should exist"
            );
            let content_loc = Location {
                node: cs.cursor_node,
                position: 1,
            };
            let children = self.grove.live_children_at(&content_loc);
            assert!(
                children.contains(&content_id),
                "Content should be at Cursor[1]"
            );
        }
    }

    /// Run all standard invariant checks.
    pub fn assert_invariants(&self) {
        self.assert_cursor_valid();
        self.assert_no_self_edges();
        self.assert_no_cycles();
        self.assert_renders_ok();
    }

    /// Snapshot the "cursor-erased AST" — the set of (parent_node, position, child_node)
    /// edges, looking through Cursor nodes. This should be invariant under cursor movement.
    pub fn ast_snapshot(&self) -> Vec<(Uuid, u8, Uuid)> {
        let mut edges: Vec<(Uuid, u8, Uuid)> = Vec::new();
        let mut visited = HashSet::new();
        self.collect_ast_edges(self.grove.root_id.unwrap(), &mut edges, &mut visited);
        edges.sort();
        edges
    }

    fn collect_ast_edges(
        &self,
        node_id: Uuid,
        edges: &mut Vec<(Uuid, u8, Uuid)>,
        visited: &mut HashSet<Uuid>,
    ) {
        if !visited.insert(node_id) {
            return;
        }
        let node = match self.grove.node(node_id) {
            Some(n) => n,
            None => return,
        };
        let is_cursor = matches!(
            node.constructor,
            GroveConstructor::Lang(Constructor::Cursor)
        );
        for pos in 0..node.arity {
            let loc = Location { node: node_id, position: pos };
            for child_id in self.grove.live_children_at(&loc) {
                let child_node = self.grove.node(child_id);
                let child_is_cursor = child_node.map_or(false, |n| {
                    matches!(n.constructor, GroveConstructor::Lang(Constructor::Cursor))
                });

                if child_is_cursor {
                    // Look through cursor: its content (pos 1) belongs to this location.
                    // Recursively peel nested cursors (e.g. CursorA wrapping CursorB).
                    self.collect_cursor_content(child_id, node_id, pos, is_cursor, edges, visited);
                } else if !is_cursor || pos == 1 {
                    // Skip cursor metadata (pos 0 = identity)
                    if !is_cursor {
                        edges.push((node_id, pos, child_id));
                    }
                    self.collect_ast_edges(child_id, edges, visited);
                }
            }
        }
    }

    /// Recursively peel through cursor nodes to find non-cursor content.
    /// All non-cursor content is attributed to (parent_id, parent_pos) in the
    /// cursor-erased AST.
    fn collect_cursor_content(
        &self,
        cursor_id: Uuid,
        parent_id: Uuid,
        parent_pos: u8,
        parent_is_cursor: bool,
        edges: &mut Vec<(Uuid, u8, Uuid)>,
        visited: &mut HashSet<Uuid>,
    ) {
        let content_loc = Location { node: cursor_id, position: 1 };
        for content_id in self.grove.live_children_at(&content_loc) {
            let content_node = self.grove.node(content_id);
            let content_is_cursor = content_node.map_or(false, |n| {
                matches!(n.constructor, GroveConstructor::Lang(Constructor::Cursor))
            });

            if content_is_cursor {
                // Nested cursor — keep peeling
                self.collect_cursor_content(
                    content_id, parent_id, parent_pos, parent_is_cursor, edges, visited,
                );
            } else {
                if !parent_is_cursor {
                    edges.push((parent_id, parent_pos, content_id));
                }
                self.collect_ast_edges(content_id, edges, visited);
            }
        }
    }

    /// Assert that cursor movement doesn't change the cursor-erased AST.
    pub fn assert_movement_preserves_ast(&mut self, action: &crate::Action) {
        let before = self.ast_snapshot();
        self.act(action);
        let after = self.ast_snapshot();
        assert_eq!(
            before, after,
            "Cursor movement changed the AST!\nBefore: {:?}\nAfter:  {:?}\nTree:\n{}",
            before, after, self.tree_string()
        );
    }

    /// Produce a human-readable string of the tree structure.
    pub fn tree_string(&self) -> String {
        let root_id = self.grove.root_id.unwrap();
        let mut out = String::new();
        self.tree_string_node(root_id, 0, &mut out, &mut HashSet::new());
        out
    }

    fn tree_string_node(
        &self,
        node_id: Uuid,
        depth: usize,
        out: &mut String,
        visited: &mut HashSet<Uuid>,
    ) {
        if depth > 20 || !visited.insert(node_id) {
            out.push_str(&format!("{}...\n", "  ".repeat(depth)));
            return;
        }

        let node = match self.grove.node(node_id) {
            Some(n) => n,
            None => {
                out.push_str(&format!("{}???\n", "  ".repeat(depth)));
                return;
            }
        };

        let indent = "  ".repeat(depth);
        let name = match &node.constructor {
            GroveConstructor::Root => "Root".to_string(),
            GroveConstructor::Lang(c) => match c {
                Constructor::Identifier(s) => format!("'{}'", s),
                other => other.display_name().to_string(),
            },
        };

        let cursor_marker = if Some(node_id) == self.controller.cursor_node {
            " [CURSOR]"
        } else {
            ""
        };

        out.push_str(&format!("{}{}{}\n", indent, name, cursor_marker));

        for pos in 0..node.arity {
            let loc = Location {
                node: node_id,
                position: pos,
            };
            let children = self.grove.live_children_at(&loc);
            if children.is_empty() {
                out.push_str(&format!("{}  [{}]: ⬚\n", indent, pos));
            } else if children.len() == 1 {
                out.push_str(&format!("{}  [{}]:\n", indent, pos));
                self.tree_string_node(children[0], depth + 2, out, visited);
            } else {
                out.push_str(&format!("{}  [{}]: CONFLICT({})\n", indent, pos, children.len()));
                for child in children {
                    self.tree_string_node(child, depth + 2, out, visited);
                }
            }
        }

        visited.remove(&node_id);
    }
}
