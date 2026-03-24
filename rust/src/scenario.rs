//! Test scenario framework for simulating browser-like interactions.
//!
//! Wraps Grove + Blossom + Controller directly (no WASM boundary),
//! providing a high-level API that mirrors what a user does in the browser.

use std::collections::HashSet;
use uuid::Uuid;

use crate::blossom::Blossom;
use crate::controller::Controller;
use crate::grove::{Grove, Location, Patch, Site};
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

    fn apply_patches(&mut self, patches: &[Patch]) {
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
    #[cfg(test)]
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
    #[cfg(test)]
    pub fn assert_no_cycles(&self) {
        let root_id = self.grove.root_id.unwrap();
        let mut visited = HashSet::new();
        self.check_cycles_from(root_id, &mut visited);
    }

    #[cfg(test)]
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
    #[cfg(test)]
    pub fn assert_renders_ok(&self) {
        let tree = self.render();
        // Also verify it's serializable (catches recursive objects)
        let json = serde_json::to_string(&tree);
        assert!(json.is_ok(), "Render tree not serializable: {:?}", json.err());
    }

    /// Assert the cursor is in a valid state.
    #[cfg(test)]
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
    #[cfg(test)]
    pub fn assert_invariants(&self) {
        self.assert_cursor_valid();
        self.assert_no_self_edges();
        self.assert_no_cycles();
        self.assert_renders_ok();
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

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::Constructor;

    // ── Basic sanity ─────────────────────────────────────────────────────────

    #[test]
    fn fresh_scenario_is_valid() {
        let s = Scenario::new();
        s.assert_invariants();
        assert!(s.cursor_content_name().is_none(), "Cursor starts empty");
    }

    #[test]
    fn insert_zero_and_check_type() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
        assert_eq!(s.cursor_syn_name().as_deref(), Some("Num"));
    }

    #[test]
    fn insert_delete_roundtrip() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        assert!(s.cursor_content_id().is_some());
        s.delete();
        s.assert_invariants();
        assert!(s.cursor_content_id().is_none());
    }

    #[test]
    fn cut_paste_roundtrip() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        let zero_id = s.cursor_content_id().unwrap();

        // Cut: no patches, node stays in place, just marked as clipboard
        s.cut();
        s.assert_invariants();
        // Content is still there (cut doesn't remove it)
        assert_eq!(s.cursor_content_id(), Some(zero_id));
        // But clipboard is set
        assert_eq!(s.controller.clipboard, Some(zero_id));

        // Navigate away so cursor is empty, then paste
        s.up(); // wrapping content (still has Zero + clipboard mark)
        s.wrap_left(Constructor::Plus); // Plus wraps the Asc-containing tree
        // Now at Plus[0], which has the Zero
        s.right(); // Plus[1] is empty

        s.paste();
        s.assert_invariants();
        // Zero moved here
        assert_eq!(s.cursor_content_id(), Some(zero_id));
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
        // Clipboard cleared
        assert!(s.controller.clipboard.is_none());
    }

    #[test]
    fn text_insert_creates_identifier() {
        let mut s = Scenario::new();
        s.text("abc");
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Identifier(abc)"));
    }

    #[test]
    fn text_backspace() {
        let mut s = Scenario::new();
        s.text("abc");
        s.backspace();
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Identifier(ab)"));
        s.backspace();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Identifier(a)"));
        s.backspace(); // single char → delete
        assert!(s.cursor_content_id().is_none());
    }

    // ── Navigation ───────────────────────────────────────────────────────────

    #[test]
    fn navigate_up_down() {
        let mut s = Scenario::new();
        // Build: Plus(Zero, ⬚)
        s.wrap_left(Constructor::Plus);
        // After auto-advance, cursor is at Plus[0] (first child)
        s.insert(Constructor::Zero);
        // Move up to Plus
        s.up();
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Plus"));
        // Move down back to first child
        s.down();
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    #[test]
    fn navigate_right_cyclic() {
        let mut s = Scenario::new();
        // Build Plus with two children
        s.wrap_left(Constructor::Plus);
        s.insert(Constructor::Zero); // Plus[0] = Zero
        s.right(); // Move to Plus[1]
        s.assert_invariants();
        assert!(s.cursor_content_id().is_none(), "Plus[1] should be empty");
        s.right(); // Wrap around to Plus[0]
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    #[test]
    fn navigate_to_root_boundary() {
        let mut s = Scenario::new();
        // At root, Up should be no-op
        s.up();
        s.assert_invariants();
        // Cursor should still be at root
        let loc = s.cursor_location().unwrap();
        assert_eq!(loc.node, s.root_id());
    }

    #[test]
    fn navigate_down_on_empty_is_noop() {
        let mut s = Scenario::new();
        s.down();
        s.assert_invariants();
        assert!(s.cursor_content_id().is_none());
    }

    #[test]
    fn navigate_down_on_arity0_is_noop() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.down();
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    // ── Wrap operations ──────────────────────────────────────────────────────

    #[test]
    fn wrap_left_builds_expression() {
        let mut s = Scenario::new();
        // Build: (0 + 0)
        s.insert(Constructor::Zero);
        s.up(); // now wrapping Zero at Root[0]
        s.wrap_left(Constructor::Plus); // Plus wraps Zero at pos 0, auto-advance into Plus
        // Should be at Plus[0] wrapping Zero
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
        s.right(); // Plus[1]
        s.insert(Constructor::Zero);
        s.up(); // back to Plus
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Plus"));
        assert_eq!(s.cursor_syn_name().as_deref(), Some("Num"));
    }

    #[test]
    fn wrap_left_on_empty_creates_node() {
        let mut s = Scenario::new();
        s.wrap_left(Constructor::Plus); // creates Plus with empty children, auto-advance
        s.up(); // back up to Plus
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Plus"));
    }

    // ── Type checking ────────────────────────────────────────────────────────

    #[test]
    fn sort_inconsistency_detected() {
        let mut s = Scenario::new();
        // Num in Expression position → sort error
        s.insert(Constructor::Num);
        s.assert_invariants();
        let info = s.cursor_info();
        assert!(!info.marks.is_empty(), "Num in Expression context should produce mark");
    }

    #[test]
    fn fun_arrow_type() {
        let mut s = Scenario::new();
        // fun x ↦ 0  — should synthesize Arrow(?, ℕ)
        s.wrap_left(Constructor::Fun);
        // Auto-advanced into Fun[0] (pattern)
        s.text("x");
        s.right(); // Fun[1] (body)
        s.insert(Constructor::Zero);
        s.up(); // Fun
        s.assert_invariants();
        let info = s.cursor_info();
        // Fun synthesizes Arrow(syn[0], syn[1]) = Arrow(?, ℕ)
        assert!(info.syn.is_some());
    }

    #[test]
    fn ascription_type() {
        let mut s = Scenario::new();
        // (0 : Num)
        s.insert(Constructor::Zero);
        s.up();
        s.wrap_left(Constructor::Asc); // Asc wraps Zero at pos 0, auto-advance into Asc
        // At Asc[0] wrapping Zero
        s.right(); // Asc[1] (type annotation)
        s.insert(Constructor::Num);
        s.up(); // Asc
        s.assert_invariants();
        // Asc syn = Surface(Num), which displays as ℕ
        let info = s.cursor_info();
        assert!(Scenario::render_node_constructor(info.syn.as_ref().unwrap()) == Some("Num"));
    }

    #[test]
    fn ascription_ana_flows_to_body() {
        let mut s = Scenario::new();
        // (⬚ : Num) — the body's ana should be ℕ
        s.wrap_left(Constructor::Asc);
        // At Asc[0] (empty body)
        let _body_info = s.cursor_info();
        // Before annotation, ana might be Unknown
        s.right(); // Asc[1]
        s.insert(Constructor::Num);
        // Now go back to body
        s.right(); // wraps to Asc[0]
        assert_eq!(
            s.cursor_ana_name().as_deref(),
            Some("Num"),
            "Asc body's analyzed type should be the annotation type"
        );
    }

    /// REGRESSION: inserting a type annotation while cursor is at Asc[1] must
    /// propagate the mark to Zero at Asc[0] immediately — not only when the
    /// cursor visits Zero later.
    #[test]
    fn ascription_mark_propagates_without_cursor_visit() {
        let mut s = Scenario::new();
        // Build: (0 : ⬚)
        s.insert(Constructor::Zero);
        s.up();
        s.wrap_left(Constructor::Asc);
        // Cursor at Asc[0] wrapping Zero
        s.right(); // Asc[1] — empty annotation slot

        // Insert Arrow — now (0 : Arrow(⬚,⬚))
        // Zero (Num) is inconsistent with Arrow(?,?)
        s.wrap_left(Constructor::Arrow);

        // WITHOUT navigating back to Zero, check that Zero has a mark.
        // Read Zero's type attribute directly from the blossom.
        let zero_id = {
            // Find Zero node in the grove
            s.grove.nodes.values()
                .find(|n| n.constructor == GroveConstructor::Lang(Constructor::Zero))
                .map(|n| n.id)
                .expect("Zero node should exist")
        };
        let zero_attr = s.blossom.get_attr(&crate::grove::Site::Term(zero_id));
        assert!(
            !zero_attr.marks.is_empty(),
            "Zero should have a TypeInconsistent mark from Arrow annotation, \
             even without cursor visiting it. Got attr: {:?}",
            zero_attr
        );
    }

    /// REGRESSION: exact user scenario — insert Zero, Asc, Num, wrap Num in Arrow.
    /// Mark on Zero must appear immediately.
    #[test]
    fn asc_zero_then_num_wrapped_in_arrow_mark_propagates() {
        let mut s = Scenario::new();
        // 1. Insert Zero
        s.insert(Constructor::Zero);
        // 2. Wrap in Asc (press :)
        s.up();
        s.wrap_left(Constructor::Asc);
        // cursor at Asc[0] wrapping Zero
        // 3. Right to Asc[1]
        s.right();
        // 4. Insert Num
        s.insert(Constructor::Num);
        // 5. Wrap Num in Arrow (press - directly, no Up needed)
        s.wrap_left(Constructor::Arrow);
        // Now: (0 : (ℕ → ⬚))
        // Cursor is inside Arrow (auto-advanced)

        // Check Zero has a mark WITHOUT navigating to it
        let zero_id = s.grove.nodes.values()
            .find(|n| n.constructor == GroveConstructor::Lang(Constructor::Zero))
            .map(|n| n.id)
            .expect("Zero node should exist");
        let zero_attr = s.blossom.get_attr(&crate::grove::Site::Term(zero_id));
        assert!(
            !zero_attr.marks.is_empty(),
            "Zero should have TypeInconsistent mark from Arrow annotation. \
             Tree:\n{}Attr: {:?}",
            s.tree_string(),
            zero_attr
        );
    }

    // ── REGRESSION: clicking same hole repeatedly ────────────────────────────

    #[test]
    fn click_same_hole_repeatedly_no_cycle() {
        let mut s = Scenario::new();
        let loc = s.cursor_location().unwrap();

        // Click the same hole 10 times
        for _ in 0..10 {
            s.click_hole(loc.node, loc.position);
        }

        s.assert_invariants();
    }

    // ── REGRESSION: clicking on cursor node ──────────────────────────────────

    #[test]
    fn click_on_cursor_node_no_cycle() {
        let mut s = Scenario::new();
        let cursor_id = s.cursor_node_id().unwrap();

        s.click_term(cursor_id);
        s.assert_invariants();
    }

    #[test]
    fn click_on_cursor_content_no_cycle() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        let content_id = s.cursor_content_id().unwrap();

        // Click on the content (which is already wrapped by cursor)
        s.click_term(content_id);
        s.assert_invariants();
        // Content should still be Zero
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    // ── REGRESSION: projector click crash ────────────────────────────────────

    #[test]
    fn wrap_projector_then_click_no_crash() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);

        // Wrap with projector
        s.up();
        s.wrap_projector(Constructor::Structural);

        s.assert_invariants();

        // Now click on the Proj node itself
        // The Proj is the cursor's content
        let content_id = s.cursor_content_id();
        if let Some(proj_id) = content_id {
            s.click_term(proj_id);
            s.assert_invariants();
        }
    }

    #[test]
    fn wrap_projector_navigate_inside() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.up();
        s.wrap_projector(Constructor::Structural);
        s.assert_invariants();

        // Navigate down into projector content
        s.down();
        s.assert_invariants();
    }

    // ── Stress: rapid mixed actions ──────────────────────────────────────────

    #[test]
    fn rapid_insert_delete_cycle() {
        let mut s = Scenario::new();
        for _ in 0..20 {
            s.insert(Constructor::Zero);
            s.assert_cursor_valid();
            s.delete();
            s.assert_cursor_valid();
        }
        s.assert_invariants();
    }

    #[test]
    fn rapid_navigation_no_corruption() {
        let mut s = Scenario::new();
        s.wrap_left(Constructor::Plus);
        s.insert(Constructor::Zero);
        s.right();
        s.insert(Constructor::Zero);

        // Rapid navigation
        for _ in 0..20 {
            s.up();
            s.down();
            s.right();
        }
        s.assert_invariants();
    }

    #[test]
    fn build_complex_expression_and_navigate() {
        let mut s = Scenario::new();
        // Build: let x = (0 + 0) in x
        s.wrap_left(Constructor::Let);
        // At Let[0] (pattern)
        s.text("x");
        s.right(); // Let[1] (binding)
        s.wrap_left(Constructor::Plus);
        s.insert(Constructor::Zero);
        s.right();
        s.insert(Constructor::Zero);
        s.up(); // Plus
        s.up(); // Let (pos 1)
        s.right(); // Let[2] (body)
        s.text("x");
        s.up(); // Let

        s.assert_invariants();

        // Navigate through entire structure
        s.down(); // Let[0]
        s.right(); // Let[1]
        s.down(); // Plus[0]
        s.right(); // Plus[1]
        s.up(); // Plus
        s.up(); // Let (pos 1)
        s.right(); // Let[2]

        s.assert_invariants();
    }

    #[test]
    fn tree_string_output_sanity() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        let tree = s.tree_string();
        assert!(tree.contains("Root"), "Should contain Root");
        assert!(tree.contains("Cursor"), "Should contain Cursor");
        assert!(tree.contains("Zero"), "Should contain Zero");
    }

    // ── Click on various things in a tree ────────────────────────────────────

    #[test]
    fn click_on_sibling_node() {
        let mut s = Scenario::new();
        // Build Plus(Zero, Zero)
        s.wrap_left(Constructor::Plus);
        s.insert(Constructor::Zero);
        s.right();
        s.insert(Constructor::Zero);
        let right_zero = s.cursor_content_id().unwrap();
        s.right(); // back to left
        let left_zero = s.cursor_content_id().unwrap();

        // Click on the right zero from left position
        s.click_term(right_zero);
        s.assert_invariants();
        assert_eq!(s.cursor_content_id(), Some(right_zero));

        // Click on left zero
        s.click_term(left_zero);
        s.assert_invariants();
        assert_eq!(s.cursor_content_id(), Some(left_zero));
    }

    #[test]
    fn click_on_parent_node() {
        let mut s = Scenario::new();
        s.wrap_left(Constructor::Plus);
        s.insert(Constructor::Zero);
        // Cursor is at Plus[0] wrapping Zero. Click on Plus itself.
        s.up(); // go up to Plus
        let plus_id = s.cursor_content_id().unwrap();
        s.down(); // back into Plus[0]

        s.click_term(plus_id);
        s.assert_invariants();
        assert_eq!(s.cursor_content_id(), Some(plus_id));
    }

    // ── Projector edge cases ─────────────────────────────────────────────────

    #[test]
    fn projector_nested() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.up();
        s.wrap_projector(Constructor::Structural);
        s.up(); // wrapping Proj
        s.wrap_projector(Constructor::Structural); // double wrap
        s.assert_invariants();

        // Navigate into nested projectors
        s.down(); // into outer Proj[1]
        s.down(); // into inner Proj[1]
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    #[test]
    fn projector_down_up_roundtrip() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.up();
        s.wrap_projector(Constructor::Structural);
        let loc_before = s.cursor_location();

        s.down(); // into Proj[1]
        s.up();   // back to wrapping Proj
        s.assert_invariants();

        // Should be back at same position
        assert_eq!(s.cursor_content_name().as_deref(), Some("Proj"));
    }

    #[test]
    fn click_hole_inside_projector() {
        let mut s = Scenario::new();
        // Create Proj(Structural, Plus(Zero, ⬚))
        s.wrap_left(Constructor::Plus);
        s.insert(Constructor::Zero);
        s.up(); // wrapping Plus
        s.up(); // wrapping Plus from Root
        s.wrap_projector(Constructor::Structural);
        s.assert_invariants();

        // Navigate into projector → Plus → right child (hole)
        s.down(); // Proj[1] wrapping Plus
        s.down(); // Plus[0] wrapping Zero
        s.right(); // Plus[1] (hole)
        s.assert_invariants();
        assert!(s.cursor_content_id().is_none());
    }

    // ── Click stress tests ───────────────────────────────────────────────────

    #[test]
    fn click_every_node_in_tree() {
        let mut s = Scenario::new();
        // Build: Plus(Zero, Num)
        s.wrap_left(Constructor::Plus);
        s.insert(Constructor::Zero);
        s.right();
        s.insert(Constructor::Num);
        s.up(); // Plus

        // Collect all node IDs from the tree
        let node_ids: Vec<Uuid> = s.grove.nodes.keys().copied().collect();

        // Click each one — none should cause a crash
        for id in &node_ids {
            s.click_term(*id);
            s.assert_cursor_valid();
            s.assert_no_self_edges();
        }
        s.assert_invariants();
    }

    #[test]
    fn click_alternating_holes_and_terms() {
        let mut s = Scenario::new();
        s.wrap_left(Constructor::Plus);
        s.insert(Constructor::Zero);
        let zero_id = s.cursor_content_id().unwrap();
        s.right();
        let loc = s.cursor_location().unwrap();

        // Alternate between clicking the Zero and the empty hole
        for _ in 0..10 {
            s.click_term(zero_id);
            s.assert_cursor_valid();
            s.click_hole(loc.node, loc.position);
            s.assert_cursor_valid();
        }
        s.assert_invariants();
    }

    // ── Editing sequences ────────────────────────────────────────────────────

    #[test]
    fn build_and_type_check_full_program() {
        let mut s = Scenario::new();

        // Build: (fun x ↦ x + 0) : (ℕ → ℕ)
        // Start with Asc
        s.wrap_left(Constructor::Asc);
        // Asc[0]: build fun x ↦ x + 0
        s.wrap_left(Constructor::Fun);
        // Fun[0] (pattern)
        s.text("x");
        s.right(); // Fun[1] (body)
        s.wrap_left(Constructor::Plus);
        // Plus[0]
        s.text("x");
        s.right(); // Plus[1]
        s.insert(Constructor::Zero);

        // Navigate up to Asc
        s.up(); // Plus
        s.up(); // Fun (at Fun[1])
        s.up(); // Asc (at Asc[0])
        s.right(); // Asc[1] (type annotation)

        // Build Arrow(Num, Num)
        s.wrap_left(Constructor::Arrow);
        s.insert(Constructor::Num);
        s.right();
        s.insert(Constructor::Num);

        // Go up to Asc
        s.up(); // Arrow
        s.up(); // Asc (at Asc[1])
        s.up(); // wrapping Asc from Root

        s.assert_invariants();
        let info = s.cursor_info();
        // Asc should synthesize the Surface type (ℕ → ℕ)
        assert!(info.syn.is_some(), "Asc should have synthesized type");
    }

    #[test]
    fn let_binding_type_propagation() {
        let mut s = Scenario::new();
        // let x = 0 in x
        s.wrap_left(Constructor::Let);
        // Let[0] (pattern)
        s.text("x");
        s.right(); // Let[1] (binding expr)
        s.insert(Constructor::Zero);
        s.right(); // Let[2] (body)
        s.text("x");

        s.assert_invariants();
        // The body "x" should see the binding type
        assert_eq!(s.cursor_syn_name().as_deref(), Some("Num"), "Bound variable should synthesize binding's type");
    }

    #[test]
    fn fun_with_asc_pattern_binding_type() {
        let mut s = Scenario::new();
        // fun (x : ℕ) ↦ x — body x should synthesize ℕ
        s.wrap_left(Constructor::Fun);
        // At Fun[0] (pattern) — auto-advanced
        s.wrap_left(Constructor::Asc);
        // At Asc[0] inside Fun[0] — auto-advanced
        s.text("x");
        s.right(); // Asc[1]
        s.insert(Constructor::Num);
        // Navigate: Asc[1] → Asc[0] → up to Fun[0] → right to Fun[1]
        s.left(); // back to Asc[0]
        s.up();   // wrapping Asc at Fun[0]
        s.right(); // Fun[1] (body)
        s.text("x");

        s.assert_invariants();
        assert_eq!(
            s.cursor_syn_name().as_deref(),
            Some("Num"),
            "Body x should synthesize ℕ from pattern annotation (x : ℕ)"
        );
    }

    #[test]
    fn fun_with_external_arrow_binding_type() {
        let mut s = Scenario::new();
        // (fun x ↦ x) : (ℕ → ℕ) — body x should synthesize ℕ from external annotation
        s.wrap_left(Constructor::Asc);
        // At Asc[0] — auto-advanced
        s.wrap_left(Constructor::Fun);
        // At Fun[0] (pattern) — auto-advanced
        s.text("x");
        s.right(); // Fun[1] (body)
        s.text("x");
        // Navigate: Fun[1] → Fun[0] → up to Asc[0] → right to Asc[1]
        s.left();  // Fun[0]
        s.up();    // wrapping Fun at Asc[0]
        s.right(); // Asc[1]
        // Build Arrow(Num, Num)
        s.wrap_left(Constructor::Arrow);
        s.insert(Constructor::Num);
        s.right();
        s.insert(Constructor::Num);
        // Navigate back to body x: Arrow[1] → Arrow[0] → up → Asc[1] → left → Asc[0] → down → Fun → down → Fun[0] → right → Fun[1]
        s.left();  // Arrow[0]
        s.up();    // wrapping Arrow at Asc[1]
        s.left();  // Asc[0] wrapping Fun
        s.down();  // Fun[0] (pattern)
        s.right(); // Fun[1] (body)
        // Cursor should wrap 'x' in the body
        s.assert_invariants();
        assert_eq!(
            s.cursor_syn_name().as_deref(),
            Some("Num"),
            "Body x should synthesize ℕ from external (ℕ → ℕ) annotation"
        );
    }

    #[test]
    fn fun_no_annotation_binding_unknown() {
        let mut s = Scenario::new();
        // fun x ↦ x — no annotation, body x should synthesize Unknown
        s.wrap_left(Constructor::Fun);
        s.text("x");
        s.right();
        s.text("x");

        s.assert_invariants();
        // With no type info, syn should be Unknown (hole in render)
        let info = s.cursor_info();
        assert!(
            info.syn.is_none()
                || matches!(info.syn.as_ref(), Some(crate::render::RenderNode::Hole { .. })),
            "Body x with no annotation should synthesize Unknown"
        );
    }

    #[test]
    fn wrap_right_places_content_at_pos1() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.up();
        s.wrap_right(Constructor::Plus);
        // Zero should be at Plus[1], cursor auto-advanced into Plus[0]
        // Navigate to Plus[1]
        s.right();
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    #[test]
    fn delete_on_empty_is_noop() {
        let mut s = Scenario::new();
        s.delete(); // nothing to delete
        s.assert_invariants();
        assert!(s.cursor_content_id().is_none());
    }

    #[test]
    fn cut_on_empty_is_noop() {
        let mut s = Scenario::new();
        s.cut();
        s.assert_invariants();
    }

    #[test]
    fn paste_on_nonempty_is_noop() {
        let mut s = Scenario::new();
        // Set up clipboard with a Zero
        s.insert(Constructor::Zero);
        s.cut(); // marks Zero as clipboard, stays in cursor
        // Cursor still has content (Zero), so paste should be noop
        s.paste();
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    #[test]
    fn insert_on_nonempty_is_noop() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.insert(Constructor::Num); // should be noop
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    #[test]
    fn text_insert_on_non_identifier_is_noop() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.text("x"); // should be noop — Zero is not an Identifier
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    #[test]
    fn backspace_on_non_identifier_is_noop() {
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);
        s.backspace();
        s.assert_invariants();
        assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    }

    #[test]
    fn backspace_on_empty_is_noop() {
        let mut s = Scenario::new();
        s.backspace();
        s.assert_invariants();
    }

    // ── Keyboard shortcut coverage ───────────────────────────────────────────

    #[test]
    fn all_wrap_constructors() {
        // Test every WrapLeft constructor produces valid state
        let constructors = vec![
            Constructor::Plus,
            Constructor::Prod,
            Constructor::Pair,
            Constructor::Arrow,
            Constructor::Fun,
            Constructor::Asc,
            Constructor::Ap,
            Constructor::Let,
        ];
        for c in constructors {
            let mut s = Scenario::new();
            s.wrap_left(c.clone());
            s.assert_invariants();
        }
    }

    #[test]
    fn all_insert_constructors() {
        let constructors = vec![
            Constructor::Zero,
            Constructor::Typ,
            Constructor::Num,
            Constructor::Structural,
            Constructor::Collapsed,
        ];
        for c in constructors {
            let mut s = Scenario::new();
            s.insert(c.clone());
            s.assert_invariants();
        }
    }

    // ── Multi-user simulation ────────────────────────────────────────────────

    #[test]
    fn two_cursors_at_same_location() {
        // Simulate: second cursor created at same Root[0]
        let mut s = Scenario::new();
        s.insert(Constructor::Zero);

        // Create a second cursor manually at Root[0]
        let root_id = s.root_id();
        let cursor2_id = Uuid::new_v4();
        let identity2_id = Uuid::new_v4();

        use crate::grove::birth_patch;
        let patches = vec![
            birth_patch(
                root_id,
                GroveConstructor::Root,
                0,
                cursor2_id,
                GroveConstructor::Lang(Constructor::Cursor),
            ),
            birth_patch(
                cursor2_id,
                GroveConstructor::Lang(Constructor::Cursor),
                0,
                identity2_id,
                GroveConstructor::Lang(Constructor::Identifier("user2".into())),
            ),
        ];
        s.apply_patches(&patches);
        s.blossom.update_all(&s.grove);

        // Should still render OK despite conflict at Root[0]
        s.assert_renders_ok();
        s.assert_cursor_valid();
    }
}
