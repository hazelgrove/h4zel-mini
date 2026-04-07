//! Single-user tests: basic operations, navigation, editing, types, regressions.

use rust::grove::{self, Location};
use rust::lang::{Constructor, GroveConstructor};
use rust::render::RenderNode;
use rust::scenario::Scenario;
use uuid::Uuid;

// ── Basic sanity ─────────────────────────────────────────────────────────────

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

    s.cut();
    s.assert_invariants();
    assert_eq!(s.cursor_content_id(), Some(zero_id));
    assert_eq!(s.controller.clipboard, Some(zero_id));

    s.up();
    s.wrap_left(Constructor::Plus);
    s.right();

    s.paste();
    s.assert_invariants();
    assert_eq!(s.cursor_content_id(), Some(zero_id));
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
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
    s.backspace();
    assert!(s.cursor_content_id().is_none());
}

// ── Navigation ───────────────────────────────────────────────────────────────

#[test]
fn navigate_up_down() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.up();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Plus"));
    s.down();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
}

#[test]
fn navigate_right_cyclic() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.right();
    s.assert_invariants();
    assert!(s.cursor_content_id().is_none(), "Plus[1] should be empty");
    s.right();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
}

#[test]
fn navigate_to_root_boundary() {
    let mut s = Scenario::new();
    s.up();
    s.assert_invariants();
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

// ── Wrap operations ──────────────────────────────────────────────────────────

#[test]
fn wrap_left_builds_expression() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.up();
    s.wrap_left(Constructor::Plus);
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
    s.right();
    s.insert(Constructor::Zero);
    s.up();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Plus"));
    assert_eq!(s.cursor_syn_name().as_deref(), Some("Num"));
}

#[test]
fn wrap_left_on_empty_creates_node() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.up();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Plus"));
}

#[test]
fn wrap_right_places_content_at_pos1() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.up();
    s.wrap_right(Constructor::Plus);
    s.right();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
}

// ── Type checking ────────────────────────────────────────────────────────────

#[test]
fn sort_inconsistency_detected() {
    let mut s = Scenario::new();
    s.insert(Constructor::Num);
    s.assert_invariants();
    let info = s.cursor_info();
    assert!(!info.marks.is_empty(), "Num in Expression context should produce mark");
}

#[test]
fn fun_arrow_type() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Fun);
    s.text("x");
    s.right();
    s.insert(Constructor::Zero);
    s.up();
    s.assert_invariants();
    let info = s.cursor_info();
    assert!(info.syn.is_some());
}

#[test]
fn ascription_type() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.up();
    s.wrap_left(Constructor::Asc);
    s.right();
    s.insert(Constructor::Num);
    s.up();
    s.assert_invariants();
    let info = s.cursor_info();
    assert!(Scenario::render_node_constructor(info.syn.as_ref().unwrap()) == Some("Num"));
}

#[test]
fn ascription_ana_flows_to_body() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Asc);
    let _body_info = s.cursor_info();
    s.right();
    s.insert(Constructor::Num);
    s.right();
    assert_eq!(
        s.cursor_ana_name().as_deref(),
        Some("Num"),
        "Asc body's analyzed type should be the annotation type"
    );
}

#[test]
fn ascription_mark_propagates_without_cursor_visit() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.up();
    s.wrap_left(Constructor::Asc);
    s.right();
    s.wrap_left(Constructor::Arrow);

    let zero_id = s.grove.nodes.values()
        .find(|n| n.constructor == GroveConstructor::Lang(Constructor::Zero))
        .map(|n| n.id)
        .expect("Zero node should exist");
    let zero_attr = s.blossom.get_attr(&grove::Site::Term(zero_id), &s.forest);
    assert!(
        !zero_attr.marks.is_empty(),
        "Zero should have a TypeInconsistent mark from Arrow annotation, \
         even without cursor visiting it. Got attr: {:?}",
        zero_attr
    );
}

#[test]
fn asc_zero_then_num_wrapped_in_arrow_mark_propagates() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.up();
    s.wrap_left(Constructor::Asc);
    s.right();
    s.insert(Constructor::Num);
    s.wrap_left(Constructor::Arrow);

    let zero_id = s.grove.nodes.values()
        .find(|n| n.constructor == GroveConstructor::Lang(Constructor::Zero))
        .map(|n| n.id)
        .expect("Zero node should exist");
    let zero_attr = s.blossom.get_attr(&grove::Site::Term(zero_id), &s.forest);
    assert!(
        !zero_attr.marks.is_empty(),
        "Zero should have TypeInconsistent mark from Arrow annotation. \
         Tree:\n{}Attr: {:?}",
        s.tree_string(),
        zero_attr
    );
}

// ── Regression: clicks ───────────────────────────────────────────────────────

#[test]
fn click_same_hole_repeatedly_no_cycle() {
    let mut s = Scenario::new();
    let loc = s.cursor_location().unwrap();
    for _ in 0..10 {
        s.click_hole(loc.node, loc.position);
    }
    s.assert_invariants();
}

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
    s.click_term(content_id);
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
}

// ── Regression: projectors ───────────────────────────────────────────────────

#[test]
fn wrap_projector_then_click_no_crash() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.up();
    s.wrap_projector(Constructor::Structural);
    s.assert_invariants();
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
    s.down();
    s.assert_invariants();
}

#[test]
fn projector_nested() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.up();
    s.wrap_projector(Constructor::Structural);
    s.up();
    s.wrap_projector(Constructor::Structural);
    s.assert_invariants();
    s.down();
    s.down();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
}

#[test]
fn projector_down_up_roundtrip() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.up();
    s.wrap_projector(Constructor::Structural);
    s.down();
    s.up();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Proj"));
}

#[test]
fn click_hole_inside_projector() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.up();
    s.up();
    s.wrap_projector(Constructor::Structural);
    s.assert_invariants();
    s.down();
    s.down();
    s.right();
    s.assert_invariants();
    assert!(s.cursor_content_id().is_none());
}

// ── Stress tests ─────────────────────────────────────────────────────────────

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
    s.wrap_left(Constructor::Let);
    s.text("x");
    s.right();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.right();
    s.insert(Constructor::Zero);
    s.up();
    s.up();
    s.right();
    s.text("x");
    s.up();
    s.assert_invariants();
    s.down();
    s.right();
    s.down();
    s.right();
    s.up();
    s.up();
    s.right();
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

// ── Click tests ──────────────────────────────────────────────────────────────

#[test]
fn click_on_sibling_node() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.right();
    s.insert(Constructor::Zero);
    let right_zero = s.cursor_content_id().unwrap();
    s.right();
    let left_zero = s.cursor_content_id().unwrap();
    s.click_term(right_zero);
    s.assert_invariants();
    assert_eq!(s.cursor_content_id(), Some(right_zero));
    s.click_term(left_zero);
    s.assert_invariants();
    assert_eq!(s.cursor_content_id(), Some(left_zero));
}

#[test]
fn click_on_parent_node() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.up();
    let plus_id = s.cursor_content_id().unwrap();
    s.down();
    s.click_term(plus_id);
    s.assert_invariants();
    assert_eq!(s.cursor_content_id(), Some(plus_id));
}

#[test]
fn click_every_node_in_tree() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.right();
    s.insert(Constructor::Num);
    s.up();
    let node_ids: Vec<Uuid> = s.grove.nodes.keys().copied().collect();
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
    for _ in 0..10 {
        s.click_term(zero_id);
        s.assert_cursor_valid();
        s.click_hole(loc.node, loc.position);
        s.assert_cursor_valid();
    }
    s.assert_invariants();
}

// ── Editing sequences ────────────────────────────────────────────────────────

#[test]
fn build_and_type_check_full_program() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Asc);
    s.wrap_left(Constructor::Fun);
    s.text("x");
    s.right();
    s.wrap_left(Constructor::Plus);
    s.text("x");
    s.right();
    s.insert(Constructor::Zero);
    s.up();
    s.up();
    s.up();
    s.right();
    s.wrap_left(Constructor::Arrow);
    s.insert(Constructor::Num);
    s.right();
    s.insert(Constructor::Num);
    s.up();
    s.up();
    s.up();
    s.assert_invariants();
    let info = s.cursor_info();
    assert!(info.syn.is_some(), "Asc should have synthesized type");
}

#[test]
fn let_binding_type_propagation() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Let);
    s.text("x");
    s.right();
    s.insert(Constructor::Zero);
    s.right();
    s.text("x");
    s.assert_invariants();
    assert_eq!(s.cursor_syn_name().as_deref(), Some("Num"), "Bound variable should synthesize binding's type");
}

#[test]
fn fun_with_asc_pattern_binding_type() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Fun);
    s.wrap_left(Constructor::Asc);
    s.text("x");
    s.right();
    s.insert(Constructor::Num);
    s.left();
    s.up();
    s.right();
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
    s.wrap_left(Constructor::Asc);
    s.wrap_left(Constructor::Fun);
    s.text("x");
    s.right();
    s.text("x");
    s.left();
    s.up();
    s.right();
    s.wrap_left(Constructor::Arrow);
    s.insert(Constructor::Num);
    s.right();
    s.insert(Constructor::Num);
    s.left();
    s.up();
    s.left();
    s.down();
    s.right();
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
    s.wrap_left(Constructor::Fun);
    s.text("x");
    s.right();
    s.text("x");
    s.assert_invariants();
    let info = s.cursor_info();
    assert!(
        info.syn.is_none()
            || matches!(info.syn.as_ref(), Some(RenderNode::Hole { .. })),
        "Body x with no annotation should synthesize Unknown"
    );
}

// ── Edge cases ───────────────────────────────────────────────────────────────

#[test]
fn delete_on_empty_is_noop() {
    let mut s = Scenario::new();
    s.delete();
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
    s.insert(Constructor::Zero);
    s.cut();
    s.paste();
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
}

#[test]
fn insert_on_nonempty_is_noop() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.insert(Constructor::Num);
    s.assert_invariants();
    assert_eq!(s.cursor_content_name().as_deref(), Some("Zero"));
}

#[test]
fn text_insert_on_non_identifier_is_noop() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);
    s.text("x");
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

// ── Keyboard shortcut coverage ───────────────────────────────────────────────

#[test]
fn all_wrap_constructors() {
    let constructors = vec![
        Constructor::Plus, Constructor::Prod, Constructor::Pair,
        Constructor::Arrow, Constructor::Fun, Constructor::Asc,
        Constructor::Ap, Constructor::Let,
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
        Constructor::Zero, Constructor::Typ, Constructor::Num,
        Constructor::Structural, Constructor::Collapsed,
    ];
    for c in constructors {
        let mut s = Scenario::new();
        s.insert(c.clone());
        s.assert_invariants();
    }
}

// ── Movement preserves AST (single user) ─────────────────────────────────────

#[test]
fn move_right_preserves_ast_simple() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    let snapshot = s.ast_snapshot();
    s.right();
    assert_eq!(snapshot, s.ast_snapshot(), "Move Right should not change the AST");
}

#[test]
fn move_left_preserves_ast_simple() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.right();
    s.insert(Constructor::Zero);
    let snapshot = s.ast_snapshot();
    s.left();
    assert_eq!(snapshot, s.ast_snapshot(), "Move Left should not change the AST");
}

#[test]
fn move_up_preserves_ast() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    let snapshot = s.ast_snapshot();
    s.up();
    assert_eq!(snapshot, s.ast_snapshot(), "Move Up should not change the AST");
}

#[test]
fn move_down_preserves_ast() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.up();
    let snapshot = s.ast_snapshot();
    s.down();
    assert_eq!(snapshot, s.ast_snapshot(), "Move Down should not change the AST");
}

#[test]
fn repeated_movement_preserves_ast() {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.right();
    s.insert(Constructor::Zero);
    let snapshot = s.ast_snapshot();
    for _ in 0..5 {
        s.up();
        s.down();
        s.right();
        s.left();
    }
    assert_eq!(snapshot, s.ast_snapshot(), "Repeated movement should not change the AST");
}
