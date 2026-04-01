//! Multi-user conflict tests: cursor movement must never change the cursor-erased AST.

use rust::grove::{self, Location};
use rust::lang::{Constructor, GroveConstructor};
use rust::scenario::Scenario;
use rust::sync_scenario::SyncScenario;
use rust::{Action, Direction};
use uuid::Uuid;

// ── Helpers ──────────────────────────────────────────────────────────────────

/// Build a Plus(A, B) conflict at Plus[0] using manual patches (single-user).
fn setup_conflict_scenario() -> Scenario {
    let mut s = Scenario::new();
    s.wrap_left(Constructor::Plus);
    s.insert(Constructor::Zero);
    s.right();
    s.up();

    let cs = s.cursor_state().unwrap();
    let plus_id = cs.content.unwrap();

    let ident_a = Uuid::new_v4();

    let patches = vec![grove::birth_patch(
        plus_id,
        GroveConstructor::Lang(Constructor::Plus),
        0,
        ident_a,
        GroveConstructor::Lang(Constructor::Identifier("a".into())),
    )];
    s.apply_patches(&patches);
    s.blossom.update_all(&s.grove);

    s.down();
    s.click_term(ident_a);
    s
}

/// Standard two-user conflict: Alice inserts +, desync, Alice types x, Bob types y, resync.
fn setup_sync_conflict() -> SyncScenario {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Plus);
    s.desync();
    s.alice_text("x");
    s.bob_down();
    s.bob_down();
    s.bob_text("y");
    s.sync();
    s
}

// ── Single-user conflict (manual patches) ────────────────────────────────────

#[test]
fn move_right_in_conflict_preserves_ast() {
    let mut s = setup_conflict_scenario();
    let snapshot = s.ast_snapshot();
    s.right();
    assert_eq!(
        snapshot, s.ast_snapshot(),
        "Move Right from conflict should not relocate the selected term.\nTree:\n{}",
        s.tree_string()
    );
}

#[test]
fn move_left_in_conflict_preserves_ast() {
    let mut s = setup_conflict_scenario();
    s.right();
    let snapshot = s.ast_snapshot();
    s.left();
    assert_eq!(
        snapshot, s.ast_snapshot(),
        "Move Left into conflict should not relocate any term"
    );
}

#[test]
fn two_cursor_conflict_move_right_preserves_ast() {
    let mut s = Scenario::new();

    s.wrap_left(Constructor::Plus);
    let _cursor_a = s.controller.cursor_node.unwrap();
    let plus_id = s.cursor_state().unwrap().cursor_location.node;

    let cursor_b = Uuid::new_v4();
    let identity_b = Uuid::new_v4();
    s.apply_patches(&[
        grove::birth_patch(
            plus_id, GroveConstructor::Lang(Constructor::Plus), 0,
            cursor_b, GroveConstructor::Lang(Constructor::Cursor),
        ),
        grove::birth_patch(
            cursor_b, GroveConstructor::Lang(Constructor::Cursor), 0,
            identity_b, GroveConstructor::Lang(Constructor::Identifier("user-b".into())),
        ),
    ]);
    s.blossom.update_all(&s.grove);

    s.text("a");
    let a_id = s.cursor_content_id().unwrap();

    let b_id = Uuid::new_v4();
    s.apply_patches(&[grove::birth_patch(
        cursor_b, GroveConstructor::Lang(Constructor::Cursor), 1,
        b_id, GroveConstructor::Lang(Constructor::Identifier("b".into())),
    )]);
    s.blossom.update_all(&s.grove);

    let plus0 = Location { node: plus_id, position: 0 };
    assert!(s.grove.live_children_at(&plus0).len() >= 2, "Plus[0] should have conflict");

    let snapshot = s.ast_snapshot();
    s.right();
    assert_eq!(snapshot, s.ast_snapshot(), "Move Right must NOT change the cursor-erased AST");

    let plus0_children = s.grove.live_children_at(&plus0);
    let b_at_plus0 = plus0_children.iter().any(|&child| {
        child == b_id || {
            let loc = Location { node: child, position: 1 };
            s.grove.live_children_at(&loc).contains(&b_id)
        }
    });
    assert!(b_at_plus0, "b must remain at Plus[0]");

    let a_at_plus0 = plus0_children.iter().any(|&child| {
        child == a_id || {
            let loc = Location { node: child, position: 1 };
            s.grove.live_children_at(&loc).contains(&a_id)
        }
    });
    assert!(a_at_plus0, "a must remain at Plus[0]");
}

#[test]
fn insert_through_other_cursor_on_hole() {
    let mut s = Scenario::new();
    let root_id = s.root_id();
    let cursor2_id = Uuid::new_v4();
    let identity2_id = Uuid::new_v4();

    s.apply_patches(&[
        grove::birth_patch(
            root_id, GroveConstructor::Root, 0,
            cursor2_id, GroveConstructor::Lang(Constructor::Cursor),
        ),
        grove::birth_patch(
            cursor2_id, GroveConstructor::Lang(Constructor::Cursor), 0,
            identity2_id, GroveConstructor::Lang(Constructor::Identifier("user2".into())),
        ),
    ]);
    s.blossom.update_all(&s.grove);

    s.click_term(cursor2_id);
    assert!(
        s.controller.effective_content(&s.grove).is_none(),
        "Effective content should be None when wrapping another cursor on a hole"
    );

    s.insert(Constructor::Zero);
    s.assert_invariants();
    assert!(s.cursor_content_id().is_some(), "Insert through other cursor should succeed");
}

#[test]
fn two_cursors_at_same_location() {
    let mut s = Scenario::new();
    s.insert(Constructor::Zero);

    let root_id = s.root_id();
    let cursor2_id = Uuid::new_v4();
    let identity2_id = Uuid::new_v4();

    s.apply_patches(&[
        grove::birth_patch(
            root_id, GroveConstructor::Root, 0,
            cursor2_id, GroveConstructor::Lang(Constructor::Cursor),
        ),
        grove::birth_patch(
            cursor2_id, GroveConstructor::Lang(Constructor::Cursor), 0,
            identity2_id, GroveConstructor::Lang(Constructor::Identifier("user2".into())),
        ),
    ]);
    s.blossom.update_all(&s.grove);

    s.assert_renders_ok();
    s.assert_cursor_valid();
}

// ── SyncScenario: movement-preserves-AST ─────────────────────────────────────

#[test]
fn sync_move_up_from_conflict_preserves_ast() {
    let mut s = setup_sync_conflict();
    s.assert_alice_movement_preserves_ast(&Action::Move(Direction::Up));
}

#[test]
fn sync_move_down_into_conflict_preserves_ast() {
    let mut s = setup_sync_conflict();
    s.alice_up();
    s.assert_alice_movement_preserves_ast(&Action::Move(Direction::Down));
}

#[test]
fn sync_move_down_through_conflict_child_preserves_ast() {
    let mut s = setup_sync_conflict();
    s.alice_up();
    s.alice_down();
    s.assert_alice_movement_preserves_ast(&Action::Move(Direction::Down));
}

#[test]
fn sync_move_right_from_conflict_preserves_ast() {
    let mut s = setup_sync_conflict();
    s.assert_alice_movement_preserves_ast(&Action::Move(Direction::Right));
}

#[test]
fn sync_move_left_into_conflict_preserves_ast() {
    let mut s = setup_sync_conflict();
    s.alice_right();
    s.assert_alice_movement_preserves_ast(&Action::Move(Direction::Left));
}

#[test]
fn sync_up_down_roundtrip_preserves_ast() {
    let mut s = setup_sync_conflict();
    let snapshot = s.alice.ast_snapshot();
    s.alice_up();
    s.alice_down();
    s.alice_down();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Up-down-down roundtrip changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_right_left_roundtrip_preserves_ast() {
    let mut s = setup_sync_conflict();
    let snapshot = s.alice.ast_snapshot();
    s.alice_right();
    s.alice_left();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Right-left roundtrip changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_exhaustive_navigation_preserves_ast() {
    let mut s = setup_sync_conflict();
    let snapshot = s.alice.ast_snapshot();
    s.alice_up();
    s.alice_down();
    s.alice_down();
    s.alice_right();
    s.alice_up();
    s.alice_up();
    s.alice_down();
    s.alice_left();
    s.alice_down();
    s.alice_up();
    s.alice_right();
    s.alice_left();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Exhaustive navigation changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_both_users_move_preserves_ast() {
    let mut s = setup_sync_conflict();
    s.assert_bob_movement_preserves_ast(&Action::Move(Direction::Up));
    s.assert_alice_movement_preserves_ast(&Action::Move(Direction::Up));
}

#[test]
fn sync_conflict_at_both_siblings_preserves_ast() {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Plus);
    s.desync();
    s.alice_text("x");
    s.alice_right();
    s.alice_text("a");
    s.bob_down();
    s.bob_down();
    s.bob_text("y");
    s.bob_right();
    s.bob_text("b");
    s.sync();

    let snapshot = s.alice.ast_snapshot();
    s.alice_left();
    s.alice_up();
    s.alice_down();
    s.alice_right();
    s.alice_up();
    s.alice_up();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Navigation across dual conflicts changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_multiple_rounds_accumulate_conflicts() {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Plus);

    s.desync();
    s.alice_text("x");
    s.bob_down();
    s.bob_down();
    s.bob_text("y");
    s.sync();

    s.desync();
    s.alice_right();
    s.alice_text("a");
    s.bob_right();
    s.bob_text("b");
    s.sync();

    let snapshot = s.alice.ast_snapshot();
    s.alice_left();
    s.alice_up();
    s.alice_up();
    s.alice_down();
    s.alice_down();
    s.alice_right();
    s.alice_up();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Navigation after two sync rounds changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_deep_nested_conflict_preserves_ast() {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Plus);
    s.alice_wrap_left(Constructor::Arrow);

    s.desync();
    s.alice_text("x");
    s.bob_down();
    s.bob_down();
    s.bob_down();
    s.bob_text("y");
    s.sync();

    let snapshot = s.alice.ast_snapshot();
    s.alice_up();
    s.alice_up();
    s.alice_up();
    s.alice_down();
    s.alice_down();
    s.alice_down();
    s.alice_right();
    s.alice_left();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Navigation through deep nested conflict changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_concurrent_movement_preserves_ast() {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Plus);
    s.alice_text("x");

    s.desync();
    s.alice_right();
    s.bob_up();
    s.bob_up();
    s.sync();

    let alice_ast = s.alice.ast_snapshot();
    let bob_ast = s.bob.ast_snapshot();
    assert_eq!(
        alice_ast, bob_ast,
        "After syncing concurrent movements, Alice and Bob disagree on AST!\n\
         Alice:\n{}\nBob:\n{}",
        s.alice.tree_string(), s.bob.tree_string()
    );
}

#[test]
fn sync_concurrent_move_and_edit_then_navigate() {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Pair);

    s.desync();
    s.alice_text("a");
    s.bob_down();
    s.bob_down();
    s.bob_right();
    s.bob_text("b");
    s.sync();

    let snapshot = s.alice.ast_snapshot();
    s.alice_right();
    s.alice_up();
    s.alice_up();
    s.alice_down();
    s.alice_left();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Navigation after concurrent move+edit changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_conflict_arity0_preserves_ast() {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Plus);

    s.desync();
    s.alice_insert(Constructor::Zero);
    s.bob_down();
    s.bob_down();
    s.bob_insert(Constructor::Zero);
    s.sync();

    let snapshot = s.alice.ast_snapshot();
    s.alice_up();
    s.alice_up();
    s.alice_down();
    s.alice_down();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Navigation through Zero conflict changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_wrap_left_over_conflict_preserves_conflict() {
    let mut s = setup_sync_conflict();
    s.alice_wrap_left(Constructor::Asc);

    let snapshot = s.alice.ast_snapshot();
    s.alice_up();
    s.alice_up();
    s.alice_down();
    s.alice_down();
    s.alice_down();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Navigation after wrapping a conflict changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_three_way_conflict_preserves_ast() {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Plus);

    s.desync();
    s.alice_text("x");
    s.bob_down();
    s.bob_down();
    s.bob_text("y");
    s.sync();

    let snapshot = s.alice.ast_snapshot();
    s.alice_up();
    s.alice_up();
    s.alice_down();
    s.alice_right();
    s.alice_left();
    s.alice_up();
    s.alice_down();
    s.alice_down();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Navigation in three-way conflict changed the AST!\nAlice:\n{}", s.alice.tree_string());
}

#[test]
fn sync_receiver_ast_unchanged_after_remote_movement() {
    let mut s = setup_sync_conflict();
    let bob_snapshot = s.bob.ast_snapshot();
    s.alice_up();
    let bob_after = s.bob.ast_snapshot();
    assert_eq!(bob_snapshot, bob_after, "Alice's move-up changed Bob's cursor-erased AST!\nBob:\n{}", s.bob.tree_string());
}

#[test]
fn sync_interleaved_navigation_preserves_ast() {
    let mut s = setup_sync_conflict();
    let snapshot = s.alice.ast_snapshot();
    s.alice_up();
    s.bob_up();
    s.alice_down();
    s.bob_down();
    s.alice_right();
    s.bob_left();
    s.alice_up();
    s.bob_up();
    s.alice_left();
    s.bob_right();
    s.alice_down();
    s.bob_down();
    let alice_after = s.alice.ast_snapshot();
    let bob_after = s.bob.ast_snapshot();
    assert_eq!(snapshot, alice_after, "Interleaved navigation changed Alice's AST!\nAlice:\n{}", s.alice.tree_string());
    assert_eq!(snapshot, bob_after, "Interleaved navigation changed Bob's AST!\nBob:\n{}", s.bob.tree_string());
}

#[test]
fn sync_let_conflict_preserves_ast() {
    let mut s = SyncScenario::new();
    s.alice_wrap_left(Constructor::Let);
    s.alice_right();

    s.desync();
    s.alice_text("a");
    s.bob_down();
    s.bob_down();
    s.bob_right();
    s.bob_text("b");
    s.sync();

    let snapshot = s.alice.ast_snapshot();
    s.alice_right();
    s.alice_left();
    s.alice_left();
    s.alice_up();
    s.alice_up();
    s.alice_down();
    s.alice_down();
    s.alice_right();
    let after = s.alice.ast_snapshot();
    assert_eq!(snapshot, after, "Navigation around Let with conflict changed the AST!\nAlice:\n{}", s.alice.tree_string());
}
