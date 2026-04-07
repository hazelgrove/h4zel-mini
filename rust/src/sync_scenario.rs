//! Two-user sync test harness.
//!
//! Models the real app: two separate groves (like two browser tabs)
//! exchanging patches through a sync layer (like Automerge).
//! When synced, patches flow immediately. When desynced, they buffer.

use uuid::Uuid;

use crate::blossom::Blossom;
use crate::controller::Controller;
use crate::forest::Forest;
use crate::grove::{Grove, Patch};
use crate::lang::{Constructor, GroveConstructor};
use crate::scenario::Scenario;

pub struct SyncScenario {
    pub alice: Scenario,
    pub bob: Scenario,
    synced: bool,
    alice_outbox: Vec<Patch>,
    bob_outbox: Vec<Patch>,
}

impl SyncScenario {
    /// Create a two-user scenario. Both users share the same root
    /// and each has their own cursor, fully synced.
    pub fn new() -> Self {
        // Build shared genesis root
        let root_id = Uuid::new_v4();

        let mut alice_grove = Grove::new();
        alice_grove.nodes.insert(
            root_id,
            crate::grove::GroveNode {
                id: root_id,
                constructor: GroveConstructor::Root,
                arity: 1,
            },
        );
        alice_grove.root_id = Some(root_id);

        let mut bob_grove = Grove::new();
        bob_grove.nodes.insert(
            root_id,
            crate::grove::GroveNode {
                id: root_id,
                constructor: GroveConstructor::Root,
                arity: 1,
            },
        );
        bob_grove.root_id = Some(root_id);

        let mut alice = Scenario {
            grove: alice_grove,
            forest: Forest::init(root_id),
            blossom: Blossom::new(),
            controller: Controller::new(),
        };

        let mut bob = Scenario {
            grove: bob_grove,
            forest: Forest::init(root_id),
            blossom: Blossom::new(),
            controller: Controller::new(),
        };

        // Alice inits cursor, patches go to Bob
        let alice_cursor_patches = alice.controller.init_cursor("alice", &alice.grove);
        alice.apply_patches(&alice_cursor_patches);
        bob.apply_patches(&alice_cursor_patches);

        // Bob inits cursor, patches go to Alice
        let bob_cursor_patches = bob.controller.init_cursor("bob", &bob.grove);
        bob.apply_patches(&bob_cursor_patches);
        alice.apply_patches(&bob_cursor_patches);

        alice.blossom.update_all(&alice.grove, &alice.forest);
        bob.blossom.update_all(&bob.grove, &bob.forest);

        SyncScenario {
            alice,
            bob,
            synced: true,
            alice_outbox: Vec::new(),
            bob_outbox: Vec::new(),
        }
    }

    /// Perform an action as Alice. Patches are sent to Bob if synced, or buffered.
    pub fn alice_act(&mut self, action: &crate::Action) {
        let patches = self.alice.act(action);
        if self.synced {
            self.bob.apply_patches(&patches);
            self.bob.blossom.update_all(&self.bob.grove, &self.bob.forest);
        } else {
            self.alice_outbox.extend(patches);
        }
    }

    /// Perform an action as Bob. Patches are sent to Alice if synced, or buffered.
    pub fn bob_act(&mut self, action: &crate::Action) {
        let patches = self.bob.act(action);
        if self.synced {
            self.alice.apply_patches(&patches);
            self.alice.blossom.update_all(&self.alice.grove, &self.alice.forest);
        } else {
            self.bob_outbox.extend(patches);
        }
    }

    /// Disable sync. Subsequent actions buffer patches instead of exchanging.
    pub fn desync(&mut self) {
        self.synced = false;
    }

    /// Re-enable sync. Flush all buffered patches in both directions.
    pub fn sync(&mut self) {
        self.synced = true;

        // Alice's buffered patches → Bob
        let alice_patches: Vec<Patch> = self.alice_outbox.drain(..).collect();
        self.bob.apply_patches(&alice_patches);

        // Bob's buffered patches → Alice
        let bob_patches: Vec<Patch> = self.bob_outbox.drain(..).collect();
        self.alice.apply_patches(&bob_patches);

        self.alice.blossom.update_all(&self.alice.grove, &self.alice.forest);
        self.bob.blossom.update_all(&self.bob.grove, &self.bob.forest);
    }

    // ── Shorthand: Alice actions ─────────────────────────────────────────────

    pub fn alice_wrap_left(&mut self, c: Constructor) -> &mut Self {
        self.alice_act(&crate::Action::WrapLeft(c));
        self
    }

    pub fn alice_insert(&mut self, c: Constructor) -> &mut Self {
        self.alice_act(&crate::Action::Insert(c));
        self
    }

    pub fn alice_text(&mut self, s: &str) -> &mut Self {
        for ch in s.chars() {
            self.alice_act(&crate::Action::TextInsert(ch.to_string()));
        }
        self
    }

    pub fn alice_up(&mut self) -> &mut Self {
        self.alice_act(&crate::Action::Move(crate::Direction::Up));
        self
    }

    pub fn alice_down(&mut self) -> &mut Self {
        self.alice_act(&crate::Action::Move(crate::Direction::Down));
        self
    }

    pub fn alice_left(&mut self) -> &mut Self {
        self.alice_act(&crate::Action::Move(crate::Direction::Left));
        self
    }

    pub fn alice_right(&mut self) -> &mut Self {
        self.alice_act(&crate::Action::Move(crate::Direction::Right));
        self
    }

    pub fn alice_delete(&mut self) -> &mut Self {
        self.alice_act(&crate::Action::Delete);
        self
    }

    // ── Shorthand: Bob actions ───────────────────────────────────────────────

    pub fn bob_wrap_left(&mut self, c: Constructor) -> &mut Self {
        self.bob_act(&crate::Action::WrapLeft(c));
        self
    }

    pub fn bob_insert(&mut self, c: Constructor) -> &mut Self {
        self.bob_act(&crate::Action::Insert(c));
        self
    }

    pub fn bob_text(&mut self, s: &str) -> &mut Self {
        for ch in s.chars() {
            self.bob_act(&crate::Action::TextInsert(ch.to_string()));
        }
        self
    }

    pub fn bob_up(&mut self) -> &mut Self {
        self.bob_act(&crate::Action::Move(crate::Direction::Up));
        self
    }

    pub fn bob_down(&mut self) -> &mut Self {
        self.bob_act(&crate::Action::Move(crate::Direction::Down));
        self
    }

    pub fn bob_left(&mut self) -> &mut Self {
        self.bob_act(&crate::Action::Move(crate::Direction::Left));
        self
    }

    pub fn bob_right(&mut self) -> &mut Self {
        self.bob_act(&crate::Action::Move(crate::Direction::Right));
        self
    }

    pub fn bob_delete(&mut self) -> &mut Self {
        self.bob_act(&crate::Action::Delete);
        self
    }

    // ── Assertions ───────────────────────────────────────────────────────────

    /// Assert that an Alice action does not change Alice's cursor-erased AST.
    pub fn assert_alice_movement_preserves_ast(&mut self, action: &crate::Action) {
        let before = self.alice.ast_snapshot();
        self.alice_act(action);
        let after = self.alice.ast_snapshot();
        assert_eq!(
            before, after,
            "Alice's action {:?} changed the cursor-erased AST!\n\
             Alice's tree:\n{}",
            action, self.alice.tree_string()
        );
    }

    /// Assert that a Bob action does not change Bob's cursor-erased AST.
    pub fn assert_bob_movement_preserves_ast(&mut self, action: &crate::Action) {
        let before = self.bob.ast_snapshot();
        self.bob_act(action);
        let after = self.bob.ast_snapshot();
        assert_eq!(
            before, after,
            "Bob's action {:?} changed the cursor-erased AST!\n\
             Bob's tree:\n{}",
            action, self.bob.tree_string()
        );
    }
}
