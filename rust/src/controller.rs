use uuid::Uuid;

use crate::grove::{
    birth_patch, connect_patch, create_node_patch, kill_patch, Grove, Location, Patch,
};
use crate::lang::{Constructor, GroveConstructor};

/// Derived cursor state — everything computed from the identity node.
pub struct CursorState {
    /// The Cursor node itself.
    pub cursor_node: Uuid,
    /// Where the cursor sits in the tree (parent location of cursor node).
    pub cursor_location: Location,
    /// The content wrapped by the cursor (child at Cursor[1]), if any.
    pub content: Option<Uuid>,
}

/// Controller: translates user actions into grove patches.
pub struct Controller {
    /// Stable identity node at Cursor[0] — never moves.
    pub cursor_identity: Option<Uuid>,
    /// The Cursor node — cached for convenience.
    pub cursor_node: Option<Uuid>,
    /// Local clipboard (node UUID of cut content).
    pub clipboard: Option<Uuid>,
}

impl Controller {
    pub fn new() -> Self {
        Controller {
            cursor_identity: None,
            cursor_node: None,
            clipboard: None,
        }
    }

    /// Derive full cursor state from the identity node.
    pub fn cursor_state(&self, grove: &Grove) -> Option<CursorState> {
        let _identity_id = self.cursor_identity?;
        let cursor_node = self.cursor_node?;

        let cursor_location = grove.parent_location(cursor_node)?;

        let content_loc = Location {
            node: cursor_node,
            position: 1,
        };
        let children = grove.live_children_at(&content_loc);
        let content = children.first().copied();

        Some(CursorState {
            cursor_node,
            cursor_location,
            content,
        })
    }

    // ── Initialization ───────────────────────────────────────────────────────

    /// Initialize the cursor. Searches for existing cursor with matching session
    /// ID, or creates a new one. Returns patches to apply.
    pub fn init_cursor(&mut self, session_id: &str, grove: &Grove) -> Vec<Patch> {
        // Search for existing Cursor nodes
        for node in grove.nodes.values() {
            if let GroveConstructor::Lang(Constructor::Cursor) = &node.constructor {
                // Check if identity (position 0) matches session_id
                let id_loc = Location {
                    node: node.id,
                    position: 0,
                };
                let id_children = grove.live_children_at(&id_loc);
                for &child_id in &id_children {
                    if let Some(child_node) = grove.node(child_id) {
                        if let GroveConstructor::Lang(Constructor::Identifier(ref name)) =
                            child_node.constructor
                        {
                            if name == session_id {
                                // Found our cursor
                                self.cursor_identity = Some(child_id);
                                self.cursor_node = Some(node.id);
                                return Vec::new();
                            }
                        }
                    }
                }
            }
        }

        // Not found — create new cursor at Root[0]
        let root_id = match grove.root_id {
            Some(id) => id,
            None => return Vec::new(),
        };

        let cursor_id = Uuid::new_v4();
        let identity_id = Uuid::new_v4();
        let root_loc = Location {
            node: root_id,
            position: 0,
        };

        let cursor_constructor = GroveConstructor::Lang(Constructor::Cursor);
        let identity_constructor =
            GroveConstructor::Lang(Constructor::Identifier(session_id.to_string()));

        let mut patches = Vec::new();

        // Check if Root[0] already has content
        let existing = grove.live_children_at(&root_loc);

        // Create cursor at Root[0]
        // Patch 1: Root[0] → Cursor (creates cursor node)
        patches.push(birth_patch(
            root_id,
            GroveConstructor::Root,
            0,
            cursor_id,
            cursor_constructor.clone(),
        ));

        // Patch 2: Cursor[0] → Identity (creates identity node)
        patches.push(birth_patch(
            cursor_id,
            cursor_constructor.clone(),
            0,
            identity_id,
            identity_constructor,
        ));

        // If Root[0] had content, wrap it into Cursor[1]
        if let Some(&existing_id) = existing.first() {
            // Kill old edge: Root[0] → existing
            if let Some(edge) = grove.edge_from_loc_to(&root_loc, existing_id) {
                patches.push(kill_patch(edge, grove));
            }
            // Connect: Cursor[1] → existing
            // At this point cursor_id is "new" (will be created by patch 1), so use birth_patch
            patches.push(birth_patch(
                cursor_id,
                cursor_constructor,
                1,
                existing_id,
                grove.nodes[&existing_id].constructor.clone(),
            ));
        }

        self.cursor_identity = Some(identity_id);
        self.cursor_node = Some(cursor_id);

        patches
    }

    // ── Primitives (section 9.3) ─────────────────────────────────────────────

    /// Unwrap: move content from Cursor[1] to cursor's parent location.
    fn unwrap_patches(&self, cs: &CursorState, grove: &Grove) -> Vec<Patch> {
        let content_id = match cs.content {
            Some(id) => id,
            None => return Vec::new(),
        };

        let content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };
        let content_edge = match grove.edge_from_loc_to(&content_loc, content_id) {
            Some(e) => e,
            None => return Vec::new(),
        };

        vec![
            kill_patch(content_edge, grove),
            connect_patch(&cs.cursor_location, content_id, grove),
        ]
    }

    /// Move: relocate cursor node to a new location.
    fn move_patches(&self, cs: &CursorState, new_loc: &Location, grove: &Grove) -> Vec<Patch> {
        let cursor_edge = match grove.edge_from_loc_to(&cs.cursor_location, cs.cursor_node) {
            Some(e) => e,
            None => return Vec::new(),
        };

        vec![
            kill_patch(cursor_edge, grove),
            connect_patch(new_loc, cs.cursor_node, grove),
        ]
    }

    /// Wrap: move content at a location into Cursor[1].
    fn wrap_patches(&self, cs: &CursorState, loc: &Location, grove: &Grove) -> Vec<Patch> {
        let children = grove.live_children_at(loc);
        let content_id = match children.first() {
            Some(&id) => id,
            None => return Vec::new(),
        };

        // Safety: never wrap the cursor node into itself
        if content_id == cs.cursor_node {
            return Vec::new();
        }

        let content_edge = match grove.edge_from_loc_to(loc, content_id) {
            Some(e) => e,
            None => return Vec::new(),
        };

        let cursor_content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };

        vec![
            kill_patch(content_edge, grove),
            connect_patch(&cursor_content_loc, content_id, grove),
        ]
    }

    // ── Movements (section 9.5) ──────────────────────────────────────────────

    pub fn move_up(&self, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        // target = the node the cursor is inside
        let target_node = cs.cursor_location.node;
        let grandparent_loc = match grove.parent_location(target_node) {
            Some(loc) => loc,
            None => return Vec::new(), // at root
        };

        let mut patches = Vec::new();
        patches.extend(self.unwrap_patches(&cs, grove));
        patches.extend(self.move_patches(&cs, &grandparent_loc, grove));
        patches.extend(self.wrap_patches(&cs, &grandparent_loc, grove));
        patches
    }

    pub fn move_down(&self, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        let content_id = match cs.content {
            Some(id) => id,
            None => return Vec::new(),
        };

        // Check if content is a reference (grove root with 2+ parents)
        if grove.is_grove_root(content_id) && grove.node(content_id).is_some() {
            return Vec::new();
        }

        let content_node = match grove.node(content_id) {
            Some(n) => n,
            None => return Vec::new(),
        };

        if content_node.arity == 0 {
            return Vec::new();
        }

        // Target position: 1 for Proj/Cursor (skip metadata), 0 otherwise
        let target_pos = match content_node.constructor.constructor() {
            Some(c) if c.is_transparent() => 1,
            _ => 0,
        };

        let target_loc = Location {
            node: content_id,
            position: target_pos,
        };

        let mut patches = Vec::new();
        patches.extend(self.unwrap_patches(&cs, grove));
        patches.extend(self.move_patches(&cs, &target_loc, grove));
        // If target has a child, wrap it
        patches.extend(self.wrap_patches(&cs, &target_loc, grove));
        patches
    }

    fn move_sibling(&self, delta: i8, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        let current_loc = &cs.cursor_location;
        let parent_node = match grove.node(current_loc.node) {
            Some(n) => n,
            None => return Vec::new(),
        };

        let arity = parent_node.arity;
        if arity <= 1 {
            return Vec::new();
        }

        let is_transparent = parent_node
            .constructor
            .constructor()
            .map(|c| c.is_transparent())
            .unwrap_or(false);

        let mut next_pos =
            (current_loc.position as i16 + delta as i16).rem_euclid(arity as i16) as u8;

        // Skip protected position 0 for transparent wrappers
        if is_transparent && next_pos == 0 {
            next_pos = if delta > 0 {
                1 % arity
            } else {
                arity - 1
            };
        }

        if next_pos == current_loc.position {
            return Vec::new();
        }

        let next_loc = Location {
            node: current_loc.node,
            position: next_pos,
        };

        let mut patches = Vec::new();
        patches.extend(self.unwrap_patches(&cs, grove));
        patches.extend(self.move_patches(&cs, &next_loc, grove));
        patches.extend(self.wrap_patches(&cs, &next_loc, grove));
        patches
    }

    pub fn move_right(&self, grove: &Grove) -> Vec<Patch> {
        self.move_sibling(1, grove)
    }

    pub fn move_left(&self, grove: &Grove) -> Vec<Patch> {
        self.move_sibling(-1, grove)
    }

    pub fn move_to_term(&self, term_id: Uuid, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        // Guard: don't move cursor to itself
        if term_id == cs.cursor_node {
            return Vec::new();
        }

        // Guard: don't move to cursor's identity node
        if Some(term_id) == self.cursor_identity {
            return Vec::new();
        }

        // Guard: if term is already the cursor's content, it's already selected
        if Some(term_id) == cs.content {
            return Vec::new();
        }

        // Guard: don't move into another cursor's content
        if self.is_inside_other_cursor(term_id, grove) {
            return Vec::new();
        }

        let target_loc = match grove.parent_location(term_id) {
            Some(loc) => loc,
            None => return Vec::new(),
        };

        // Guard: don't move to cursor's own child slots
        if target_loc.node == cs.cursor_node {
            return Vec::new();
        }

        let mut patches = Vec::new();
        patches.extend(self.unwrap_patches(&cs, grove));
        patches.extend(self.move_patches(&cs, &target_loc, grove));
        // Wrap the term
        patches.extend(self.wrap_patches(&cs, &target_loc, grove));
        patches
    }

    pub fn move_to_location(&self, loc: Location, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        // Guard: already at this location
        if loc == cs.cursor_location {
            return Vec::new();
        }

        // Guard: don't move to cursor's own child slots
        if loc.node == cs.cursor_node {
            return Vec::new();
        }

        // Guard: don't enter another cursor's content
        if self.is_inside_other_cursor(loc.node, grove) {
            return Vec::new();
        }

        let mut patches = Vec::new();
        patches.extend(self.unwrap_patches(&cs, grove));
        patches.extend(self.move_patches(&cs, &loc, grove));
        // If location has a child, wrap it
        patches.extend(self.wrap_patches(&cs, &loc, grove));
        patches
    }

    fn is_inside_other_cursor(&self, node_id: Uuid, grove: &Grove) -> bool {
        let own_cursor = match self.cursor_node {
            Some(id) => id,
            None => return false,
        };

        let mut current = node_id;
        for _ in 0..1000 {
            if let Some(parent_edge) = grove.unique_parent_edge(current) {
                let parent_node_id = parent_edge.source.node;
                if parent_node_id == own_cursor {
                    return false; // Inside own cursor is OK
                }
                if let Some(parent) = grove.node(parent_node_id) {
                    if let GroveConstructor::Lang(Constructor::Cursor) = &parent.constructor {
                        // Inside another cursor's content (position 1)
                        if parent_edge.source.position == 1 {
                            return true;
                        }
                    }
                }
                current = parent_node_id;
            } else {
                break;
            }
        }
        false
    }

    // ── Editing actions (section 12) ─────────────────────────────────────────

    pub fn wrap_left(&self, constructor: Constructor, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        let new_id = Uuid::new_v4();
        let new_constructor = GroveConstructor::Lang(constructor.clone());
        let cursor_content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };

        let mut patches = Vec::new();

        // Create new node at Cursor[1]
        patches.push(create_node_patch(
            &cursor_content_loc,
            new_id,
            new_constructor.clone(),
            grove,
        ));

        // If cursor had content, move it to position 0 of new node
        if let Some(content_id) = cs.content {
            let content_edge =
                grove.edge_from_loc_to(&cursor_content_loc, content_id);
            if let Some(edge) = content_edge {
                patches.push(kill_patch(edge, grove));
            }
            // Connect content to new_node[0]
            patches.push(birth_patch(
                new_id,
                new_constructor,
                0,
                content_id,
                grove.nodes[&content_id].constructor.clone(),
            ));
        }

        patches
    }

    pub fn wrap_right(&self, constructor: Constructor, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        let new_id = Uuid::new_v4();
        let new_constructor = GroveConstructor::Lang(constructor.clone());
        let cursor_content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };

        let mut patches = Vec::new();

        // Create new node at Cursor[1]
        patches.push(create_node_patch(
            &cursor_content_loc,
            new_id,
            new_constructor.clone(),
            grove,
        ));

        // If cursor had content, move it to position 1 of new node
        if let Some(content_id) = cs.content {
            let content_edge =
                grove.edge_from_loc_to(&cursor_content_loc, content_id);
            if let Some(edge) = content_edge {
                patches.push(kill_patch(edge, grove));
            }
            patches.push(birth_patch(
                new_id,
                new_constructor,
                1,
                content_id,
                grove.nodes[&content_id].constructor.clone(),
            ));
        }

        patches
    }

    pub fn insert(&self, constructor: Constructor, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        // Only works when cursor content is empty
        if cs.content.is_some() {
            return Vec::new();
        }

        let new_id = Uuid::new_v4();
        let cursor_content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };

        vec![create_node_patch(
            &cursor_content_loc,
            new_id,
            GroveConstructor::Lang(constructor),
            grove,
        )]
    }

    pub fn delete(&self, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        let content_id = match cs.content {
            Some(id) => id,
            None => return Vec::new(),
        };

        let content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };

        match grove.edge_from_loc_to(&content_loc, content_id) {
            Some(edge) => vec![kill_patch(edge, grove)],
            None => Vec::new(),
        }
    }

    /// Cut: mark cursor's content as clipboard. No patches — node stays in place,
    /// rendered with clipboard highlight until paste.
    pub fn cut(&mut self, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        match cs.content {
            Some(id) => {
                self.clipboard = Some(id);
            }
            None => {}
        }

        // No patches — purely local state
        Vec::new()
    }

    /// Paste: move the clipboard node to Cursor[1]. Disconnects from its current
    /// location and reconnects at cursor. Two patches.
    pub fn paste(&mut self, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        // Only paste into empty cursor
        if cs.content.is_some() {
            return Vec::new();
        }

        let clip_id = match self.clipboard.take() {
            Some(id) => id,
            None => return Vec::new(),
        };

        let mut patches = Vec::new();

        // Disconnect clipboard node from its current parent (if it has one)
        if let Some(parent_edge) = grove.unique_parent_edge(clip_id) {
            patches.push(kill_patch(parent_edge, grove));
        }

        // Connect to Cursor[1]
        let cursor_content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };
        patches.push(connect_patch(&cursor_content_loc, clip_id, grove));

        patches
    }

    pub fn text_insert(&self, ch: &str, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        let cursor_content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };

        let new_name = match cs.content {
            Some(content_id) => {
                // If current content is an Identifier, extend it
                match grove.node(content_id) {
                    Some(node) => {
                        if let GroveConstructor::Lang(Constructor::Identifier(ref name)) =
                            node.constructor
                        {
                            format!("{}{}", name, ch)
                        } else {
                            return Vec::new(); // Not an identifier, can't text-insert
                        }
                    }
                    None => return Vec::new(),
                }
            }
            None => {
                // Empty cursor — create new single-character identifier
                ch.to_string()
            }
        };

        let new_id = Uuid::new_v4();
        let new_constructor = GroveConstructor::Lang(Constructor::Identifier(new_name));

        let mut patches = Vec::new();

        // Delete old identifier if present
        if let Some(content_id) = cs.content {
            if let Some(edge) = grove.edge_from_loc_to(&cursor_content_loc, content_id) {
                patches.push(kill_patch(edge, grove));
            }
        }

        // Create new identifier
        patches.push(create_node_patch(
            &cursor_content_loc,
            new_id,
            new_constructor,
            grove,
        ));

        patches
    }

    pub fn text_backspace(&self, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        let content_id = match cs.content {
            Some(id) => id,
            None => return Vec::new(),
        };

        let node = match grove.node(content_id) {
            Some(n) => n,
            None => return Vec::new(),
        };

        let name = match &node.constructor {
            GroveConstructor::Lang(Constructor::Identifier(name)) => name.clone(),
            _ => return Vec::new(),
        };

        let cursor_content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };

        let mut patches = Vec::new();

        // Delete old identifier
        if let Some(edge) = grove.edge_from_loc_to(&cursor_content_loc, content_id) {
            patches.push(kill_patch(edge, grove));
        }

        // If length > 1, create shorter identifier
        if name.len() > 1 {
            let new_name = &name[..name.len() - 1];
            let new_id = Uuid::new_v4();
            patches.push(create_node_patch(
                &cursor_content_loc,
                new_id,
                GroveConstructor::Lang(Constructor::Identifier(new_name.to_string())),
                grove,
            ));
        }

        patches
    }

    pub fn wrap_with_projector(&self, proj_type_constructor: Constructor, grove: &Grove) -> Vec<Patch> {
        let cs = match self.cursor_state(grove) {
            Some(cs) => cs,
            None => return Vec::new(),
        };

        let proj_id = Uuid::new_v4();
        let proj_type_id = Uuid::new_v4();
        let proj_constructor = GroveConstructor::Lang(Constructor::Proj);
        let type_constructor = GroveConstructor::Lang(proj_type_constructor.clone());

        let cursor_content_loc = Location {
            node: cs.cursor_node,
            position: 1,
        };

        let mut patches = Vec::new();

        // Create Proj node at Cursor[1]
        patches.push(create_node_patch(
            &cursor_content_loc,
            proj_id,
            proj_constructor.clone(),
            grove,
        ));

        // Create projector type at Proj[0]
        patches.push(birth_patch(
            proj_id,
            proj_constructor.clone(),
            0,
            proj_type_id,
            type_constructor,
        ));

        // For Canvas: create PosNil at Canvas[0]
        if proj_type_constructor == Constructor::Canvas {
            let posnil_id = Uuid::new_v4();
            patches.push(birth_patch(
                proj_type_id,
                GroveConstructor::Lang(Constructor::Canvas),
                0,
                posnil_id,
                GroveConstructor::Lang(Constructor::PosNil),
            ));
        }

        // For Labeled: create default label Identifier at Labeled[0]
        if proj_type_constructor == Constructor::Labeled {
            let label_id = Uuid::new_v4();
            patches.push(birth_patch(
                proj_type_id,
                GroveConstructor::Lang(Constructor::Labeled),
                0,
                label_id,
                GroveConstructor::Lang(Constructor::Identifier("label".into())),
            ));
        }

        // Move cursor's old content to Proj[1]
        if let Some(content_id) = cs.content {
            if let Some(edge) = grove.edge_from_loc_to(&cursor_content_loc, content_id) {
                patches.push(kill_patch(edge, grove));
            }
            patches.push(birth_patch(
                proj_id,
                proj_constructor,
                1,
                content_id,
                grove.nodes[&content_id].constructor.clone(),
            ));
        }

        patches
    }

    // ── Canvas position update ─────────────────────────────────────────────

    /// Rebuild the PosCons chain at Canvas[0] with new positions.
    /// Orphans the old chain (grove never GCs).
    pub fn canvas_drag(
        &self,
        canvas_id: Uuid,
        positions: &[crate::CanvasPos],
        grove: &Grove,
    ) -> Vec<Patch> {
        let canvas_node = match grove.node(canvas_id) {
            Some(n) => n,
            None => return Vec::new(),
        };
        // Verify it's a Canvas
        if canvas_node.constructor != GroveConstructor::Lang(Constructor::Canvas) {
            return Vec::new();
        }

        let chain_loc = Location {
            node: canvas_id,
            position: 0,
        };
        let mut patches = Vec::new();

        // Kill old chain root
        for &old_child in grove.live_children_at(&chain_loc).iter() {
            if let Some(edge) = grove.edge_from_loc_to(&chain_loc, old_child) {
                patches.push(kill_patch(edge, grove));
            }
        }

        // Build new chain top-down
        let gl = |c: Constructor| GroveConstructor::Lang(c);
        let mut parent_id = canvas_id;
        let mut parent_constructor = canvas_node.constructor.clone();
        let mut parent_pos: u8 = 0;

        for (i, pos) in positions.iter().enumerate() {
            let cons_id = Uuid::new_v4();
            let cons_c = gl(Constructor::PosCons);

            // Connect parent → PosCons
            patches.push(birth_patch(
                parent_id, parent_constructor.clone(), parent_pos,
                cons_id, cons_c.clone(),
            ));

            // PosCons[0] → nodeIdent
            patches.push(birth_patch(
                cons_id, cons_c.clone(), 0,
                Uuid::new_v4(), gl(Constructor::Identifier(pos.node_id.clone())),
            ));
            // PosCons[1] → x
            patches.push(birth_patch(
                cons_id, cons_c.clone(), 1,
                Uuid::new_v4(), gl(Constructor::Identifier(pos.x.to_string())),
            ));
            // PosCons[2] → y
            patches.push(birth_patch(
                cons_id, cons_c.clone(), 2,
                Uuid::new_v4(), gl(Constructor::Identifier(pos.y.to_string())),
            ));

            // Next entry chains from PosCons[3]
            parent_id = cons_id;
            parent_constructor = cons_c;
            parent_pos = 3;
        }

        // Terminate with PosNil
        patches.push(birth_patch(
            parent_id, parent_constructor, parent_pos,
            Uuid::new_v4(), gl(Constructor::PosNil),
        ));

        patches
    }

    // ── Auto-advance after wrap ──────────────────────────────────────────────

    /// After WrapLeft/WrapRight, auto-advance Down into the new node.
    pub fn auto_advance_down(&self, grove: &Grove) -> Vec<Patch> {
        self.move_down(grove)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grove::{birth_patch, Grove};

    fn setup_root() -> (Grove, Uuid) {
        let mut grove = Grove::new();
        let root_id = Uuid::new_v4();
        grove.apply_patch(&birth_patch(
            root_id,
            GroveConstructor::Root,
            0,
            Uuid::new_v4(),
            GroveConstructor::Lang(Constructor::Zero),
        ));
        assert!(grove.root_id.is_some());
        let rid = grove.root_id.unwrap();
        (grove, rid)
    }

    #[test]
    fn test_cursor_init() {
        let (mut grove, root_id) = setup_root();
        let mut ctrl = Controller::new();

        let patches = ctrl.init_cursor("test-session", &grove);
        assert!(!patches.is_empty(), "Should generate cursor creation patches");

        // Apply patches
        for patch in &patches {
            grove.apply_patch(patch);
        }

        assert!(ctrl.cursor_identity.is_some());
        assert!(ctrl.cursor_node.is_some());

        // Cursor should be at Root[0]
        let cs = ctrl.cursor_state(&grove).unwrap();
        assert_eq!(cs.cursor_location.node, root_id);
        assert_eq!(cs.cursor_location.position, 0);
    }

    #[test]
    fn test_insert_and_delete() {
        let (mut grove, root_id) = setup_root();
        let mut ctrl = Controller::new();

        // Kill the dummy Zero that was at Root[0]
        let root_loc = Location {
            node: root_id,
            position: 0,
        };
        let children = grove.live_children_at(&root_loc);
        for child in children {
            if let Some(edge) = grove.edge_from_loc_to(&root_loc, child) {
                grove.apply_patch(&kill_patch(edge, &grove));
            }
        }

        // Init cursor
        let patches = ctrl.init_cursor("test", &grove);
        for p in &patches {
            grove.apply_patch(p);
        }

        // Insert Zero
        let patches = ctrl.insert(Constructor::Zero, &grove);
        for p in &patches {
            grove.apply_patch(p);
        }

        let cs = ctrl.cursor_state(&grove).unwrap();
        assert!(cs.content.is_some(), "Cursor should have content after insert");

        // Delete
        let patches = ctrl.delete(&grove);
        for p in &patches {
            grove.apply_patch(p);
        }

        let cs = ctrl.cursor_state(&grove).unwrap();
        assert!(cs.content.is_none(), "Cursor content should be empty after delete");
    }
}
