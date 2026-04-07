use serde::Serialize;
use uuid::Uuid;

use crate::blossom::Blossom;
use crate::controller::Controller;
use crate::forest::Forest;
use crate::grove::{Grove, Location, Site};
use crate::lang::{Constructor, GroveConstructor};

const MAX_DEPTH: usize = 100;

/// Recursive render tree returned to TypeScript.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind")]
pub enum RenderNode {
    #[serde(rename = "hole")]
    Hole {
        #[serde(rename = "locNode")]
        loc_node: String,
        #[serde(rename = "locPos")]
        loc_pos: u8,
        #[serde(skip_serializing_if = "std::ops::Not::not")]
        dirty: bool,
    },
    #[serde(rename = "term")]
    Term {
        id: String,
        constructor: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        value: Option<String>,
        slots: Vec<RenderSlot>,
        cursor: String,
        clipboard: bool,
        #[serde(skip_serializing_if = "std::ops::Not::not")]
        dirty: bool,
        #[serde(skip_serializing_if = "Option::is_none")]
        sort: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        ana: Option<Box<RenderNode>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        syn: Option<Box<RenderNode>>,
        marks: Vec<RenderMark>,
    },
    #[serde(rename = "conflict")]
    Conflict {
        children: Vec<RenderNode>,
        #[serde(rename = "locNode")]
        loc_node: String,
        #[serde(rename = "locPos")]
        loc_pos: u8,
    },
    #[serde(rename = "ref")]
    Reference { id: String },
}

#[derive(Clone, Debug, Serialize)]
pub struct RenderSlot {
    pub position: u8,
    pub content: RenderNode,
}

/// Type info for the cursor's current site, displayed in the inspector.
#[derive(Clone, Debug, Serialize)]
pub struct CursorInfo {
    pub sort: Option<String>,
    pub ana: Option<RenderNode>,
    pub syn: Option<RenderNode>,
    pub marks: Vec<RenderMark>,
    #[serde(rename = "hasContent")]
    pub has_content: bool,
    #[serde(rename = "contentConstructor")]
    pub content_constructor: Option<String>,
}

/// A rendered error mark.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind")]
pub enum RenderMark {
    #[serde(rename = "sort")]
    SortInconsistent {
        expected: String,
        actual: String,
    },
    #[serde(rename = "type")]
    TypeInconsistent {
        expected: RenderNode,
        actual: RenderNode,
    },
}

/// Render the full tree from root.
pub fn render_tree(
    grove: &Grove,
    forest: &Forest,
    blossom: &Blossom,
    controller: &Controller,
) -> RenderNode {
    let root_id = match grove.root_id {
        Some(id) => id,
        None => {
            return RenderNode::Hole {
                loc_node: String::new(),
                loc_pos: 0,
                dirty: false,
            }
        }
    };

    let root_loc = Location {
        node: root_id,
        position: 0,
    };
    render_location(&root_loc, grove, forest, blossom, controller, 0)
}

/// Render a location (child slot).
fn render_location(
    loc: &Location,
    grove: &Grove,
    forest: &Forest,
    blossom: &Blossom,
    controller: &Controller,
    depth: usize,
) -> RenderNode {
    if depth > MAX_DEPTH {
        return RenderNode::Hole {
            loc_node: loc.node.to_string(),
            loc_pos: loc.position,
            dirty: false,
        };
    }

    let loc_dirty = blossom.is_site_dirty(&Site::Loc(loc.clone()), forest);
    let children = grove.live_children_at(loc);

    match children.len() {
        0 => RenderNode::Hole {
            loc_node: loc.node.to_string(),
            loc_pos: loc.position,
            dirty: loc_dirty,
        },
        1 => render_term(children[0], grove, forest, blossom, controller, depth),
        _ => {
            let rendered: Vec<RenderNode> = children
                .iter()
                .map(|&id| render_term(id, grove, forest, blossom, controller, depth))
                .collect();
            RenderNode::Conflict {
                children: rendered,
                loc_node: loc.node.to_string(),
                loc_pos: loc.position,
            }
        }
    }
}

/// Render a term (node).
fn render_term(
    node_id: Uuid,
    grove: &Grove,
    forest: &Forest,
    blossom: &Blossom,
    controller: &Controller,
    depth: usize,
) -> RenderNode {
    // Forest handles cycle/reference detection
    if forest.is_reference(node_id, grove) {
        return RenderNode::Reference {
            id: node_id.to_string(),
        };
    }

    let node = match grove.node(node_id) {
        Some(n) => n,
        None => {
            return RenderNode::Reference {
                id: node_id.to_string(),
            };
        }
    };

    let (constructor_name, value) = match &node.constructor {
        GroveConstructor::Root => ("Root".to_string(), None),
        GroveConstructor::Lang(c) => {
            let name = c.display_name().to_string();
            let val = match c {
                Constructor::Identifier(s) => Some(s.clone()),
                _ => None,
            };
            (name, val)
        }
    };

    // Cursor detection
    let cursor = if Some(node_id) == controller.cursor_node {
        "own".to_string()
    } else if is_other_cursor(node_id, grove) {
        "other".to_string()
    } else {
        "none".to_string()
    };

    let clipboard = controller.clipboard == Some(node_id);
    let dirty = blossom.is_site_dirty(&Site::Term(node_id), forest);

    // Type info from blossom (via forest for TreeSite lookup)
    let attr = blossom.get_attr(&Site::Term(node_id), forest);
    let sort = attr.sort.as_ref().map(|s| format!("{:?}", s));
    let ana = attr.ana.as_ref().map(|t| Box::new(type_to_render(t, grove)));
    let syn = attr.syn.as_ref().map(|t| Box::new(type_to_render(t, grove)));
    let marks: Vec<RenderMark> = attr.marks.iter().map(|m| render_mark(m, grove)).collect();

    // Render children
    let slots: Vec<RenderSlot> = (0..node.arity)
        .map(|pos| {
            let loc = Location {
                node: node_id,
                position: pos,
            };
            let content =
                render_location(&loc, grove, forest, blossom, controller, depth + 1);
            RenderSlot {
                position: pos,
                content,
            }
        })
        .collect();

    RenderNode::Term {
        id: node_id.to_string(),
        constructor: constructor_name,
        value,
        slots,
        cursor,
        clipboard,
        dirty,
        sort,
        ana,
        syn,
        marks,
    }
}

fn is_other_cursor(node_id: Uuid, grove: &Grove) -> bool {
    if let Some(node) = grove.node(node_id) {
        matches!(node.constructor, GroveConstructor::Lang(Constructor::Cursor))
    } else {
        false
    }
}

// ── Type rendering ───────────────────────────────────────────────────────────

fn type_to_render(t: &crate::types::TypeRef, grove: &Grove) -> RenderNode {
    use crate::types::TypeRef;

    let resolved = t.resolve(grove);
    match &resolved {
        TypeRef::Unknown => RenderNode::Hole {
            loc_node: String::new(),
            loc_pos: 0,
            dirty: false,
        },
        TypeRef::Synthetic(constructor, children) => {
            let slots: Vec<RenderSlot> = children
                .iter()
                .enumerate()
                .map(|(i, child)| RenderSlot {
                    position: i as u8,
                    content: type_to_render(child, grove),
                })
                .collect();
            RenderNode::Term {
                id: String::new(),
                constructor: constructor.display_name().to_string(),
                value: None,
                slots,
                cursor: "none".to_string(),
                clipboard: false,
                dirty: false,
                sort: None,
                ana: None,
                syn: None,
                marks: Vec::new(),
            }
        }
        TypeRef::Surface(_) => RenderNode::Hole {
            loc_node: String::new(),
            loc_pos: 0,
            dirty: false,
        },
    }
}

fn render_mark(mark: &crate::types::Mark, grove: &Grove) -> RenderMark {
    use crate::types::Mark;
    match mark {
        Mark::SortInconsistent(expected, actual) => RenderMark::SortInconsistent {
            expected: format!("{:?}", expected),
            actual: format!("{:?}", actual),
        },
        Mark::TypeInconsistent(expected, actual) => RenderMark::TypeInconsistent {
            expected: type_to_render(expected, grove),
            actual: type_to_render(actual, grove),
        },
    }
}

/// Get cursor info for the inspector.
pub fn cursor_info(
    grove: &Grove,
    forest: &Forest,
    blossom: &Blossom,
    controller: &Controller,
) -> CursorInfo {
    let cs = match controller.cursor_state(grove) {
        Some(cs) => cs,
        None => {
            return CursorInfo {
                sort: None,
                ana: None,
                syn: None,
                marks: Vec::new(),
                has_content: false,
                content_constructor: None,
            }
        }
    };

    let loc_attr = blossom.get_attr(&Site::Loc(cs.cursor_location.clone()), forest);

    let (attr, content_constructor) = if let Some(content_id) = cs.content {
        let term_attr = blossom.get_attr(&Site::Term(content_id), forest);
        let ctor = grove.node(content_id).and_then(|n| {
            n.constructor
                .constructor()
                .map(|c| c.display_name().to_string())
        });
        (term_attr, ctor)
    } else {
        (loc_attr, None)
    };

    CursorInfo {
        sort: attr.sort.as_ref().map(|s| format!("{:?}", s)),
        ana: attr.ana.as_ref().map(|t| type_to_render(t, grove)),
        syn: attr.syn.as_ref().map(|t| type_to_render(t, grove)),
        marks: attr.marks.iter().map(|m| render_mark(m, grove)).collect(),
        has_content: cs.content.is_some(),
        content_constructor,
    }
}
