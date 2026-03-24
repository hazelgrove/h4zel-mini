use serde::Serialize;
use uuid::Uuid;

use crate::blossom::Blossom;
use crate::controller::Controller;
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
        #[serde(skip_serializing_if = "Option::is_none")]
        sort: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        ana: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        syn: Option<String>,
        marks: Vec<String>,
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
    pub ana: Option<String>,
    pub syn: Option<String>,
    pub marks: Vec<String>,
    #[serde(rename = "hasContent")]
    pub has_content: bool,
    #[serde(rename = "contentConstructor")]
    pub content_constructor: Option<String>,
}

/// Render the full tree from root.
pub fn render_tree(grove: &Grove, blossom: &Blossom, controller: &Controller) -> RenderNode {
    let root_id = match grove.root_id {
        Some(id) => id,
        None => {
            return RenderNode::Hole {
                loc_node: String::new(),
                loc_pos: 0,
            }
        }
    };

    // Render starting from Root[0]
    let root_loc = Location {
        node: root_id,
        position: 0,
    };
    render_location(&root_loc, grove, blossom, controller, 0)
}

/// Render a location (child slot).
fn render_location(
    loc: &Location,
    grove: &Grove,
    blossom: &Blossom,
    controller: &Controller,
    depth: usize,
) -> RenderNode {
    if depth > MAX_DEPTH {
        return RenderNode::Hole {
            loc_node: loc.node.to_string(),
            loc_pos: loc.position,
        };
    }

    let children = grove.live_children_at(loc);

    match children.len() {
        0 => RenderNode::Hole {
            loc_node: loc.node.to_string(),
            loc_pos: loc.position,
        },
        1 => render_term(children[0], grove, blossom, controller, depth),
        _ => {
            // Conflict: multiple children at one location
            let rendered: Vec<RenderNode> = children
                .iter()
                .map(|&id| render_term(id, grove, blossom, controller, depth))
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
    blossom: &Blossom,
    controller: &Controller,
    depth: usize,
) -> RenderNode {
    // Reference detection: if node has 2+ parents or is in unicycle, it's a reference
    // EXCEPT for the top root and the node's actual tree position
    if grove.is_grove_root(node_id) && grove.live_parent_edge_ids(node_id).len() >= 2 {
        return RenderNode::Reference {
            id: node_id.to_string(),
        };
    }
    if grove.is_in_unicycle(node_id) {
        return RenderNode::Reference {
            id: node_id.to_string(),
        };
    }

    let node = match grove.node(node_id) {
        Some(n) => n,
        None => {
            return RenderNode::Reference {
                id: node_id.to_string(),
            }
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

    // Type info
    let attr = blossom.get_attr(&Site::Term(node_id));
    let sort = attr.sort.as_ref().map(|s| format!("{:?}", s));
    let ana = attr.ana.as_ref().map(|t| t.display(grove));
    let syn = attr.syn.as_ref().map(|t| t.display(grove));
    let marks: Vec<String> = attr.marks.iter().map(|m| format!("{:?}", m)).collect();

    // Render children
    let slots: Vec<RenderSlot> = (0..node.arity)
        .map(|pos| {
            let loc = Location {
                node: node_id,
                position: pos,
            };
            let content = render_location(&loc, grove, blossom, controller, depth + 1);
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

/// Get cursor info for the inspector.
pub fn cursor_info(grove: &Grove, blossom: &Blossom, controller: &Controller) -> CursorInfo {
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

    // Type info at the cursor's location
    let loc_attr = blossom.get_attr(&Site::Loc(cs.cursor_location.clone()));

    // If there's content, use the term's type info
    let (attr, content_constructor) = if let Some(content_id) = cs.content {
        let term_attr = blossom.get_attr(&Site::Term(content_id));
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
        ana: attr.ana.as_ref().map(|t| t.display(grove)),
        syn: attr.syn.as_ref().map(|t| t.display(grove)),
        marks: attr.marks.iter().map(|m| format!("{:?}", m)).collect(),
        has_content: cs.content.is_some(),
        content_constructor,
    }
}
