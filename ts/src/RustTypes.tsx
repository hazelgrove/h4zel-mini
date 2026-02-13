export type Constructor =
    | "Typ"
    | "Num"
    | "Zero"
    | "Plus"
    | "Prod"
    | "Pair"
    | "Arrow"
    | "Fun"
    | "Asc"
    | "Ap"
    | "Let"
    | {Identifier : String}
    // Projector wrapper and types
    | "Proj"
    | "Structural"
    | "Collapsed"
    | "Labeled"
    | "Canvas"
    // Position map for Canvas projector
    | "PosNil"
    | "PosCons"
    // Cursor: wraps selected term in world tree
    | "Cursor"

export type GroveConstructor = 
    | "Root"
    | {Lang : Constructor} 

export type TermConstructor = 
    | {Constructor : GroveConstructor}
    | {Reference : any} // TermEdge

export type ForestAction = 
    | {OpenReference : any} // TermEdge

export type BlossomAction = 
    | {ForestAction: ForestAction}
    | "AllUpdateSteps"
    | "UpdateStep"

// Navigation directions for cursor movement in the term tree.
// - Up: Move toward root (parent)
// - Down: Move toward leaves (first child)
// - Right: Move to next sibling (wraps around to first sibling)
//
// There is no "Left" direction - Right wraps around cyclically through
// siblings. This simplifies the navigation model: three directions suffice
// to reach any position in a tree structure.
export type Direction =
    | "Up"
    | "Down"
    | "Right"

export type Action =
    | {BlossomAction: BlossomAction}
    | {WrapLeft: Constructor}
    | {WrapRight: Constructor}
    | {Insert: Constructor}
    | "Delete"
    | {Move: Direction}
    | "Cut"
    | "Paste"
    | {MoveToLocation: any} // TermLocation
    | {MoveToTerm: any} // Term
    | {TextInsert: String}
    | "TextBackspace"
    | {WrapWithProjector: Constructor}

export type Sort = 
    | "Type"
    | "Pattern"
    | "Expression"

export type Mark = 
    | {SortInconsistent : [Sort, Sort]}
    | {TypeInconsistent : [any, any]} // TypeLocation