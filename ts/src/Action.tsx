export type Constructor = 
    | "Zero"
    | "Plus"
    | "Pair"
    | "Fun"
    | "Ap"
    | "Let"

export type BlossomAction = 
    | "AllUpdateSteps"
    | "UpdateStep"

export type Direction = 
    | "Up"
    | "Down"
    | "Right"

export type Action = 
    | {BlossomAction: BlossomAction}
    | {WrapLeft: Constructor}
    | {Insert: Constructor}
    | "Delete"
    | {Move: Direction}
    | "Cut" 
    | "Paste"
    | {MoveToLocation: any} // TermLocation
    | {MoveToTerm: any} // Term
    | {TextInsert: String}
    | "TextBackspace"
