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

export type Sort = 
    | "Type"
    | "Pattern"
    | "Expression"

export type Mark = 
    | {SortInconsistent : [Sort, Sort]}