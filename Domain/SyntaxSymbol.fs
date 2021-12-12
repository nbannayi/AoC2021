namespace AoC2021.Domain

// Syntax symbol module for AoC 2021 day 10.

/// Represents the diffrent kinds of symbol used in syntax checking.
type SyntaxSymbolType =
    | OpenRoundBracket    // '('
    | ClosedRoundBracket  // ')'
    | OpenSquareBracket   // '['
    | ClosedSquareBracket // ']'
    | OpenBrace           // '{'
    | ClosedBrace         // '}'
    | OpenAngleBracket    // '<'
    | ClosedAngleBracket  // '>'

    /// Probide the inverse of a type.
    member this.inverse =
        match this with
        | OpenRoundBracket    -> ClosedRoundBracket
        | ClosedRoundBracket  -> OpenRoundBracket
        | OpenSquareBracket   -> ClosedSquareBracket
        | ClosedSquareBracket -> OpenSquareBracket
        | OpenBrace           -> ClosedBrace
        | ClosedBrace         -> OpenBrace
        | OpenAngleBracket    -> ClosedAngleBracket
        | ClosedAngleBracket  -> OpenAngleBracket

/// Represents the two kinds of bracket.
type SyntaxSymbolBracketType =
    | Open
    | Closed

    member this.inverse =
        match this with
        | Open   -> Closed
        | Closed -> Open

/// Represents a Syntax Symbol
type SyntaxSymbol =
    {
        Symbol      : SyntaxSymbolType
        BracketType : SyntaxSymbolBracketType
    }

    /// Create a syntax symbol.
    static member create symbol =

        match symbol with
        | '(' -> { Symbol = OpenRoundBracket;    BracketType = Open }
        | ')' -> { Symbol = ClosedRoundBracket;  BracketType = Closed }
        | '[' -> { Symbol = OpenSquareBracket;   BracketType = Open }
        | ']' -> { Symbol = ClosedSquareBracket; BracketType = Closed }
        | '{' -> { Symbol = OpenBrace;           BracketType = Open }
        | '}' -> { Symbol = ClosedBrace;         BracketType = Closed }
        | '<' -> { Symbol = OpenAngleBracket;    BracketType = Open }
        | '>' -> { Symbol = ClosedAngleBracket;  BracketType = Closed }
        | _ -> failwith "Invalid symbol provided must be one of '(',')','[',']','{','}','<','>'"

module SyntaxSymbol =

    /// Get inverse of a symbol.
    let getInverse syntaxSymbol =
        {
            syntaxSymbol with
                Symbol = syntaxSymbol.Symbol.inverse
                BracketType = syntaxSymbol.BracketType.inverse                
        }

    /// Get score for corruption symbol (part 1.)
    let getInvalidScore syntaxSymbol =
        match syntaxSymbol.Symbol with        
        | ClosedRoundBracket  -> 3 
        | ClosedSquareBracket -> 57 
        | ClosedBrace         -> 1197 
        | ClosedAngleBracket  -> 25137
        | _ -> 0    

    /// Get score for completion symbol (part 2.)
    let getCompletingScore syntaxSymbol =
        match syntaxSymbol.Symbol with        
        | ClosedRoundBracket  -> 1L 
        | ClosedSquareBracket -> 2L 
        | ClosedBrace         -> 3L 
        | ClosedAngleBracket  -> 4L
        | _ -> 0L   

    /// Given two consecutive symbols, return a tuple of validity
    /// (true or false) and list result of combining them.
    let validateConsecutiveSymbols topSymbol nextSymbolBelow =
        let topSymbolBracketType, nextSymbolBelowBracketType =
            topSymbol.BracketType, nextSymbolBelow.BracketType

        let isValid = 
            match topSymbolBracketType, nextSymbolBelowBracketType with
            | Closed, Open -> topSymbol.Symbol = nextSymbolBelow.Symbol.inverse
            | _ -> true

        match isValid, topSymbolBracketType, nextSymbolBelowBracketType with
        | true, Closed, Open ->  true, []
        | _ -> isValid, [topSymbol; nextSymbolBelow]         