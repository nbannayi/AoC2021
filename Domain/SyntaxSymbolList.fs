namespace AoC2021.Domain

// Syntax symbol list module for AoC 2021 day 10.

open AoC2021.Utilities

/// Represents a symbol list.
type SyntaxSymbolList =
    {
        SymbolList : SyntaxSymbol list        
    }

    static member create symbolString =
        {
            SymbolList =
                symbolString
                |> Seq.map (SyntaxSymbol.create)
                |> List.ofSeq
        }

module SyntaxSymbolList =

    /// Stack evaluator to process each element from the list.
    let private processSyntaxSymbol symbol stack =
        let stack = stack |> Stack.push symbol
        if stack |> Stack.size > 1 then
            let topSymbol = stack |> Stack.peek
            let nextSymbolBelow = stack |> Stack.peekPrevious

            let isValid, symbols =
                match topSymbol, nextSymbolBelow with
                | Some s1, Some s2 -> (s1, s2) ||> SyntaxSymbol.validateConsecutiveSymbols
                | _ -> false, []

            match isValid, symbols with
            | true, [] -> true, stack |> Stack.pop |> snd |> Stack.pop |> snd
            | true, _ -> true, stack
            | _ -> false, stack
        else
            true, stack

    /// Find the invalid symbol in a list, returns Some or None if it doesn't exist.
    let findInvalidSymbol (syntaxSymbolList: SyntaxSymbolList) =
        let symbolStack = Stack.empty
        ((true,symbolStack), syntaxSymbolList.SymbolList)
        ||> List.scan (fun (_,stack) ss -> processSyntaxSymbol (Some ss) stack)
        |> List.skipWhile (fun (status,_) -> status)
        |> List.tryHead
        |> Option.bind (fun r -> r |> snd |> Stack.pop |> fst)

    /// Return true if list is corrupted, false otherwise.
    let isCorrupted (syntaxSymbolList: SyntaxSymbolList) =
        syntaxSymbolList
        |> findInvalidSymbol
        |> function
           | Some _ -> true
           | None -> false

    /// Find list of symbols that will complete non-corrupted lines.
    let findCompletionSymbols (syntaxSymbolList: SyntaxSymbolList) =
        let symbolStack = Stack.empty
        ((true,symbolStack), syntaxSymbolList.SymbolList)
        ||> List.fold (fun (_,stack) ss -> processSyntaxSymbol (Some ss) stack)
        |> snd
        |> Stack.toSeq
        |> Seq.choose (fun s -> s)
        |> Seq.map (fun s -> s |> SyntaxSymbol.getInverse)
        |> List.ofSeq

    /// Get completion score required for part 2.
    let getCompletionScore (completionSyntaxSymbols: SyntaxSymbol list) =
        (0L, completionSyntaxSymbols)
        ||> List.fold (fun sc cs -> 5L*sc + SyntaxSymbol.getCompletingScore cs)