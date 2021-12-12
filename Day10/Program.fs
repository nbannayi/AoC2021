open AoC2021.Utilities
open AoC2021.Domain

// Advent of Code 2021 day 10.

[<EntryPoint>]
let main _argv =    

    let syntaxLists =        
        "InputFiles/Day10Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (SyntaxSymbolList.create)
        |> List.ofSeq

    let sumOfInvalidSymbolScores =
        syntaxLists
        |> List.map (fun sl -> SyntaxSymbolList.findInvalidSymbol sl)
        |> List.choose (fun sl -> sl)
        |> List.map (SyntaxSymbol.getInvalidScore)
        |> List.sum

    printfn "Part 1: result is %d" sumOfInvalidSymbolScores

    let middleCompletionScore = 
        let sortedCompletiomScores =
            syntaxLists
            |> List.filter (SyntaxSymbolList.isCorrupted >> not)
            |> List.map (fun sl -> SyntaxSymbolList.findCompletionSymbols sl)
            |> List.map (fun cs -> SyntaxSymbolList.getCompletionScore cs)
            |> Array.ofList
            |> Array.sort
        in sortedCompletiomScores.[(sortedCompletiomScores.Length-1)/2]
        
    printfn "Part 2: result is %d" middleCompletionScore
    0 