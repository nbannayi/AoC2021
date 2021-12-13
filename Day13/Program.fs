open AoC2021.Utilities
open AoC2021.Domain

// Advent of Code 2021 day 13.

[<EntryPoint>]
let main _argv =

    let transparentPaperSections =        
        "InputFiles/Day13Input.txt"
        |> Seq.ofFileChunks "\n\n"
        |> Array.ofSeq

    let transparentPaperDots =
        transparentPaperSections.[0].Split "\n"
        |> Array.map (fun p ->
            let tokens = p.Split ','
            in int tokens.[0], int tokens.[1])
        |> List.ofArray

    let foldLines =
        transparentPaperSections.[1].Split "\n"
        |> Array.map (fun l ->            
            let lineTokens = (l.Split ' ' |> Array.last).Split '='
            match lineTokens.[0], (int lineTokens.[1]) with
            | "y", y -> FoldLine.Horizontal(y)
            | "x", x -> FoldLine.Vertical(x)
            | _ -> failwith "Invalid fold axis found.")

    let transparentPaper = TransparentPaper.create transparentPaperDots

    // First fold.
    let noDots =
        transparentPaper
        |> TransparentPaper.fold (foldLines |> Array.head)
        |> TransparentPaper.getNoDots

    printfn "Part 1: result is %d\n" noDots

    // All folds.
    (transparentPaper, foldLines)
    ||> Array.fold (fun tp fl -> tp |> TransparentPaper.fold fl)
    |> TransparentPaper.display

    // ^ View image above to get answer...
    printfn "Part 2: result is EAHKRECP"    
    0