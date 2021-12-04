open AoC2021.Utilities

// Advent of Code 2021 day 1.

[<EntryPoint>]
let main _argv =    
    let depths =
        "InputFiles/Day01Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (int)

    let getDepthChanges (depths: seq<int>) =
        depths
        |> Seq.pairwise
        |> Seq.map (fun (d1, d2) ->
            let changeDescription =
                match d1, d2 with
                | d1, d2 when d2 > d1 -> "increased"
                | d1, d2 when d2 < d1 -> "decreased"
                | d1, d2 when d2 = d1 -> "no change"
                | _ -> "unknown" // Shouldn't get here.
            (d2, changeDescription))
        
    let getNoDepthIncreases (depthChanges: seq<int * string>) =
        depthChanges
        |> Seq.filter (fun d -> (snd d) = "increased")
        |> Seq.length

    printfn "Part 1: result is %d" (depths |> getDepthChanges |> getNoDepthIncreases)

    let depths' =
        depths
        |> Seq.windowed 3
        |> Seq.map (Array.reduce (+))        

    printfn "Part 2: result is %d" (depths' |> getDepthChanges |> getNoDepthIncreases)
    0 