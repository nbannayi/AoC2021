open AoC2021.Utilities

// Advent of Code 2021 day 14.

[<EntryPoint>]
let main _argv =

    let polymerInput =        
        "InputFiles/Day14Input.txt"
        |> Seq.ofFileChunks "\n\n"
        |> Array.ofSeq
    let polymerTemplate = polymerInput |> Array.head
    let polymerRules =
        (polymerInput |> Array.last).Split "\n"
        |> Array.map (fun pp ->
            let pp' = (pp.Split " -> ") in (pp'.[0],pp'.[1]))
        |> Map.ofArray
    
    let processPolymer (polymer: string) =
        let processPair (pair: string) =
            let insertPolymer = polymerRules.[pair]
            [string pair.[0]; insertPolymer; string pair.[1]]
            |> String.concat ""
        let polymer' =
            polymer
            |> Seq.windowed 2
            |> Seq.map (fun p -> string p.[0] + string p.[1])
            |> Seq.map (processPair)
            |> Seq.map (fun p -> p.[1..2])
            |> String.concat ""
        (string polymer.[0]) + polymer'

    let polymerEndState =
        (polymerTemplate, [1..10]) ||> List.fold (fun p _ -> processPolymer p)

    let polymerEndStateCounts = 
        polymerEndState
        |> Seq.countBy (fun s -> s)
        |> Seq.sortBy (snd)
        |> Seq.map (snd)

    let minOccurences = polymerEndStateCounts |> Seq.head
    let maxOccurences = polymerEndStateCounts |> Seq.last

    printfn "Part 1: result is %d" (maxOccurences-minOccurences)

    let initialPairCounts =
        polymerTemplate  
        |> Seq.windowed 2
        |> Seq.map (fun p -> sprintf "%c%c" p.[0] p.[1])        
        |> Array.ofSeq
        |> Array.countBy (fun p -> p)
        |> Array.map (fun (p,n) -> (p, n |> int64))
   
    let processPolymer' (pairs: (string * int64)[]) =
        let processPair (pair: string * int64) =
            let template, n = pair
            let insert = polymerRules.[template]
            let left = sprintf "%c%s" template.[0] insert
            let right = sprintf "%s%c" insert template.[1]
            [|(left, n);(right,n)|]

        pairs
        |> Array.map (processPair)
        |> Array.collect (fun p -> p)
        |> Array.groupBy (fst)
        |> Array.map (fun p -> fst p, snd p |> Array.map (snd) |> Array.sum)        

    let polymerEndState' =
        (initialPairCounts, [1..40]) ||> List.fold (fun p _ -> processPolymer' p)

    let polymerEndStateCounts' =
        let firstLetter = polymerTemplate |> Seq.head
        let lastLetter = polymerTemplate |> Seq.last

        polymerEndState'
        |> Array.map (fun p -> let pair, n = p in [|(pair.[0],n);(pair.[1],n)|])
        |> Array.collect (fun p -> p)
        |> Array.groupBy (fst)
        |> Array.map (fun p -> fst p, snd p |> Array.map (snd) |> Array.sum)
        |> Array.map (fun (p,n) ->
            match (p,n) with
            | p, n when p = firstLetter || p = lastLetter -> (p, n+1L)
            | _ -> (p,n))
        |> Array.map (fun (p,n) -> p, n/2L)
        |> Array.map (snd)
        |> Array.sort

    let minOccurences = polymerEndStateCounts' |> Seq.head
    let maxOccurences = polymerEndStateCounts' |> Seq.last

    printfn "Part 2: result is %d" (maxOccurences-minOccurences)
    0