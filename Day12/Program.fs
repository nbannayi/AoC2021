open AoC2021.Utilities
open AoC2021.Domain

// Advent of Code 2021 day 12.

[<EntryPoint>]
let main _argv =

    let caveLinesOriginal =        
        "InputFiles/Day12Input.txt"
        |> Seq.ofFileLines

    let caveLinesReversed =
        caveLinesOriginal
        |> Seq.map (fun cl -> let tokens = cl.Split '-' in sprintf "%s-%s" tokens.[1] tokens.[0])        

    let caveLines = 
        Seq.append caveLinesOriginal caveLinesReversed
        |> List.ofSeq
           
    let caveSystem = caveLines |> CaveSystem.create
  
    let rec getConnections caveSystem (caves: Cave list) (cave: Cave) (cave': Cave option) =

        let checkVisited (cave: Cave) caves =
            if cave.Type = SmallCave then not (caves |> List.contains cave) else true

        let checkVisited' (cave: Cave) (cave': Cave) caves =
            let noCaves' = caves |> List.filter (fun c -> c = cave') |> List.length
            if (cave = cave' && noCaves' < 2) then true else (checkVisited cave caves)            

        match cave.Type with
        | Exit ->
            caveSystem.NoPaths <- caveSystem.NoPaths + 1
            caveSystem.Paths <- caveSystem.Paths @ [caves]
            caves
        | _ ->            
            let connections =
                caveSystem.Connections.[cave]
                |> List.filter (fun c ->                                        
                    match cave' with
                    | None -> checkVisited cave caves
                    | Some cave' -> checkVisited' cave cave' caves)                        
            let caves' = caves @ [cave]
            let connections' =
                connections 
                |> List.map (fun c -> connections @ (getConnections caveSystem caves' c cave'))
            
            (connections' |> List.collect (fun c -> c))            
            
    getConnections caveSystem [] CaveSystem.entrance None |> ignore
    printfn "Part 1: result is %d" caveSystem.NoPaths

    // Part 2 is horribly slow takes about 2 mins, needs to be reviewed and optimised
    // at some point!
    let caveSystem' = caveSystem |> CaveSystem.reset

    let smallCaves =
        caveSystem'.Connections
        |> Map.toList
        |> List.map (fst)
        |> List.filter (fun c -> c.Type = SmallCave)
        |> List.distinct

    let noPaths = 
        smallCaves
        |> List.map (fun c ->
            let caveSystem' = caveSystem' |> CaveSystem.reset
            getConnections caveSystem' [] CaveSystem.entrance (Some c) |> ignore        
            caveSystem'.Paths)
        |> List.collect (fun c -> c)
        |> List.distinct
        |> List.length

    printfn "Part 2: result is %d" noPaths
    0