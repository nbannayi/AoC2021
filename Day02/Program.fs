open AoC2021.Utilities

// Advent of Code 2021 day 2.

[<EntryPoint>]
let main _argv =
    let commands =
        "InputFiles/Day02Input.txt"
        |> Seq.ofFileLines
        |> Seq.map (fun command ->
            let tokens = command.Split ' ' in tokens.[0], int tokens.[1])

    // State represents (x, depth).
    let finalBearing =
        ((0, 0), commands)
        ||> Seq.fold (fun bearing command ->
            let x, depth = bearing
            match command with
            | "forward", n -> (x+n, depth)
            | "down",    n -> (x,   depth+n)
            | "up",      n -> (x,   depth-n)
            | _            -> (x,   depth))
            
    printfn "Part 1: result is %d" (fst finalBearing * snd finalBearing)

    // State represents (x, depth, aim).
    let finalBearing' =
        let x', depth', _ =
            ((0,0,0), commands)
            ||> Seq.fold (fun bearing command ->
                let x, depth, aim = bearing
                match command with
                | "forward", n -> (x+n, depth+aim*n, aim)
                | "down",    n -> (x,   depth,       aim+n)
                | "up",      n -> (x,   depth,       aim-n)
                | _            -> (x,   depth,       aim))
        in (x', depth')
    
    printfn "Part 2: result is %d" (fst finalBearing' * snd finalBearing')
    0