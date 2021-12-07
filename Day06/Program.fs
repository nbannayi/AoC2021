open AoC2021.Utilities

// Advent of Code 2021 day 6.

[<EntryPoint>]
let main _argv =
    let lanternfishSchool =
        let lanternfishInput =
            "InputFiles/Day06Input.txt"
            |> Seq.ofFileLines
            |> Seq.head            
        in lanternfishInput.Split ','
        |> Array.map (int)        
        
    let processLanternfishSchool lanternfishSchool n =
        let processLanternfish lanternfish =        
            match lanternfish with
            | 0 -> [|6; 8|]
            | _ -> [|lanternfish-1|]

        let processLanternfishSchool' lanternfishSchool = 
            lanternfishSchool
            |> Array.map (processLanternfish)
            |> Array.collect (fun s -> s)
        (lanternfishSchool, [1..n])
        ||> List.fold (fun lfs i -> processLanternfishSchool' lfs)

    let lanternfishSchool' = processLanternfishSchool lanternfishSchool 80

    printfn "Part 1: result is %d" lanternfishSchool'.Length

    let lanternfishCounts = Array.create 10 0L
    let noDays = 256

    lanternfishSchool
    |> Array.iter (fun f -> (lanternfishCounts.[f] <- lanternfishCounts.[f] + 1L))

    [0..(noDays-1)]
    |> List.iter (fun d ->        
        let d' = (d + 7) % 9        
        lanternfishCounts.[d'] <- lanternfishCounts.[d'] + lanternfishCounts.[d % 9])    

    printfn "Part 2: result is %d" (lanternfishCounts |> Array.sum)
    0