open AoC2021.Utilities

// Advent of Code 2021 day 7.

open System

[<EntryPoint>]
let main _argv =
    let crabs = 
        let crabsInput =        
            "InputFiles/Day07Input.txt"
            |> Seq.ofFileLines
            |> Seq.head
        in crabsInput.Split "," |> Array.map (int)      

    let crabFuelCalculator1 (c1: int) (c2: int) =
        Math.Abs(c1-c2)

    let crabFuelCalculator2 (c1: int) (c2: int) =
        [1..Math.Abs(c1-c2)]
        |> List.sum

    let getCrabFuelRequirements crabs crabFuelCalculator =
        [|for c1 in crabs |> Array.distinct do
            for c2 in crabs -> (c1,c2)|]
        |> Array.map (fun cp -> fst cp, (crabFuelCalculator (fst cp) (snd cp)))
        |> Array.groupBy (fst)
        |> Array.map (fun cp -> (fst cp, (snd cp) |> Array.sumBy (snd)))
        |> Array.sortBy (snd)

    let crabFuelRequirements1 = getCrabFuelRequirements crabs crabFuelCalculator1
    printfn "Part 1: result is %d" (crabFuelRequirements1 |> Array.head |> snd)

    let crabFuelRequirements2 = getCrabFuelRequirements crabs crabFuelCalculator2
    printfn "Part 2: result is %d" (crabFuelRequirements2 |> Array.head |> snd)    
    0