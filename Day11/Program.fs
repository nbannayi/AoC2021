open AoC2021.Utilities
open AoC2021.Domain

// Advent of Code 2021 day 11.

[<EntryPoint>]
let main _argv =    

    let octupusGridInputLines =        
        "InputFiles/Day11Input.txt"
        |> Seq.ofFileLines
        |> Array.ofSeq

    let octopusGrid' =
        DumboOctopusGrid.create octupusGridInputLines
        |> DumboOctopusGrid.cycleNTimes 100

    printfn "Part 1: result is %d" (octopusGrid' |> DumboOctopusGrid.getNoFlashes)

    let noCyclesTillFlashSimultaneously =
        DumboOctopusGrid.create octupusGridInputLines
        |> DumboOctopusGrid.cycleTillAllHaveFlashedSimulataneously
        |> fst

    printfn "Part 2: result is %d" noCyclesTillFlashSimultaneously
    0
