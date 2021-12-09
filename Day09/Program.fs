open AoC2021.Utilities
open AoC2021.Domain

// Advent of Code 2021 day 9.

[<EntryPoint>]
let main _argv =    

    let heightMapInput =        
        "InputFiles/Day09Input.txt"
        |> Seq.ofFileLines
        |> Array.ofSeq

    let heightMap =
        heightMapInput
        |> HeightMap.create

    let lowPoints =
        [for row in [0..heightMap.height-1] do
            for col in [0..heightMap.width-1] -> (row,col)]
        |> List.filter (fun p -> heightMap |> HeightMap.isLowPoint p)

    let sumOfRiskPoints =
        lowPoints
        |> List.sumBy (fun p -> 1 + heightMap.Grid.[fst p,snd p].Value)

    printfn "Part 1: result is %d" sumOfRiskPoints

    let rec getBasin (points: (int * int) list) heightMap =
                
        let points' =
            points
            |> List.map (fun p -> heightMap |> HeightMap.getSurroundingPoints p)
            |> List.collect (fun p -> p)
            |> List.filter (fun p -> p.Value < 9)
            |> List.map (fun p-> p.Position)
            |> List.distinct
        
        points |> List.iter (fun p -> heightMap |> HeightMap.setPoint p 10)

        match points' with
        | [] -> points
        | _ -> points @ getBasin points' heightMap

    let productOfTop3Basins =
        lowPoints
        |> List.map (fun p -> getBasin [p] heightMap)
        |> List.map (List.length)
        |> List.sortDescending
        |> List.take 3
        |> List.reduce (*)
    
    printfn "Part 2: result is %d" productOfTop3Basins
    0