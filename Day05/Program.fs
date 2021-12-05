open AoC2021.Utilities
open AoC2021.Domain

// Advent of Code 2021 day 5.

[<EntryPoint>]
let main _argv =

    // Parse raw input to build a collection of vent lines.
    let ventLines =
        let parseVentLine (line: string) =
            let tokens = line.Split "->"
            let leftTokens = tokens.[0].TrimEnd().Split ',' |> Array.map (int)
            let rightTokens = tokens.[1].TrimStart().Split ',' |> Array.map (int)
            {
                EndPoint1 = { X = leftTokens.[0];  Y = leftTokens.[1] }
                EndPoint2 = { X = rightTokens.[0]; Y = rightTokens.[1] }
            }
        "InputFiles/Day05Input.txt"
        |> Seq.ofFileLines
        |> Seq.map(fun line -> parseVentLine line)
     
    // Filter out diagonal ventlines (hence use of ' in bindings below.)
    let ventLines' = ventLines |> Seq.filter (fun vl -> [Horizontal; Vertical] |> List.contains vl.getLineType)
    let ventRectangle' = VentRectangle.create ventLines'
    
    let noVentCrossingPoints' =
        ventLines'
        |> VentRectangle.puncture ventRectangle'
        |> VentRectangle.getNoCrossingPoints

    printfn "Part 1: result is %d" noVentCrossingPoints'

    // Don't filter out diagonal ventlines (original unfiltered set.)
    let ventRectangle = VentRectangle.create ventLines
    let noVentCrossingPoints =
        ventLines
        |> VentRectangle.puncture ventRectangle
        |> VentRectangle.getNoCrossingPoints

    printfn "Part 2: result is %d" noVentCrossingPoints
    0