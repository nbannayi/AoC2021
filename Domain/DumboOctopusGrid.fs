namespace AoC2021.Domain

// Dumbo Octopus Grid module for AoC 2021 day 11.

open AoC2021.Utilities

/// Repreents a rectangular grid of Dumbo octopuses.
type DumboOctopusGrid =
    {
        Grid : DumboOctopus [,]
    }

    /// Height of grid.
    member this.height =
        this.Grid |> Array2D.length1

    /// Width of grid.
    member this.width =
        this.Grid |> Array2D.length2

    /// Create a grid of Dumbo octopuses in initial state from passed in strings
    /// containing energy levels.
    static member create (dumboOctopusGridLines: string array) =
        let height = dumboOctopusGridLines.Length
        let width = dumboOctopusGridLines.[0].Length

        let grid = Array2D.init height width (fun _ _ -> DumboOctopus.init)

        let getDumboOctopusPosition row col =
            match row, col with
            | 0, 0                                           -> TopLeftCorner
            | 0, c when c = width-1                          -> TopRightCorner            
            | 0, c when c > 0 && c < width-1                 -> TopMiddleEdge
            | r, 0 when r > 0 && r < height-1                -> MiddleLeftEdge
            | r, c when r > 0 && r < height-1 && c = width-1 -> MiddleRightEdge 
            | r, 0 when r = height-1                         -> BottomLeftCorner
            | r, c when r = height-1 && c > 0 && c < width-1 -> BottomMiddleEdge
            | r, c when r = height-1 && c = width-1          -> BottomRightCorner 
            | _                                              -> Middle

        for row in [0..height-1] do
             for col in [0..width-1] do
                 grid.[row,col] <-
                     {
                         EnergyLevel  = dumboOctopusGridLines.[row].[col] |> Char.toInt |> Option.defaultValue 0
                         FlashCount   = 0
                         Position     = row,col 
                         PositionType = getDumboOctopusPosition row col
                         HasFlashed   = false
                     }

        { Grid = grid }

module DumboOctopusGrid =

    /// In a given grid return number of flashes that have occured.
    let getNoFlashes (octopusGrid: DumboOctopusGrid) =
        [for row in [0..octopusGrid.height-1] do
            for col in [0..octopusGrid.width-1] -> octopusGrid.Grid.[row,col].FlashCount]
        |> List.sum

    /// Return a list of all octopuses in grid.
    let getAllOctopuses (octopusGrid: DumboOctopusGrid) =
        [for row in [0..octopusGrid.height-1] do
            for col in [0..octopusGrid.width-1] -> octopusGrid.Grid.[row,col]]        

    /// Display all octopuses in grid with highlighted flashes.
    let display (octopusGrid: DumboOctopusGrid) =        
        octopusGrid
        |> getAllOctopuses
        |> List.iter (DumboOctopus.display)
        printfn ""

    /// Get octopuses surrounding a given octopus in the grid.
    let getSurroundingOctopuses (octopus: DumboOctopus) (octopusGrid: DumboOctopusGrid) =
        let row,col = octopus.Position

        let surroundingPositions = 
            match octopus.PositionType with
            | TopLeftCorner     -> [row+1,0;   0,col+1;   1,1];
            | TopRightCorner    -> [0,col-1;   row+1,col; 1,col-1]     
            | TopMiddleEdge     -> [0,col-1;   0,col+1;   row+1,col; row+1,col-1; row+1,col+1]
            | MiddleLeftEdge    -> [row-1,0;   row+1,0;   row,col+1; row-1,col+1; row+1,col+1]
            | MiddleRightEdge   -> [row-1,col; row,col-1; row+1,col; row-1,col-1; row+1,col-1]
            | BottomLeftCorner  -> [row-1,0;   row,col+1; row-1,col+1]
            | BottomMiddleEdge  -> [row,col-1; row,col+1; row-1,col; row-1,col-1; row-1,col+1]
            | BottomRightCorner -> [row,col-1; row-1,col; row-1,col-1]
            | Middle            -> [row-1,col; row+1,col; row,col-1; row,col+1; row-1,col-1; row+1,col-1; row-1,col+1; row+1,col+1]
            | Unknown           -> failwith "Unknown position type."

        surroundingPositions
        |> List.map (fun (row,col) -> octopusGrid.Grid.[row,col])

    /// Reset all octopuses in the grid ready for next cycle.
    let reset (octopusGrid: DumboOctopusGrid) =
        octopusGrid
        |> getAllOctopuses
        |> List.iter (fun o ->
            let row,col = o.Position
            octopusGrid.Grid.[row,col] <- o |> DumboOctopus.reset)
        octopusGrid

    /// Cycle entire grid once.
    let cycle (octopusGrid: DumboOctopusGrid) =

        // First cycle the grid.
        octopusGrid
        |> getAllOctopuses
        |> List.iter (fun o ->
            let row,col = o.Position
            octopusGrid.Grid.[row,col] <- o |> DumboOctopus.cycle)

        // Get all octopuses that are greater than 9 and not flashed.        
        let getOctopusesToCycle (octopusGrid: DumboOctopusGrid) =        
            octopusGrid
            |> getAllOctopuses 
            |> List.filter (fun o -> o.EnergyLevel > 9 && not o.HasFlashed)

        // For each octopus find all surrounding octopuses and cycle them.        
        let rec cycle' (octopusGrid: DumboOctopusGrid) (octopuses: DumboOctopus list) =
           
            octopuses
            |> List.iter (fun o ->
                octopusGrid            
                |> getSurroundingOctopuses o
                |> List.iter (fun o' ->
                    let row,col = o'.Position
                    octopusGrid.Grid.[row,col] <- o' |> DumboOctopus.cycle))

            // Finally, flash the octopuses.
            octopuses
            |> List.iter (fun o ->
                let row,col = o.Position
                octopusGrid.Grid.[row,col] <- o |> DumboOctopus.flash)

            // Recurse through any newly activated octopuses.
            let octopuses' = getOctopusesToCycle octopusGrid
            match octopuses' with
            | [] -> octopusGrid
            | _ -> cycle' octopusGrid octopuses'

        getOctopusesToCycle octopusGrid
        |> cycle' octopusGrid
        |> reset

    /// Cycle grid specified number of times.
    let cycleNTimes n (octopusGrid: DumboOctopusGrid) =
        (octopusGrid, [1..n])
        ||> List.fold (fun og _ -> cycle og)

    /// Return true if all have flashed simultaneously in current state.
    let haveAllFlashedSimulateously (octopusGrid: DumboOctopusGrid) =
        octopusGrid
        |> getAllOctopuses
        |> List.map (fun o -> o.EnergyLevel)
        |> List.reduce (+)
        |> (=) 0

    /// Cycle till all octopuses have flashed simulatenously, and return a tuple of
    /// number of cycles and the final octopus grid.
    let cycleTillAllHaveFlashedSimulataneously (octopusGrid: DumboOctopusGrid) =
        let rec cycle' n octopusGrid =
            let octopusGrid' = cycle octopusGrid
            if octopusGrid' |> haveAllFlashedSimulateously then
                n+1, octopusGrid
            else
                cycle' (n+1) octopusGrid
        cycle' 0 octopusGrid