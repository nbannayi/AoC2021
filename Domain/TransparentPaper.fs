namespace AoC2021.Domain

// Transparent paper for AoC 2021 day 13.

/// Represents a dot on a tracing paper.
type Dot =
    {
        FilledIn : bool
        Position : int * int // row,col
    }

    static member create position = { FilledIn = true;  Position = position }
    static member empty  position = { FilledIn = false; Position = position }

/// Represenst type of fold, horizontal or vertical about a line.
type FoldLine =
    | Horizontal of int
    | Vertical of int
    
module Dot =

    /// Display dot (filled in #, not filled in .)
    let display dot =
        match dot.FilledIn with
        | true  -> printf "#"
        | false -> printf "."

    /// Merge two dots that coallesce.
    let merge thisDot otherDot =
        match thisDot.FilledIn, otherDot.FilledIn with
        | false, false -> { thisDot with FilledIn = false }        
        | _ -> { thisDot with FilledIn = true }

/// Represents a sheet of tarcing paper.
type TransparentPaper =
    {
        Grid   : Dot [,]
        Height : int
        Width  : int
    }

    static member create dotCoordinatesList =

        let width, height =
            (dotCoordinatesList |> List.map (fst) |> List.max) + 1,
            (dotCoordinatesList |> List.map (snd) |> List.max) + 1 

        let grid = Array2D.create height width (Dot.empty (0,0))

        dotCoordinatesList
        |> List.iter (fun d ->
            let col,row = d in grid.[row,col] <- Dot.create (row,col))
        
        {
            Grid = grid
            Height = height
            Width = width
        }

module TransparentPaper =

    /// Get total number of dots at a given time.
    let getNoDots transparentPaper =
        [for row in [0..transparentPaper.Height-1] do
            for col in [0..transparentPaper.Width-1] ->
                if transparentPaper.Grid.[row,col].FilledIn then 1 else 0]
        |> List.sum

    /// Display whole sheet.
    let display transparentPaper =
        for row in [0..transparentPaper.Height-1] do
            for col in [0..transparentPaper.Width-1] do
                transparentPaper.Grid.[row,col] |> Dot.display
            printfn ""
        printfn ""

    /// Merge a dot onto a piece of paper (created by a fold.)
    let merge otherDot transparentPaper =
        let row,col = otherDot.Position
        let thisDot = transparentPaper.Grid.[row,col]
        transparentPaper.Grid.[row,col] <- Dot.merge thisDot otherDot
        transparentPaper

    /// Perform a fold on given fold line.
    let fold (foldLine: FoldLine) (transparentPaper: TransparentPaper) =

        let height = transparentPaper.Height
        let width = transparentPaper.Width        

        let superimposeGridDots, grid' =
            match foldLine with
            | Horizontal l ->
                [for row in [(height-1)..(-1)..l] do
                    for col in [0..width-1] ->
                        let dot = transparentPaper.Grid.[row,col]                    
                        { dot with Position = 2*l-row,col }],
                Array2D.create (l+1) (width+1) (Dot.empty (0,0))                
            | Vertical l ->
                [for row in [0..height-1] do
                    for col in [(width-1)..(-1)..l-1] do
                        let dot = transparentPaper.Grid.[row,col]
                        { dot with Position = row,2*l-col }],
                Array2D.create (height+1) (l+1) (Dot.empty (0,0))

        let transparentPaper' =
            (transparentPaper, superimposeGridDots)
            ||> List.fold (fun tp d -> tp |> merge d)

        let height', width' =
            match foldLine with
                | Horizontal l ->
                    for row in [0..l] do
                        for col in [0..width-1] do
                            grid'.[row,col] <- transparentPaper'.Grid.[row,col]
                    l,width            
                | Vertical l -> 
                    for row in [0..height-1] do
                        for col in [0..l] do
                            grid'.[row,col] <- transparentPaper'.Grid.[row,col]
                    height,l

        { Grid = grid'; Height = height'; Width = width' }