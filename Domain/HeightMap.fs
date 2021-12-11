namespace AoC2021.Domain

// Height map module for AoC 2021 day 9.

open AoC2021.Utilities

type HeightPointType =
    | TopLeftCorner
    | TopMiddleEdge
    | TopRightCorner
    | MiddleLeftEdge
    | Middle
    | MiddleRightEdge
    | BottomLeftCorner
    | BottomMiddleEdge
    | BottomRightCorner
    | Unknown
  
type HeightPoint =
    {
        Value    : int 
        Type     : HeightPointType
        Position : int * int // row, col.
    }

    static member init =
        {
            Value = 0
            Type = Unknown
            Position = (0,0)
        }

    member this.fill n =
        {
            this with Value = n
        }

type HeightMap =
    {
        Grid : HeightPoint [,]      
    }

    member this.height =
        this.Grid |> Array2D.length1

    member this.width =
        this.Grid |> Array2D.length2

    static member create (heightMapLines: string array) =
        let height = heightMapLines.Length
        let width = heightMapLines.[0].Length

        let grid = Array2D.init height width (fun _ _ -> HeightPoint.init)

        let getHeightPointType row col =
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
                        Value = heightMapLines.[row].[col] |> Char.toInt |> Option.defaultValue 0
                        Type = getHeightPointType row col
                        Position = (row, col)
                    }

        { Grid = grid }

module HeightMap =

    let getSurroundingPoints (row, col) (heightMap: HeightMap) =
        let heightPoint = heightMap.Grid.[row,col]
        match heightPoint.Type with
        | TopLeftCorner     -> [heightMap.Grid.[row+1,0]; heightMap.Grid.[0,col+1]]
        | TopRightCorner    -> [heightMap.Grid.[0,col-1]; heightMap.Grid.[row+1,col]] 
        | TopMiddleEdge     -> [heightMap.Grid.[0,col-1]; heightMap.Grid.[0,col+1]; heightMap.Grid.[row+1,col]]
        | MiddleLeftEdge    -> [heightMap.Grid.[row-1,0]; heightMap.Grid.[row,col+1]; heightMap.Grid.[row+1,col]]
        | MiddleRightEdge   -> [heightMap.Grid.[row-1,col]; heightMap.Grid.[row,col-1]; heightMap.Grid.[row+1,col]]
        | BottomLeftCorner  -> [heightMap.Grid.[row-1,0]; heightMap.Grid.[row,col+1]]
        | BottomMiddleEdge  -> [heightMap.Grid.[row,col-1]; heightMap.Grid.[row,col+1]; heightMap.Grid.[row-1,col]]
        | BottomRightCorner -> [heightMap.Grid.[row,col-1]; heightMap.Grid.[row-1,col]]
        | Middle            -> [heightMap.Grid.[row-1,col]; heightMap.Grid.[row+1,col]; heightMap.Grid.[row,col-1]; heightMap.Grid.[row,col+1]]
        | Unknown           -> failwith "Unknown point type." 

    let isLowPoint (row, col) (heightMap: HeightMap) =
        let value = heightMap.Grid.[row,col].Value
        heightMap
        |> getSurroundingPoints (row, col)
        |> List.forall (fun hp -> value < hp.Value)

    let setPoint (row, col) n (heightMap: HeightMap) =
        heightMap.Grid.[row,col] <- heightMap.Grid.[row,col].fill n

