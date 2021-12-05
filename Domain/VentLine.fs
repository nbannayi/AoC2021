namespace AoC2021.Domain

// VentLine module for AoC 2021 day 5.

open System

/// Type to represent a point along a vent line.
type VentPoint = { X: int; Y: int }

/// Types of Vent line available.
type VentLineType = Horizontal | Vertical | Diagonal | DiagonalNot45Degrees

/// Type to represent a ventline composed of vent ponts.
type VentLine =
    {
        EndPoint1: VentPoint;
        EndPoint2: VentPoint
    }

    /// Get the type of vent line represented.
    member this.getLineType =

        match this.EndPoint1, this.EndPoint2 with
        | p1, p2 when p1.X = p2.X -> Vertical
        | p1, p2 when p1.Y = p2.Y -> Horizontal
        | p1, p2 when Math.Abs(p2.Y-p1.Y) = Math.Abs(p2.X-p1.X) -> Diagonal
        | _ -> DiagonalNot45Degrees // Don't have to handle these (thank god!)

module VentLine =

    /// Work out area covered by a vent line.
    let areaCovered ventLine =

        Math.Max(ventLine.EndPoint1.X,ventLine.EndPoint2.X),
        Math.Max(ventLine.EndPoint1.Y,ventLine.EndPoint2.Y)

    /// Get all vent points on a vent line.
    let getPointsOnLine (ventLine: VentLine) =

        match ventLine.getLineType with
        | Horizontal ->
            let horizRange = Math.Abs(ventLine.EndPoint2.X - ventLine.EndPoint1.X)
            let xStart = Math.Min(ventLine.EndPoint1.X,ventLine.EndPoint2.X)
            [|xStart..(xStart+horizRange)|] |> Array.map (fun x -> { X = x; Y = ventLine.EndPoint1.Y })
        | Vertical ->            
            let vertRange = Math.Abs(ventLine.EndPoint2.Y - ventLine.EndPoint1.Y)
            let yStart = Math.Min(ventLine.EndPoint1.Y,ventLine.EndPoint2.Y)
            [|yStart..(yStart+vertRange)|] |> Array.map (fun y -> { X = ventLine.EndPoint1.X; Y = y })
        | Diagonal ->
            let ix = Math.Sign(ventLine.EndPoint2.X - ventLine.EndPoint1.X)
            let iy = Math.Sign(ventLine.EndPoint2.Y - ventLine.EndPoint1.Y)            
            let xStart, yStart, xEnd, yEnd =
                ventLine.EndPoint1.X, ventLine.EndPoint1.Y, ventLine.EndPoint2.X, ventLine.EndPoint2.Y         
            ([|xStart..ix..xEnd|], [|yStart..iy..yEnd|])
            ||> Array.zip
            |> Array.map (fun p -> { X = fst p; Y = snd p})
        | _ -> failwith "Only horizontal, vertical or 45 degree diagonal vent lines supported."