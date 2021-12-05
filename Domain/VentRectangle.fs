namespace AoC2021.Domain

// VentRectangle module for AoC 2021 day 5.

/// Type to represent a rectangle that will hold a specified set of vent lines.
type VentRectangle =
    {
        Grid: int[,]
    }

    /// Create vent rectangle that can hold given vent lines.
    static member create ventLines =
        let width, height =
            ventLines |> Seq.maxBy (fun vl -> (vl |> VentLine.areaCovered) |> fst) |> VentLine.areaCovered |> fst,
            ventLines |> Seq.maxBy (fun vl -> (vl |> VentLine.areaCovered) |> snd) |> VentLine.areaCovered |> snd
        Array2D.zeroCreate<int> (height+1) (width+1)

module VentRectangle =

    /// Fill the vent rectangle with vents based on the given vent lines.
    let puncture (ventRectangle: int[,]) ventLines =
        ventLines
        |> Seq.iter (fun vl ->
            vl
            |> VentLine.getPointsOnLine
            |> Array.iter (fun vp ->            
                let currentValue = ventRectangle.[vp.Y, vp.X]
                ventRectangle.[vp.Y,vp.X] <- currentValue + 1))
        ventRectangle
    
    /// Given a populated vent rectangle get no. of points where vent lines cross.
    let getNoCrossingPoints ventRectangle =
        [for x in [0..(ventRectangle |> Array2D.length2)-1] do
            for y in [0..(ventRectangle |> Array2D.length1)-1] ->
                if ventRectangle.[y,x] > 1 then 1 else 0]
        |> List.sum