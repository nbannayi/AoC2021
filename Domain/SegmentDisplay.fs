namespace AoC2021.Domain

// Segment display module for AoC 2021 day 8.

/// Represents a seven segment display.
type SegmentDisplay =
    {
        Segments: Segment array        
    }

    static member create (identifier: string) =
        {
            Segments =
                identifier
                |> Seq.map (fun id -> Segment.create id)
                |> Array.ofSeq
        }
                
module SegmentDisplay =

    /// Activate all required segments on display given as a string e.g. "abc".
    let activateSegments identifier (display: SegmentDisplay) =
        let activateSegment segmentChar (display: SegmentDisplay) =
            let index = display.Segments |> Array.findIndex (fun d -> d.asChar = segmentChar)        
            display.Segments.[index] <- display.Segments.[index] |> Segment.activatate

        identifier |> Seq.iter (fun s -> display |> activateSegment s)
        display

    /// Deactivate all segments on display.
    let reset (display: SegmentDisplay) =
        {
            display with
                Segments = display.Segments |> Array.map (Segment.deactivate)
        }

    /// Get decimal digit represented by segment display.
    let getDigit display =
        display.Segments
        |> Array.map (fun s -> match s.State with | On -> "1" | Off -> "0")
        |> Array.reduce (+)
        |> function
           | "1110111" -> 0
           | "0010010" -> 1
           | "1011101" -> 2
           | "1011011" -> 3
           | "0111010" -> 4
           | "1101011" -> 5
           | "1101111" -> 6
           | "1010010" -> 7
           | "1111111" -> 8
           | "1111011" -> 9
           | _ -> failwith "Invalid display, digit unknown."           