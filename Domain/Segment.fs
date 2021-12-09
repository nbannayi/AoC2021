namespace AoC2021.Domain

// Segment module for AoC 2021 day 8.

type SegmentId = A | B | C | D | E | F | G | Blank
type SegmentState = On | Off

/// Represents a segment on a seven segment display.
type Segment =
    {
        Id: SegmentId
        State: SegmentState
    }

    static member create (id: char) =

        let id' =
            match id.ToString().ToUpper() with
            | "A" -> A | "B" -> B | "C" -> C | "D" -> D | "E" -> E | "F" -> F | "G" -> G
            | _ -> failwith "Invalid segment id supplied, needs to be a letter from a-g."

        {
            Id = id'
            State = Off
        }

    member this.asChar =
        match this.Id with
        | A -> 'a' | B -> 'b' | C -> 'c' | D -> 'd' | E -> 'e' | F -> 'f' | G -> 'g' | Blank -> ' '

module Segment =

    /// Activates a segment.
    let activatate segment =
        { segment with State = On }

    /// Deactivates a segment.
    let deactivate segment =
        { segment with State = Off }