namespace AoC2021.Utilities

open System

module String =

    /// Converts a string to a sequence of char.
    let toCharSeq (str : string) : char seq =
        str
        |> Seq.map (fun c -> c)

    /// Converts a string to a sequence of 1 character strings.
    let toStringSeq (str : string) : string seq =
        str
        |> Array.ofSeq
        |> Array.map (string)
        |> Seq.ofArray
        
    /// Converts a string of digits to a sequence of int.
    let toIntSeq (str : string) : int seq =
        str
        |> Seq.map (string)
        |> Seq.map (fun c -> int c)

    /// Returns true if every character in string is upper case, false otherwise.
    let isFullyUpperCase (str: string) : bool =
        str |>
        Seq.forall (Char.IsUpper)

    /// Returns true if every character in string is lower case, false otherwise.
    let isFullyLowerCase (str: string) : bool =
        str |>
        Seq.forall (Char.IsLower)  