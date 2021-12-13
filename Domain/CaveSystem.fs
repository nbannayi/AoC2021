namespace AoC2021.Domain

// Cave system module for AoC 2021 day 12.

open AoC2021.Utilities

/// Represents a cave type.
type CaveType =
    | BigCave
    | SmallCave
    | Entrance
    | Exit

/// Represents a cave.
type Cave =
    {
        Label : string
        Type  : CaveType 
    }

    static member create label =        
        {
            Label = label
            Type =
                match label, String.isFullyUpperCase label with
                | "start", _ -> Entrance
                | "end", _   -> Exit
                | _, true    -> BigCave
                | _, false   -> SmallCave
        }
    
/// Represents a cave system.
type CaveSystem =
    {
        Connections     : Map<Cave,Cave list>

        // Had to resort to ummtability here, needs reviewing at some point.
        mutable NoPaths : int
        mutable Paths   : Cave list list
    }

    /// Return a cave entrance.
    static member entrance = Cave.create "start"

    /// Return a cave exit.
    static member exit = Cave.create "end"

    /// Build a cave system from passed in imput lines.
    static member create (pathLines: string list) =
        {
            Connections = 
                pathLines
                |> List.map (fun pl ->
                    let tokens = pl.Split '-' in
                    Cave.create tokens.[0], Cave.create tokens.[1])
                |> List.groupBy (fst)
                |> List.map (fun pl -> fst pl, (snd pl) |> List.map (snd))
                |> List.map (fun pl -> fst pl, (snd pl) |> List.filter (fun c -> c.Type <> Entrance))
                |> Map.ofList
            NoPaths = 0
            Paths = List.empty
        }

module CaveSystem =

    /// Reset the cave counters for part 2.
    let reset (caveSystem: CaveSystem) =
        {
            caveSystem with
                Paths = List.empty
                NoPaths = 0
        }