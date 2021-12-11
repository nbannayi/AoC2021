namespace AoC2021.Domain

// Dumbo Octopus module for AoC 2021 day 11.

open System

/// Type to represent octopus position. 
type DumboOctopusPositionType =
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

/// Type to represent octopus.
type DumboOctopus =
    {
        EnergyLevel  : int
        FlashCount   : int        
        Position     : int * int
        PositionType : DumboOctopusPositionType
        HasFlashed   : bool
    }

    static member init =
        { EnergyLevel = 0; FlashCount = 0; Position = (0,0); PositionType = Unknown; HasFlashed = false }

module DumboOctopus =

    /// Rests eneryy level and flash status.
    let reset dumboOctopus =
        let energyLevel = dumboOctopus.EnergyLevel
        {
            dumboOctopus with
                HasFlashed = false
                EnergyLevel = if energyLevel > 9 then 0 else energyLevel
        }

    /// Cycle an octopus.
    let cycle dumboOctopus =
        { dumboOctopus with EnergyLevel = dumboOctopus.EnergyLevel+1 }

    /// Flash an octopus.
    let flash dumboOctopus =
        let flashCount =
            if not dumboOctopus.HasFlashed then
                dumboOctopus.FlashCount+1
            else
                dumboOctopus.FlashCount        
        {
            dumboOctopus with
                HasFlashed = true
                FlashCount = flashCount
        }

    /// Display an octopus, highlighting if flashed.
    let display (dumboOctopus: DumboOctopus) =
        let currentConsoleForegroundColour = Console.ForegroundColor
        let energyLevel = dumboOctopus.EnergyLevel

        Console.ForegroundColor <-
            match dumboOctopus.EnergyLevel with
            | 0 -> ConsoleColor.White
            | _ -> ConsoleColor.DarkGray
        
        match dumboOctopus.PositionType with
        | TopRightCorner | MiddleRightEdge | BottomRightCorner -> printfn "%d" energyLevel
        | _ -> printf "%d" energyLevel

        Console.ForegroundColor <- currentConsoleForegroundColour