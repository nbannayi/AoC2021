namespace AoC2021.Domain

// BingoNumber module for AoC 2021 day 4.

/// BingoNumber to represent a number and whether crossed off or not.
type BingoNumber =
    {
        Number     : int
        CrossedOff : bool
    }

    /// Create a bingo number which is not crossed off by default.
    static member create number =
        {
            Number = number
            CrossedOff = false
        }

/// BingoBoard to contain a grid of bingo numbers in a given dimension.
type BingoBoard =
    {
        Grid       : BingoNumber [,]
        Dimensions : int * int
    }

    /// Create a bingo board with a grid of un-crossed off bingo numbers.
    static member create (dimensions: int * int) (numbers: int [,]) =
        let height, width = dimensions
        let grid = Array2D.init height width (fun _ _ -> BingoNumber.create 0)
        
        for row in [0..height-1] do
           for col in [0..width-1] do
               grid.[row,col] <- { Number = numbers.[row,col]; CrossedOff = false }

        { Grid = grid; Dimensions = dimensions }    

module BingoBoard =

    /// Display a bingo board.
    let display (board: BingoBoard) : unit =
        let width, height = board.Dimensions
        printfn ""
        for row in [0..height-1] do
           for col in [0..width-1] do
                printf "%02d[%s] "
                    board.Grid.[row,col].Number
                    (if board.Grid.[row,col].CrossedOff then "X" else " ")
           printfn ""

    /// Return true if at least one horizontal or vertical line is checked off.
    /// Returns false otherwise.
    let isBoardCompleted (board: BingoBoard) =

        let isRowCompleted row (board: BingoBoard) =
            let _,width = board.Dimensions
            [for col in [0..width-1] -> board.Grid.[row,col].CrossedOff]
            |> List.reduce (&&)                

        let isColumnCompleted col (board: BingoBoard) =
            let height,_ = board.Dimensions
            [for row in [0..height-1] -> board.Grid.[row,col].CrossedOff]
            |> List.reduce (&&)

        let width, height = board.Dimensions
        [for row in [0..height-1] -> board |> isRowCompleted row] @
        [for col in [0..width-1]  -> board |> isColumnCompleted col]
        |> List.contains true

    /// Takes a board and returns the same board with the given number crossed off.
    let crossOffNumber (number: int) (board: BingoBoard) =        
        let width, height = board.Dimensions
        seq [for row in [0..height-1] do
                for col in [0..width-1] -> (row,col,board.Grid.[row,col])]
        |> Seq.skipWhile (fun (_,_,bn) -> bn.Number <> number)
        |> Seq.tryHead
        |> Option.iter (fun (row,col,_) ->
            board.Grid.[row,col] <- { board.Grid.[row,col] with CrossedOff = true })
        board

    /// Get score of current board regardless of whether complete or not.
    let getScore number (board: BingoBoard) =
        let width, height = board.Dimensions
        [for row in [0..height-1] do
            for col in [0..width-1] ->
                if not board.Grid.[row,col].CrossedOff then board.Grid.[row,col].Number else 0]
        |> List.sum
        |> (*) number