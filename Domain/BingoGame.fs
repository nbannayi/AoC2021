namespace AoC2021.Domain

// BingoGame module for AoC 2021 day 4.

open System

/// BingoGame to represent a game and register the timestamp and score on completion.
type BingoGame =
    {
        Board              : BingoBoard
        Completed          : bool
        Score              : int
        CompletedTimeStamp : int64
    }

    static member create board =
        {
            Board = board
            Completed = false
            Score = 0
            CompletedTimeStamp = 0L
        }

module BingoGame =

    /// Given a bingo game and a number return a bingo game with all state updated.
    let update n (bingoGame: BingoGame) =
        match bingoGame.Completed with
        | false ->
            let board' = bingoGame.Board |> BingoBoard.crossOffNumber n
            let completed = board' |> BingoBoard.isBoardCompleted
            let score = if completed then board' |> BingoBoard.getScore n else 0
            let completedTimeStamp = if completed then DateTime.Now.Ticks else 0L
            {
                bingoGame with
                    Board = board'
                    Completed = completed
                    Score = score
                    CompletedTimeStamp = completedTimeStamp
            }
        | true ->
            bingoGame