open AoC2021.Utilities
open AoC2021.Domain

// Advent of Code 2021 day 4.

[<EntryPoint>]
let main _argv =

    // Do all parsing.
    let bingoGridHeight, bingoGridWidth = (5,5)
    let bingoInput = "InputFiles/Day04Input.txt" |> Seq.ofFileChunks "\n\n"

    let bingoNumbers = (bingoInput|> Seq.head).Split ',' |> Array.map (int)

    let bingoBoards =
        let buildBingoBoard (boardInput: string) =
            let boardRowsArray = boardInput.Split "\n"
            let boardNumbers = Array2D.zeroCreate<int> bingoGridHeight bingoGridWidth
            boardRowsArray
            |> Array.mapi (fun row numbers ->
                numbers.Trim().Replace("  ", " ").Split ' '
                |> Array.iteri (fun col num -> boardNumbers.[row,col] <- int num))
            |> ignore
            BingoBoard.create (bingoGridHeight, bingoGridWidth) boardNumbers
        bingoInput
        |> Seq.tail
        |> Seq.map (buildBingoBoard)

    // Create an array of bingo games.
    let bingoGames =
        bingoBoards
        |> Seq.map (BingoGame.create)
        |> Array.ofSeq

    // For each bingo number, checked each bingo game for the number and update accordingly in the array.
    // Finally, sort array by the completed time stamp so we can find the winner and loser of all games.
    let completedBingoGames =
        (bingoGames, bingoNumbers)
        ||> Array.fold (fun bg n -> bg |> Array.map (BingoGame.update n))
        |> Array.sortBy (fun cbg -> cbg.CompletedTimeStamp)

    let winningBingoGame = completedBingoGames |> Array.head
    printfn "Part 1: result is %d" winningBingoGame.Score

    let losingBingoGame = completedBingoGames |> Array.last
    printfn "Part 2: result is %d" losingBingoGame.Score
    0