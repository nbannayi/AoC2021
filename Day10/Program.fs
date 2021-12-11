open AoC2021.Utilities

// Advent of Code 2021 day 10.
// TODO: Incomplete!

[<EntryPoint>]
let main _argv =    

    let syntaxLines =        
        "InputFiles/Day10ExampleInput.txt"
        |> Seq.ofFileLines

    let openingBrackets = [|'(';'[';'{';'<'|]
    let closingBrackets = [|')';']';'}';'>'|]
    
    let inputStack = Stack.empty
    let inputStack' = inputStack |> Stack.push 'a'

    let syntaxLine = "[({(<(())[]>[[{[]{<()<>>"
    (*
    let parseSyntaxSymbol stack symbol =
        let stack' = stack |> Stack.push symbol

        let symbolTop, symbol2ndToTop =
            stack' |> Stack.peek, stack' |> Stack.peekPrevious

        match symbolTop, symbol2ndToTop with
        | '(',')' |  -> stack' |> Stack.pop |> snd |> Stack.pop |> snd            
        0
    *)
    let parseSyntaxLine syntaxLine =
        let evaluateStack = Stack.empty
        syntaxLine |> Array.iter 

    syntaxLine |> Seq.iter (printfn "%A")
    0 // return an integer exit code