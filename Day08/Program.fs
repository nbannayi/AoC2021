open AoC2021.Utilities
open AoC2021.Domain

// Advent of Code 2021 day 8.

[<EntryPoint>]
let main _argv =    

    let digitsLines =        
        "InputFiles/Day08Input.txt"
        |> Seq.ofFileLines        
        |> Seq.map (fun dl -> dl.Split " | ")

    let output1478s =
        digitsLines
        |> Seq.map (fun dl -> dl.[1].Split " ")
        |> Seq.collect (fun d -> d)
        |> Seq.filter (fun d -> [2; 3; 4; 7] |> List.contains (d.Length))

    printfn "Part 1: result is %d" (output1478s |> Seq.length)

    // Yeah - I'm pretty sure I have over complicated it!!
    let getDigitsSequence (examples: string array) =

        let one = examples |> Array.find (fun e -> e.Length = 2)
        let four = examples |> Array.find (fun e -> e.Length = 4)
        let seven = examples |> Array.find (fun e -> e.Length = 3)

        // Work out possible 'c', 'f' = elements of one.
        let c', f' = one.[0], one.[1] // ok

        // Work out possible 'b', 'd' = elements of four minus one.
        let b', d' = let diff = (set(four) - set(one)) |> Set.toArray in diff.[0], diff.[1] // ok

        // Work out 'a' = seven minus one.
        let a = (set(seven) - set(one)) |> Set.minElement // ok 
        
        // Work out 'b' = 6 digit number where b' appears but not d'.
        // This then provides 'd'.
        let b =
           let filter = examples |> Array.filter (fun e ->
              e.Length = 6 &&
              ((e.Contains(b') && e.Contains(d') = false) || (e.Contains(d') && e.Contains(b') = false)))
           in (Set.intersect (set(filter.[0])) (set [b';d'])) |> Set.minElement |> char
        let d = if b = b' then d' else b' // ok
        
        // Work out 'f' = intersection of 5 digit number containing a,b,d and one.
        // This then provides 'c'.
        let f =
            let filter = examples |> Array.filter (fun e -> e.Length = 5 && (set [a;b;d]).IsSubsetOf(set(e)))
            in set(filter.[0]) |> Set.intersect(set(one)) |> Set.minElement
        let c = if f = f' then c' else f'

        // Work out 'g' = intersection of all 5 digit numbers - a - d.
        // Finally this gives us e by elimination.
        let g = 
           let filter = examples |> Array.filter (fun e -> e.Length = 5) |> Array.map (set) |> Array.ofSeq |> Set.intersectMany
           in (filter - (set [a;d])) |> Set.minElement
        let e = (set("abcdefg") - (set [a;b;c;d;f;g])) |> Set.minElement

        [a;b;c;d;e;f;g] |> List.map (string) |> List.reduce (+)

    let getDigitsCode (sequence: string) (codes: string array) =
        let segmentDisplay = SegmentDisplay.create sequence
        codes
        |> Array.map (fun c ->
            segmentDisplay
            |> SegmentDisplay.reset
            |> SegmentDisplay.activateSegments c
            |> SegmentDisplay.getDigit
            |> string)
        |> Array.reduce (+)
        |> int

    let codesCollection =
        digitsLines
        |> Seq.map (fun dl -> dl.[0].Split ' ', dl.[1].Split ' ')
        |> Seq.map (fun dl' ->
            let sequence = getDigitsSequence (fst dl')
            getDigitsCode sequence (snd dl'))
        
    printfn "Part 2: result is %d" (codesCollection |> Seq.sum)
    0