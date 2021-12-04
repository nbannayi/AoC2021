open AoC2021.Utilities

// Advent of Code 2021 day 3.

open System

type GreekMeasures = GammaRate | EpsilonRate
type GasMeasures = OxygenGeneratorRating | CO2ScrubberRating
    
[<EntryPoint>]
let main _argv =
    let binaryNumbers =
        "InputFiles/Day03Input.txt"
        |> Seq.ofFileLines        

    let getSignificantBits binaryNumbers =

        let getSignificantBitPair binaryDigits =            
            let pairs =
                binaryDigits
                |> Seq.countBy (fun b -> b)
                |> Seq.sortBy (fst)
            let zerosCount, onesCount =
                pairs |> Seq.head |> snd, pairs |> Seq.last |> snd
            if onesCount >= zerosCount then (1,0) else (0,1)

        binaryNumbers
        |> Seq.transpose
        |> Seq.map (getSignificantBitPair)
        
    let getGreekMeasureValue binaryNumbers measure =
        let binaryMeasureValue =

            let significantBits = binaryNumbers |> getSignificantBits

            let measureBits =
                match measure with
                | GammaRate   -> significantBits |> Seq.map (fst >> string)
                | EpsilonRate -> significantBits |> Seq.map (snd >> string)
            in measureBits |> Seq.reduce (+)

        Convert.ToInt32(binaryMeasureValue, 2)

    let getGasMeasureValue (binaryNumbers: seq<string>) measure =

        let processBinaryNumbers (binaryNumbers: seq<string>) index measure =
            let indexedSignificantBits =
                binaryNumbers
                |> getSignificantBits
                |> Array.ofSeq
            let requiredBit =
                match measure with
                | OxygenGeneratorRating -> fst indexedSignificantBits.[index]
                | CO2ScrubberRating     -> snd indexedSignificantBits.[index]        

            if binaryNumbers |> Seq.length > 1 then
                binaryNumbers            
                |> Seq.filter (fun bn -> string bn.[index] = string requiredBit)
            else
                binaryNumbers        

        let binaryMeasureValue =
            let noDigits = binaryNumbers |> (Seq.head >> Seq.length)
            (binaryNumbers, [0..noDigits-1])
            ||> List.fold (fun bn index -> processBinaryNumbers bn index measure)
            |> Seq.head            

        Convert.ToInt32(binaryMeasureValue, 2)

    let gr, er, ogr, csr =
        getGreekMeasureValue binaryNumbers GammaRate,
        getGreekMeasureValue binaryNumbers EpsilonRate,
        getGasMeasureValue   binaryNumbers OxygenGeneratorRating,
        getGasMeasureValue   binaryNumbers CO2ScrubberRating

    let powerConsumption = gr * er
    printfn "Part 1: result is %d" powerConsumption
   
    let lifeSupportRating = ogr * csr
    printfn "Part 2: result is %d" lifeSupportRating
    0