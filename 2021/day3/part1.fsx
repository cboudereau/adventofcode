let convert x = System.Convert.ToInt32(x, 2)
convert "10110" = 22

let (|Weight|_|) = function
    | '0' -> Some -1
    | '1' -> Some 1
    | _ -> None
    
let mostCommon = 
    let convert x = if x > 0 then '1' else '0' 
    Seq.toArray
    >> Array.map convert
    >> System.String


let lessCommon = 
    let convert x = if x > 0 then '0' else '1' 
    Seq.toArray
    >> Array.map convert
    >> System.String

let weight =
    let map2 f s x = 
        match s with
        | [] -> x
        | s -> List.map2 f s x
    List.fold (fun s x -> 
        x
        |> Seq.toList
        |> Seq.choose (function '0' -> Some -1 | '1' -> Some 1 | _ -> None)
        |> Seq.toList
        |> map2 (+) s) []


let gammaRate = mostCommon >>  convert
let epsilonRate = lessCommon >> convert

let w = 
    [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
    ] 
    |> weight 

w |> gammaRate = 22
w |> epsilonRate = 9

(w |> gammaRate) * (w |> epsilonRate) = 198 

let tw = 
    __SOURCE_DIRECTORY__ + "/part1.csv"
    |> System.IO.File.ReadAllLines
    |> Array.toList 
    |> weight

(gammaRate tw) * (epsilonRate tw)
