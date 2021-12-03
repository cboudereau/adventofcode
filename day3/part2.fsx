let convert x = System.Convert.ToInt32(x, 2)
convert "10110" = 22

let flip f x y = f y x

let rate f v = 
    let rec rate f v (pos:int) = function
        | [h] -> h
        | [_;_] as l -> l |> List.find (flip Array.get pos >> (=) v)
        | l -> 
            let (ones, zeros) = l |> List.partition (flip Array.get pos >> (=) '0')
            match (zeros |> List.length) - (ones |> List.length) with
            | s when f s -> rate f v (pos + 1) ones 
            | _ -> rate f v (pos + 1) zeros
    
    List.map (fun x -> (x:string) |> Seq.toArray)
    >> rate f v 0
    >> System.String

let oxygen = rate ((>) 0) '1'

let co2 = rate ((<) 0) '0'

let input = 
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
input |> oxygen |> convert = 23
input |> co2 |> convert = 10

(input |> oxygen |> convert) * (input |> co2 |> convert) = 230


let inputs =     
    __SOURCE_DIRECTORY__ + "/part2.csv"
    |> System.IO.File.ReadAllLines
    |> Array.toList 

let o = inputs |> oxygen |> convert
let c = inputs |> co2 |> convert

o * c = 1353024
