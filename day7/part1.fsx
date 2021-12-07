module Parsing = 
    let tryInt x = 
        match System.Int32.TryParse(x:string) with
        | true, v -> Some v
        | _ -> None


let solve f l = 
    let mn = l |> List.min
    let mx = l |> List.max

    let possibilites = [mn..mx]
    
    let allSolutions = 
        possibilites
        |> List.map (fun p ->
            p, l |> List.map (fun x -> 
                let cost = f x p
                x, cost
            )
        )

    allSolutions |> List.minBy (snd >> List.sumBy snd) |> snd |> List.sumBy snd

let parse x = (x:string).Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> List.choose Parsing.tryInt

let part1Cost x y = abs (x - y)

let part2Cost x y = 
    let cost = abs (x - y)
    if cost < 2 then cost
    else [1..cost] |> List.sum

part2Cost 4 1 = 1 + 2 + 3

"""16,1,2,0,4,2,7,1,2,14""" |> parse |> solve part1Cost = 37
"""16,1,2,0,4,2,7,1,2,14""" |> parse |> solve part2Cost = 168

let input = __SOURCE_DIRECTORY__ + "/part1.csv" |> System.IO.File.ReadAllText |> parse

input |> solve part1Cost = 359648