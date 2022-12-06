module Parsing = 
    let tryInt x = 
        match System.Int16.TryParse(x:string) with
        | true, v -> Some v
        | _ -> None

let parse (x:string) = 
    let lines = x.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    let row (x:string) = 
        x.Split(',', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose Parsing.tryInt

    lines |> Array.map row |> Array.head |> Array.toList

let run n x = 
    let rec run n day (x:System.Int16 list) = 
        printfn "day %i" day
        if n = day then x
        else 
            let newDay = function
                | 0s -> [6s;8s]
                | x -> [x - 1s]
            x |> List.collect newDay |> run n (day + 1)

    run n 0 x

let score = List.length

let input = """3,4,3,1,2"""


run 4 [1s]

input 
|> parse 
|> run 18 
|> score = 26

input |> parse |> run 80 |> score = 5934

let runAll n = parse >> run n >> score

__SOURCE_DIRECTORY__ + "/part1.csv"
|> System.IO.File.ReadAllText
|> runAll 80 = 353274

let r = 
    __SOURCE_DIRECTORY__ + "/part1.csv"
    |> System.IO.File.ReadAllText
    |> parse |> run 257 



