module Async = 
    let map f x = async.Bind(x, f >> async.Return)

module Parsing = 
    let tryInt x = 
        match System.Int32.TryParse(x:string) with
        | true, v -> Some v
        | _ -> None
    let (|Int32|_|) = tryInt

let flip f x y = f y x

let increases z = function
    | None -> Some (0, (None, [z]))
    | Some (t, (p, [y;x])) ->
        let sum = x + y + z

        let t = p |> Option.map(fun p -> if sum > p then t + 1 else t) |> Option.defaultValue t

        Some (t, (Some sum, [z;y]))

    | Some (t, (p, others)) -> Some (t, (p, z::others))


[
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
]
|> Seq.fold (flip increases) None
|> Option.map fst
|> Option.defaultValue 0


let r = 
    __SOURCE_DIRECTORY__ + "/part2.csv"
    |> System.IO.File.ReadAllLinesAsync
    |> Async.AwaitTask
    |> Async.map (
        Seq.choose Parsing.tryInt 
        >> Seq.fold (flip increases) None
        >> Option.map fst
        >> Option.defaultValue 0)
    |> Async.RunSynchronously

