#r "nuget:FSharp.Data"

module Async = 
    let map f x = async.Bind(x, f >> async.Return)

let flip f x y = f y x

open FSharp.Data

let [<Literal>] csvFile = __SOURCE_DIRECTORY__ + """/part1.csv"""

type CsvInput = CsvProvider< csvFile, HasHeaders=false >

let r = 
    let increases x = function
        | None -> Some (x, 0)
        | Some (previous, count) when x > previous -> 
            Some(x, count+1)  
        | Some (_, count) -> 
            Some (x, count)
    CsvInput.AsyncLoad(csvFile) 
    |> Async.map (fun x -> x.Rows |> Seq.map (fun x-> x.Column1) |> Seq.fold (flip increases) None |> Option.map snd |> Option.defaultValue 0)
    |> Async.RunSynchronously