let readFile filePath = 
    System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))
    |> Array.map (fun (x:string) -> 
            let left = 
                let pos = x.IndexOf(' ')
                x.Substring(0, pos) |> System.Int32.Parse
            let right = 
                let pos = x.LastIndexOf(' ')
                x.Substring(pos + 1) |> System.Int32.Parse
            left, right
        )

let day1 x =
    x
    |> Array.fold (fun (left,right) (l,r) -> l::left, r::right) ([],[])
    |> fun (l,r) -> l |> List.sort, r |> List.sort
    |> fun (l,r) -> List.zip l r
    |> List.map (fun (l,r) -> abs (r - l))
    |> List.sum

"sample.txt" |> readFile |> day1 = 11
"day1.txt" |> readFile |> day1 = 2904518