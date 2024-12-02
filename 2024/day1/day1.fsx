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
    |> Array.fold (fun (left,right) (l,r) -> l::left, r::right) ([],[])

let part1 x =
    x
    |> fun (l,r) -> l |> List.sort, r |> List.sort
    |> fun (l,r) -> List.zip l r
    |> List.map (fun (l,r) -> abs (r - l))
    |> List.sum

"sample.txt" |> readFile |> part1 = 11
"day1.txt" |> readFile |> part1 = 2904518

let part2 x = 
    x 
    |> fun (l, r) -> l |> List.map (fun x -> x * ((r |> List.filter ((=) x)) |> List.length))
    |> List.sum

"sample.txt" |> readFile |> part2 = 31
"day1.txt" |> readFile |> part2