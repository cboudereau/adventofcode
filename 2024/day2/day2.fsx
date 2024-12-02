let readFile filePath = 
    System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))
    |> Array.map (fun x -> x.Split(' ') |> Array.map System.Int64.Parse)


let part1 count x = 
    let isSafe (a:int64[]) = 
        let rec isSafe isNegative (a:int64 list) = 
            match a, isNegative with
            | h1::h2::_, None -> isSafe (Some (h2 < h1)) a
            | h1::h2::_, Some isNegative when let v = h2 - h1 in let va = abs (h2 - h1) in va > 3 || va < 1 || (isNegative && v > 0) || (not isNegative && v < 0) -> false
            | _::t, _ -> isSafe isNegative t
            | [], _ -> true
        a |> Array.toList |> isSafe None
    x 
    |> Array.filter isSafe
    |> Array.length


"sample.txt" |> readFile |> part1 0 = 2
"day2.txt" |> readFile |> part1 0 = 269
