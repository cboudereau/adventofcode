let readFile filePath = 
    System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))
    |> Array.map (fun x -> x.Split(' ') |> Array.map System.Int64.Parse)


let part1 x = 
    let isSafe (a:int64[]) = 
        // 7 6 4 2 1
        let rec isSafe isNegative (a:int64 list) = 
            match a, isNegative with
            | h1::h2::_, None -> isSafe (Some (h2 < h1)) a
            | h1::h2::_, Some isNegative when let v = abs (h2 - h1) in v > 3 || v < 1 || (isNegative && v > 0) || (not isNegative && v < 0) -> false
            | _::t, _ -> isSafe isNegative t
            | [], _ -> true
        a |> Array.toList |> isSafe None
    x 
    |> Array.filter isSafe
    // |> Array.length


"sample.txt" |> readFile |> part1 //= 2
"day2.txt" |> readFile |> part1
