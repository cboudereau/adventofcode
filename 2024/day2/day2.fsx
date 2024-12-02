let readFile filePath = 
    System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))
    |> Array.map (fun x -> x.Split(' ') |> Array.map System.Int64.Parse)

let isSafe (a:int64[]) = 
    let rec isSafe isNegative (a:int64 list) = 
        match a, isNegative with
        | h1::h2::_, None -> isSafe (Some (h2 < h1)) a
        | h1::h2::_, Some isNegative when let v = h2 - h1 in let va = abs (h2 - h1) in va > 3 || va < 1 || (isNegative && v > 0) || (not isNegative && v < 0) -> false
        | _::t, _ -> isSafe isNegative t
        | [], _ -> true
    a |> Array.toList |> isSafe None

let isSafeN (a:int64[]) = 
    let rec isSafeN idx (b:int64[]) = 
        if isSafe b then true
        else 
            if idx < (a |> Array.length) then 
                let b = a |> Array.removeAt idx
                isSafeN (idx + 1) b
            else false
    isSafeN 0 a

let part1 x = 
    x 
    |> Array.filter isSafe
    |> Array.length

"sample.txt" |> readFile |> part1 = 2
"day2.txt" |> readFile |> part1 = 269

let part2 x = 
    x 
    |> Array.filter isSafeN
    |> Array.length

"sample.txt" |> readFile |> part2 = 4
"day2.txt" |> readFile |> part2 = 337
