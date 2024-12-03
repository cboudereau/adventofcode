let readFile filePath = System.IO.File.ReadAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))

let parseNumber idx (input:string) =
    let rec parseNumber idx result (input:string) = 
        if idx >= input.Length || not (System.Char.IsNumber(input[idx])) then 
            if result > 0L then Some (idx, result)
            else None
        else 
            let result = result * 10L + (int64 (input[idx] - '0'))
            parseNumber (idx + 1) result input
    parseNumber idx 0L input

let parseChar char idx (input:string) = 
    if idx >= input.Length || input[idx] <> char then None
    else Some (idx+1)

let parseString (s:string) idx (input:string) = 
    if idx >= input.Length || input.IndexOf(s, idx) <> idx then None
    else Some (idx + s.Length)

let parseMul (idx:int) (input:string) = 
    parseString "mul(" idx input
    |> Option.bind (fun idx -> 
        parseNumber idx input
        |> Option.bind (fun (idx, x) ->
            parseChar ',' idx input
            |> Option.bind (fun idx ->
                parseNumber idx input
                |> Option.bind (fun (idx, y) ->
                    parseChar ')' idx input
                    |> Option.map (fun idx -> idx, (x,y))))))

let parseMuls (idx:int) (input:string) = 
    let rec parse result (idx:int) (input:string) =
        if idx >= input.Length then result
        else
            match parseMul idx input with
            | Some (idx, r) -> parse (r::result) idx input
            | None -> parse result (idx+1) input
    parse [] idx input

let part1 = parseMuls 0 >> List.map (fun (x,y) -> x * y) >> List.sum

"sample.txt" |> readFile |> part1 = 161
"input.txt" |> readFile |> part1 = 159892596

let parseMuls2 (idx:int) (input:string) = 
    let mutable enabled = 1L
    let rec parse result (idx:int) (input:string) =
        if idx >= input.Length then result
        else
            match parseString "do()" idx input, parseString "don't()" idx input, parseMul idx input with
            | None, None, Some (idx, (x,y)) -> parse ((enabled, x, y)::result) idx input
            | Some idx, None, None -> 
                enabled <- 1L
                parse result idx input
            | None, Some idx, None ->
                enabled <- 0L
                parse result idx input 
            | _ -> parse result (idx+1) input
    parse [] idx input

let part2 = parseMuls2 0 >> List.map (fun (x,y,z) -> x * y * z) >> List.sum

"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" |> part2 = 48

"input.txt" |> readFile |> part2 = 92626942