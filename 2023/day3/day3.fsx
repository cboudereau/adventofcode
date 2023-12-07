module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module Int64 = 
    let tryParse (x:string) = 
        match System.Int64.TryParse(x) with
        | true, v -> Some (v)
        | _ -> None

let (|Int64|_|) = Int64.tryParse

let adjencies (mini,maxi) (minj,maxj) (i,j) = 
    [ i - 1, j
      i - 1, j - 1
      i    , j - 1
      i + 1, j - 1
      i - 1, j + 1
      i + 1, j
      i + 1, j + 1
      i    , j + 1 ]
    |> List.filter (fun (i, j) -> i >= mini && i < maxi && j >= minj && j < maxj)

(0,0) |> adjencies (0,5) (0,3) |> Test.assertEq "adjencies: upper left" [(1, 0); (1, 1); (0, 1)]
(4,2) |> adjencies (0,5) (0,3) |> Test.assertEq "adjencies: bottom right" [(3, 2); (3, 1); (4, 1)]
(2, 1) |> adjencies  (0, 5) (0, 3) |> Test.assertEq "adjencies: middle" [(1, 1); (1, 0); (2, 0); (3, 0); (1, 2); (3, 1); (3, 2); (2, 2)]

let readAllLines (path) = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllLines

let flip f x y = f y x

let isSymbol c = System.Char.IsDigit c |> not && c <> '.'

let symbolsMap = 
    readAllLines 
    >> Array.map Seq.toArray
    >> fun m -> 
        let adjencies = 
            let maxi = m |> Array.length
            let maxj = m |> Array.head |> Array.length
            adjencies (0, maxi) (0, maxj)
        let m = 
            m
            |> Array.mapi (fun i a -> a |> Array.mapi (fun j x -> (i,j),x))
            |> Array.concat
        m
        |> Array.fold (fun ((symbols, digits), r) (coords, c) -> 
            if System.Char.IsDigit(c) then 
                let adjacencies = adjencies coords
                let adjacentSymbols = 
                    m 
                    |> Array.filter (fst >> flip List.contains adjacencies)
                    |> Array.filter (snd >> isSymbol)
                    
                (Array.append symbols adjacentSymbols, (coords, c)::digits), r
            else
                let r = 
                    match symbols with
                    | [||] -> r
                    | _ -> 
                        let digits = digits |> List.rev
                        let topleft = digits |> List.head |> fst
                        (symbols, (topleft, (digits |> List.map snd |> List.toArray |> System.String |> System.Int64.Parse)))::r
                ([||], []), r
        ) (([||], []), [])
    >> snd

let part1 = symbolsMap >> List.sumBy (snd >> snd)

"part1.example.txt" |> part1 |> Test.assertEq "part1 example" 4361L
"input.txt" |> part1 |> Test.assertEq "part1 solution" 535078L

let part2 = 
    symbolsMap
    >> Seq.collect (fun (l,x) -> l |> Seq.map (fun y -> (y, x))) 
    >> Seq.groupBy fst
    >> Seq.map (fun (k, v) -> k, v |> Seq.map snd |> Seq.toList |> List.distinct)
    >> Seq.filter (snd >> List.length >> (<) 1)
    >> Seq.map (snd)
    >> Seq.fold (fun s x -> s + (x |> List.fold (fun s x -> s * snd x) 1L)) 0L

"part1.example.txt" |> part2 |> Test.assertEq "part2 example" 467835L 
"input.txt" |> part2 |> Test.assertEq "part2 example" 75312571L 