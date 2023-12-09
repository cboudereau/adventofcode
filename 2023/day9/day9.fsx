module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

module String = 
    let split c x = 
        (x:string).Split((c:string),System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
let (|Split|) = String.split

let readAllLines path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllLines

let (|List|) f l = l |> List.map f

let parse = 
    readAllLines
    >> Array.map(function Split " " (List System.Int64.Parse row) -> row)

let solve =
    Array.map (fun row ->
        let rec diff lasts diffs row = 
            match row with
            | 0L::[] -> 
                lasts |> List.fold (+) 0L
            | h::last::[] ->
                let lasts = last::lasts
                let diffs = (last - h)::diffs
                diffs |> List.rev |> diff lasts []
            | h1::h2::t -> 
                let diffs = (h2 - h1)::diffs
                let rows = h2::t
                diff lasts diffs rows
            | other -> failwithf "unexpected case %A" other
        diff [] [] row
    ) 

let part1 = parse >> solve >> Array.sum

"example.txt" |> part1 |> Test.assertEq "part1 example" 114L
"input.txt" |> part1 |> Test.assertEq "part1 example" 1853145119L

let part2 = parse >> Array.map List.rev >> solve >> Array.sum

"example.txt" |> part2 |> Test.assertEq "part1 example" 2L
"input.txt" |> part2 |> Test.assertEq "part1 example" 923L