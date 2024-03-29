module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

let parse (path) = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllLines

module Int = 
    let tryParse (x:string) = 
        match System.Int32.TryParse(x) with
        | true, v -> Some (v)
        | _ -> None

    
let prune (x:string) = 
    let digits = 
        ["one", 1; "two", 2; "three", 3; "four", 4; "five", 5; "six", 6; "seven", 7; "eight", 8; "nine", 9 ]

    let rec prune r (x:string) = 
        if System.String.IsNullOrEmpty x then r
        else
            let found = 
                digits
                |> Seq.filter (fst >> x.StartsWith)
                |> Seq.tryHead
            
            let pruned = found |> Option.map (snd >> sprintf "%i") |> Option.defaultWith (fun () -> sprintf "%c" x[0]) |> sprintf "%s%s" r
            x.Substring(1) |> prune pruned
    prune "" x        

prune "two1nine" |> Test.assertEq "prune 219" "2wo19ine"
prune "eightwothree" |> Test.assertEq "prune 8 3" "8igh2wo3hree"
prune "7pqrstsixteen" |> Test.assertEq "prune 7 6" "7pqrst6ixteen"

let part1 (input:string []) = 
    input
    |> Array.map (fun x -> x |> Seq.choose(Array.singleton >> System.String >> Int.tryParse) |> Seq.toArray)
    |> Array.map (fun x -> 10 * (x |> Seq.head) + (x |> Seq.last))

"part1.example.txt" |> parse |> part1 |> Array.toList |> Test.assertEq "part1 example" [12;38;15;77]
"part2.example.txt" |> parse |> Array.map prune |> part1 |> Array.toList |> Test.assertEq "part2 example" [29; 83; 13; 24; 42; 14; 76]

"input.txt" |> parse |> part1 |> Array.sum |> Test.assertEq "part1 solution" 56397
"input.txt" |> parse |> Array.map prune |> part1 |> Array.sum |> Test.assertEq "part2 solution" 55701