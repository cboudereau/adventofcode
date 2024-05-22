module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let readAll filePath = System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))

let stringLength (x:string) = 
    let rec count counter = 
        function
        | [] -> counter
        | '"'::t -> count counter t
        | '\\'::'x'::_::_::t 
        | '\\'::_::t 
        | _::t -> count (counter + 1) t
    x |> Seq.toList |> count 0

let encodedStringLength (x:string) = 
    let rec count counter = 
        function
        | [] -> counter
        | '"'::t 
        | '\\'::t -> count (counter + 2) t
        | _::t -> count (counter + 1) t

    2 + (x |> Seq.toList |> count 0)


let part1 = Array.map (fun (x:string) -> (x.Length, stringLength x)) >> Array.fold (fun s (codeLength, stringLenght) -> s + codeLength - stringLenght) 0

"day8.example.txt" |> readAll |> part1 |> Test.assertEq "part1" 12
"day8.txt" |> readAll |> part1 |> Test.assertEq "part1" 1371

let part2 = Array.map (fun (x:string) -> (x.Length, encodedStringLength x)) >> Array.fold (fun s (codeLength, encodedStringLength) -> s + encodedStringLength - codeLength) 0

"day8.example.txt" |> readAll |> part2 |> Test.assertEq "part2" 19
"day8.txt" |> readAll |> part2 |> Test.assertEq "part2" 2117
