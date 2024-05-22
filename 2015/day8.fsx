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

let part1 = Array.map (fun (x:string) -> (x.Length, stringLength x)) >> Array.fold (fun s (codeLength, stringLenght) -> s + codeLength - stringLenght) 0

readAll "day8.example.txt" |> part1 |> Test.assertEq "part1" 12
readAll "day8.txt" |> part1 |> Test.assertEq "part1" 1371