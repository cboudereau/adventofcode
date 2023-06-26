module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let readFile filePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllText

let flip f x y = f y x

let parse = function '(' -> 1 | ')' -> -1 | other -> failwithf "unexpected '%c'" other

"./day1.txt"
|> readFile
|> Seq.fold (flip (parse >> (+))) 0 |> Test.assertEq "part1" 280