module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let readAll filePath = System.IO.File.ReadAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))

let sum (s:string) = 
    let rec sum r current = 
        let digit l = 
            if l |> List.isEmpty then 0L
            else l |> List.rev |> List.toArray |> System.String |> System.Int64.Parse 
        function
        | [] -> digit current + r
        | h::t when System.Char.IsNumber h || h = '-' -> sum r (h::current) t
        | _::t -> sum (digit current + r) [] t
    s |> Seq.toList |> sum 0 []


"""[1,2,3]""" |> sum |> Test.assertEq "example" 6
"""{"a":2,"b":4}""" |> sum |> Test.assertEq "example" 6
"""[[[3]]]""" |> sum |> Test.assertEq "example" 3
"""{"a":{"b":4},"c":-1}""" |> sum |> Test.assertEq "example" 3
"""{"a":[-1,1]}""" |> sum |> Test.assertEq "example" 0
"""[-1,{"a":1}]""" |> sum |> Test.assertEq "example" 0

"day12.txt" |> readAll |> sum |> Test.assertEq "part1" 119433