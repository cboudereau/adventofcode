module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

module Tuple = 
    let map f (x, y) = f x, f y

let split delimiter (input:string) = 
    let pos = input.IndexOf(delimiter:char)
    if pos = -1 then failwithf "unexpected '%c'" delimiter
    else input.Substring(0, pos), input.Substring(pos + 1)

let parse = Array.map (split ',' >> Tuple.map (split '-' >> Tuple.map System.Int32.Parse))

let day41 = 
    Array.map (fun ((x1, x2), (y1, y2)) -> 
        let s1 = set [x1..x2]
        let s2 = set [y1..y2]

        if Set.isSubset s1 s2 then 1
        elif Set.isSubset s2 s1 then 1
        else 0
    )
    >> Array.sum

"""2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""
|> fun x -> x.Split('\n')
|> parse
|> day41
|> Test.assertEq "input day41" 2

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath)

"""/day4.txt"""
|> readFile
|> parse
|> day41
|> Test.assertEq "full input day41" 657