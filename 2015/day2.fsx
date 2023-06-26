module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int32|_|) x = 
    match System.Int32.TryParse(x:string) with
    | true, v -> Some v
    | false, _ -> None

let readFile filePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllLines

let flip f x y = f y x

let area (l,w,h) = 
    2*l*w + 2*w*h + 2*h*l + ([l;w;h] |> List.sortDescending |> List.tail |> List.fold (*) 1)

area (2,3,4) |> Test.assertEq "area 58" 58
area (1,1,10) |> Test.assertEq "area 43" 43

let lwh = 
    function
    | Regex ("""(\d+)x(\d+)x(\d+)""") [Int32 l; Int32 w; Int32 h] -> (l,w,h)
    | other -> failwithf "unexpected input '%s'" other

"./day2.txt"
|> readFile
|> Seq.fold (flip (lwh >> area >> (+))) 0
|> Test.assertEq "part1" 1588178

let perimeter (l,w,h) = 
    l*w*h + ([l;w;h] |> List.sortDescending |> List.tail |> List.fold (flip ((*) 2 >> (+))) 0)

perimeter (2,3,4) |> Test.assertEq "perimeter 34" 34
perimeter (1,1,10) |> Test.assertEq "perimeter 14" 14

"./day2.txt"
|> readFile
|> Seq.fold (flip (lwh >> perimeter >> (+))) 0
|> Test.assertEq "part2" 3783758
