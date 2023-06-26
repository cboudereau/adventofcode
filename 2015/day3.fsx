module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let readFile filePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllText

let flip f x y = f y x

let parse = function '^' -> (0, 1) | 'v' -> (0, -1) | '>' -> (1, 0) | '<' -> (-1, 0) | other -> failwithf "unexpected '%c'" other

let add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

let append s x = x, x::s

let part1 x = 
    x |> Seq.fold(fun (c,l) x -> x |> parse |> add c |> append l) ((0,0), [(0,0)])
    |> snd |> Seq.groupBy id |> Seq.length 

">" |> part1 |> Test.assertEq "simple" 2
"^>v<" |> part1 |> Test.assertEq "test 4" 4
"^v^v^v^v^v" |> part1 |> Test.assertEq "lucky children" 2

"./day3.txt" |> readFile |> part1 |> Test.assertEq "part1" 2081


type Turn = Santa | RobotSanta
let part2 x = 
    x |> Seq.fold (fun ((cs, crs, t), l) x -> 
        let next = x |> parse |> add
        match t with
        | Santa -> 
            let cs = next cs
            (cs, crs, RobotSanta), (cs::l)
        | RobotSanta ->
            let crs = next crs
            (cs, crs, Santa), (crs::l)
    ) (((0,0),(0,0), Santa), [(0,0)])
    |> snd
    |> Seq.groupBy id
    |> Seq.length

"^v" |> part2 |> Test.assertEq "simple" 3
"^>v<" |> part2 |> Test.assertEq "simple" 3
"^v^v^v^v^v" |> part2 |> Test.assertEq "11" 11

"./day3.txt" |> readFile |> part2 |> Test.assertEq "part2" 2341
