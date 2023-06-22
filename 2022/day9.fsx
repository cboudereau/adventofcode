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

type Command = 
    | Left
    | Right
    | Up
    | Down

type Coords = int * int

module Coords = 
    let add (x1,y1) (x2,y2) : Coords = (x1+x2), (y1+y2)
    let connect (x1,y1) ((x2,y2) as tail) : Coords = 
        let (dx, dy) = (x1 - x2), (y1 - y2)
        if abs dx > 1 || abs dy > 1 then 
            (sign dx, sign dy) |> add tail
        else tail

module Command = 
    let parse =
        function
        | Regex """R (\d+)""" [Int32 x] -> x, Right
        | Regex """L (\d+)""" [Int32 x] -> x ,Left
        | Regex """U (\d+)""" [Int32 x] -> x, Up
        | Regex """D (\d+)""" [Int32 x] -> x, Down
        | other -> failwithf "unexpected command %s" other

    let toCoords = function
        | Up -> (0, 1) : Coords
        | Down -> (0, -1)
        | Right -> (1, 0)
        | Left -> (-1, 0)

Command.parse "R 4" |> Test.assertEq "day 8-1 command parsing" (4, Right)
Command.parse "U 4" |> Test.assertEq "day 8-1 command parsing" (4, Up)
Command.parse "L 3" |> Test.assertEq "day 8-1 command parsing" (3, Left)
Command.parse "D 1" |> Test.assertEq "day 8-1 command parsing" (1, Down)
Command.parse "L 5" |> Test.assertEq "day 8-1 command parsing" (5, Left)
Command.parse "R 2" |> Test.assertEq "day 8-1 command parsing" (2, Right)

type Rope = Coords list 

module Rope = 
    let zero start length = 
        List.replicate length start : Rope

    let move command (rope:Rope) : Rope = 
        let head = rope |> List.head |> Coords.add (Command.toCoords command)
        rope |> List.tail |> List.scan Coords.connect head

    let toString h l (rope:Rope) = 
        let sb = new System.Text.StringBuilder()
        let head = rope |> List.head
        let lastIdx = (rope |> List.length) - 1
        [h-1..-1..0]
        |> List.iter(fun ys -> 
            [0..l-1]
            |> List.iter(fun xs ->
                let c = rope |> List.tryFindIndex (fun (x,y) -> x=xs && y=ys) |> Option.map (fun idx ->  if lastIdx=idx then "T" elif rope[idx]=head then "H" else sprintf "%i" idx) |> Option.defaultValue "." 
                sb.Append(c) |> ignore
            )
            
            sb.AppendLine() |> ignore
        )
        sb.ToString()

// Part one
Rope.zero (0,0) 2
|> Rope.move Right 
|> Rope.move Right 
|> Rope.move Right 
|> Rope.move Right 

|> Rope.move Up 
|> Rope.move Up 
|> Rope.move Up 
|> Rope.move Up 

|> Rope.move Left 
|> Rope.move Left
|> Rope.move Left

|> Rope.move Down

|> Rope.move Right 
|> Rope.move Right 
|> Rope.move Right 
|> Rope.move Right 

|> Rope.move Down

|> Rope.move Left
|> Rope.move Left
|> Rope.move Left
|> Rope.move Left
|> Rope.move Left

|> Rope.move Right
|> Rope.move Right

|> Rope.toString 5 6
|> Test.assertEq 
"......
......
.TH...
......
......"
|> printfn "%s"

//Part 2
Rope.zero (0,0) 10
|> Rope.move Right 
|> Rope.move Right 
|> Rope.move Right 
|> Rope.move Right 

|> Rope.move Up 
|> Rope.move Up 
|> Rope.move Up 
|> Rope.move Up 

|> Rope.move Left 
|> Rope.move Left 
|> Rope.move Left 

|> Rope.move Down

|> Rope.move Right
|> Rope.move Right
|> Rope.move Right
|> Rope.move Right

|> Rope.move Down

|> Rope.move Left
|> Rope.move Left
|> Rope.move Left
|> Rope.move Left
|> Rope.move Left

|> Rope.move Right
|> Rope.move Right

|> Rope.toString 5 6
|> printfn "%s"

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath)

let day9 l = 
    """/day9.txt"""
    |> readFile
    |> Array.map Command.parse
    |> Array.fold(fun state (count, command) -> 
        List.replicate count command
        |> List.fold (fun (rope, visited) command -> 
            let rope = Rope.move command rope
            let last = rope |> List.last
            let visited = visited |> Set.add last 
            (rope, visited)
        ) state
    ) (Rope.zero (0,0) l, Set.singleton (0,0))
    |> snd
    |> Set.count

day9 2 = 5907

day9 10 = 2303