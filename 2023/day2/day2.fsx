module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module Int64 = 
    let tryParse (x:string) = 
        match System.Int64.TryParse(x) with
        | true, v -> Some (v)
        | _ -> None

let (|Int64|_|) = Int64.tryParse

module GameId = 
    let parse = 
        function
        | Regex """^Game\s(\d+)""" [Int64 gameId] -> gameId
        | other -> failwithf "unexpected input: '%s'" other

"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" |> GameId.parse |> Test.assertEq "game id parsing" 1L

let (|GameId|) = GameId.parse

type Cube = {
    Color: string
    Count: int64
}

let cube color n = { Cube.Color= color; Count=n }

module String = 
    let split c x = 
        (x:string).Split((c:char),System.StringSplitOptions.RemoveEmptyEntries)

module Cube = 
    let parse =
        function
        | Regex """(\d+)\s(\w+)""" [Int64 n; color] -> cube color n
        | other -> failwithf "unexpected input: '%s'" other

"3 blue" |> Cube.parse |> Test.assertEq "color parsing" (cube "blue" 3)

let (|Subsets|) = String.split ';' >> Array.map (String.split ',' >> Array.map Cube.parse)

type Subset = Cube []

type Game = 
    { GameId: int64
      Subsets: Subset [] }

let game gameId subsets = { Game.GameId = gameId; Subsets = subsets }

module Game =
    let subsets x = x.Subsets
    let parse (x:string) = 
        match x.Split(':', System.StringSplitOptions.RemoveEmptyEntries) with
        | [|GameId gameId;Subsets subsets|] -> game gameId subsets
        | other -> failwithf "unexpected game input: %A" other

let readAllLines (path) = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllLines

"part1.example.txt" 
|> readAllLines
|> Array.map Game.parse
|> Array.take 1
|> Test.assertEq "parsing part1 example" 
    [| 
        [|[| cube "blue" 3; cube "red" 4 |]; [|cube "red" 1; cube "green" 2; cube "blue" 6|]; [|cube "green" 2|] |] |> game 1L
    |]

let part1 = 
    let validSubset = [ cube "red" 12; cube "green" 13; cube "blue" 14 ] |> List.map (fun x -> x.Color, x.Count) |> Map.ofList
    let validate { Cube.Color=color; Count=number } = 
        let maxCount = validSubset |> Map.find color
        number <= maxCount

    readAllLines
    >> Array.map Game.parse
    >> Array.filter(Game.subsets >> Array.forall (Array.forall validate))
    >> Array.map (fun x -> x.GameId)
    >> Array.sum

"part1.example.txt" |> part1 |> Test.assertEq "part1 example" 8

"input.txt" |> part1 
|> Test.assertEq "part1 solution" 1853L

let maximum = 
    let zero = [cube "red" 0; cube "green" 0; cube "blue" 0] |> List.map (fun x -> x.Color, x.Count) |> Map.ofList 
    let maximum m (x:Subset) = 
        x
        |> Array.fold (fun s {Cube.Color=color; Count=count} -> 
            let maxCount = s |> Map.find color
            if count > maxCount then 
                s |> Map.add color count
            else s
        ) m
    
    readAllLines
    >> Array.map Game.parse
    >> Array.map (Game.subsets >> Array.fold maximum zero)

"part1.example.txt" |> maximum 
|> Test.assertEq "part2 maximum example"
    [| 
        Map.ofList(["red",  4L; "green",  2L; "blue",  6L]) 
        Map.ofList(["red",  1L; "green",  3L; "blue",  4L]) 
        Map.ofList(["red", 20L; "green", 13L; "blue",  6L]) 
        Map.ofList(["red", 14L; "green",  3L; "blue", 15L]) 
        Map.ofList(["red",  6L; "green",  3L; "blue",  2L]) 
    |]

let sum = Array.map (Map.fold (fun s _ v -> s * v) 1L)

"part1.example.txt"
|> maximum
|> sum
|> Test.assertEq "part2 maximum + power example" [|48L; 12L; 1560L; 630L; 36L|]

let part2 = maximum >> sum >> Array.sum

"part1.example.txt"
|> part2
|> Test.assertEq "part2 example" 2286L 

"input.txt"
|> part2
|> Test.assertEq "part2"