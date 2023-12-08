module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module Int32 = 
    let tryParse (x:string) = 
        match System.Int32.TryParse(x) with
        | true, v -> Some (v)
        | _ -> None

let (|Int32|_|) = Int32.tryParse

module String = 
    let split c x = 
        (x:string).Split((c:char),System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

module Tuple = 
    let two x y = (x,y)

let (|Split|) = String.split

let readAllLines path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllLines

let flip f x y = f y x

module CardId = 
    let parse = 
        function
        | Regex """^Card\s(\d+)""" [Int32 gameId] -> gameId
        | other -> failwithf "unexpected input: '%s'" other

"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" |> CardId.parse |> Test.assertEq "card id parsing" 1

module Scratchcard = 
    let parse = 
        function
        | Split ':' [ Regex """^Card\s+(\d+)""" [Int32 cardId]; Split '|' [Split ' ' winningNumbers; Split ' ' numbers] ] -> 
            let winningNumbers = set winningNumbers
            let numbersWon = numbers |> set |> Set.intersect winningNumbers |> Set.toList
            (cardId, numbersWon)
        | other -> failwithf "failed to parse scratchcard: '%s'" other

let part1 = 
    readAllLines
    >> Array.map Scratchcard.parse
    >> Array.sumBy (snd >> List.fold (fun s _ -> if s = 0 then 1 else 2 * s) 0)

"part1.example.txt" |> part1 |> Test.assertEq "part1 example" 13
"input.txt" |> part1 |> Test.assertEq "part1 example" 23678

let part2 = 
    readAllLines
    >> Array.map Scratchcard.parse
    >> Tuple.two Map.empty
    >> fun (copies, scratchcards) -> 
        (copies, scratchcards) 
        ||> Array.fold (fun copies scratchcard -> 
            let cardId = fst scratchcard
            let matchingNumberCount = snd scratchcard |> List.length
            let copies = copies |> Map.change cardId (Option.orElse (Some 1))
            let instances = copies[cardId]
            let hits = List.init matchingNumberCount ((+) (cardId + 1))
            (copies, hits) ||> List.fold (flip (fun cardIdHit -> 
                Map.change cardIdHit (Option.orElse (Some 1) >> Option.map ((+) instances)))) 
        ) 
    >> Map.values
    >> Seq.sum

"part1.example.txt" |> part2 |> Test.assertEq "part2 example" 30
"input.txt" |> part2 |> Test.assertEq "part2" 15455663