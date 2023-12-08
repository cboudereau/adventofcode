module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

module String = 
    let split c x = 
        (x:string).Split((c:string),System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
let (|Split|) = String.split

module Int64 = 
    let tryParse (x:string) = 
        match System.Int64.TryParse(x) with
        | true, v -> Some (v)
        | _ -> None

let (|Int64|_|) = Int64.tryParse

module Tuple = 
    let two x y = (x, y)

let readAllLines path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllLines

let flip f x y = f y x

type Label = 
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | T
    | J
    | Q
    | K
    | A
module Label =
    let parse = 
        function
        | 'A' -> A
        | 'K' -> K 
        | 'Q' -> Q 
        | 'J' -> J 
        | 'T' -> T
        | '9' -> Nine 
        | '8' -> Eight 
        | '7' -> Seven 
        | '6' -> Six
        | '5' -> Five 
        | '4' -> Four 
        | '3' -> Three 
        | '2' -> Two
        | other -> failwithf "bad label '%c'" other
type HandType = 
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

let strength (hand:Label[]) = 
    let stats = 
        hand
        |> Seq.groupBy id
        |> Seq.map (snd >> Seq.length)
        |> Seq.sort
        |> Seq.toList

    match stats with
    | [5] -> FiveOfAKind
    | [1;4] -> FourOfAKind
    | [2;3] -> FullHouse
    | [1;1;3] -> ThreeOfAKind
    | [1;2;2] -> TwoPair
    | [1;1;1;2] -> OnePair
    | [1;1;1;1;1] -> HighCard
    | other -> failwithf "unhandled hand strength %A" other

[|A;A;A;A;A;|] |> strength |> Test.assertEq "FiveOfAKind" FiveOfAKind
[|A;A;A;A;Two;|] |> strength |> Test.assertEq "FourOfAKind" FourOfAKind
[|A;A;A;Two;Two;|] |> strength |> Test.assertEq "FullHouse" FullHouse
[|A;A;A;Two;Three;|] |> strength |> Test.assertEq "ThreeOfAKind" ThreeOfAKind
[|A;A;Two;Two;Three;|] |> strength |> Test.assertEq "TwoPair" TwoPair
[|A;A;Three;Two;Four;|] |> strength |> Test.assertEq "OnePair" OnePair
[|A;Five;Four;Three;Two;|] |> strength |> Test.assertEq "HighCard" HighCard

let part1 = 
    readAllLines
    >> Seq.map (
        function 
        | Split " " [hand; Int64 bid] -> 
            let hand = hand |> Seq.map Label.parse |> Seq.toArray
            let strength = strength hand
            (strength, (hand, bid))
        | other -> failwithf "bad line: '%s'" other
    )
    >> Seq.groupBy fst
    >> Seq.map (fun (k,s) -> k, s |> Seq.map snd |> Seq.toList)
    >> Map.ofSeq
    >> fun m ->
        m 
        |> Map.fold (fun (rank, scores) handType hands -> 
            hands
            |> Seq.sortBy fst
            |> Seq.fold (fun (rank, scores) (hand, bid) -> 
                (rank + 1L, scores + bid * rank)
            ) (rank, scores)
        ) (1L, 0L)
    >> snd
"example.txt" |> part1 |> Test.assertEq "part1 example" 6440L
"input.txt" |> part1 |> Test.assertEq "part1" 251545216L

