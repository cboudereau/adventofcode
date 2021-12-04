type Cell = 
    | Marked
    | UnMarked

type Hand = {
    Index: Map<int, int * int>
    Board: (int * Cell) [,]
}

type HandState =
    | Win of Hand
    | Loose of Hand

module Hand = 
    let zero l = 
        { Hand.Index = Map.empty
          Board= Array2D.zeroCreate l l }

    let add x y n (h:Hand) = 
        let h = { h with Index= h.Index |> Map.add n (x, y) }
        (n, UnMarked) |> Array2D.set h.Board x y
        h

    let mark n (h:Hand) = 
        match h.Index |> Map.tryFind n with
        | None -> Loose h
        | Some (x, y) ->
            (n, Marked) |> Array2D.set h.Board x y
            let rec asWon y = 
                match Array2D.get h.Board x y with
                | _, Marked -> 
                    let y = y + 1
                    if y = Array2D.length2 h.Board then Win h
                    else y |> asWon 
                | _ -> Loose h
            
            asWon 0

let h = 
    Hand.zero 5
    |> Hand.add 0 0 4
    |> Hand.add 0 1 3
    |> Hand.add 0 2 2
    |> Hand.add 0 3 1
    |> Hand.add 0 4 0
        
h |> Hand.mark 4

Array2D.get h.Board 0 1


type StartedGame = {
    Input: int list
    Numbers:int list
    Hands:Hand list
}

type Game = 
    | Started of StartedGame
    | Finished of (int * Hand) option

module Array2D = 
    let toSeq array =
        seq {
            for x in 0 .. Array2D.length1 array - 1 do
                for y in 0 .. Array2D.length2 array - 1 do
                    yield array.[x, y]
        }

module Parsing = 
    let tryInt x = 
        match System.Int32.TryParse(x:string) with
        | true, v -> Some v
        | _ -> None

let flip f x y = f y x

module Game = 
    let start input hands = { StartedGame.Input = input; Numbers = List.empty; Hands = hands }

    let parse (input: string) = 
        let input = input.Split(System.Environment.NewLine) |> Array.toList
        let header = List.head input
        let numbers = header.Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.choose Parsing.tryInt |> Array.toList
        let hands = 
            let rec hands hs l h (input: string list) = 
                match input with
                | [] -> h::hs
                | head::tail when System.String.IsNullOrEmpty head -> hands (h::hs) 0 (Hand.zero 5) tail 
                | head::tail -> 
                    let (_, hand) = 
                        head.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                        |> Array.fold (fun (y, s) x -> x |> Parsing.tryInt |> Option.map (fun x -> y + 1 , s |> Hand.add l y x) |> Option.defaultValue (y, s)) (0, h)
                    hands hs (l + 1) hand tail

            input |> List.tail |> List.tail |> hands [] 0 (Hand.zero 5)
        let game = start numbers (hands |> List.rev)
        game
    
    let rec run (game:StartedGame) = 
        match game.Input with
        | [] -> Finished None
        | input::ti -> 
            let rec play hands = 
                match hands with
                | [] -> Started game
                | hand::th -> 
                    match hand |> Hand.mark input with
                    | Win h -> Finished (Some (input, h))
                    | Loose _ -> play th
            match play game.Hands with
            | Finished w -> Finished w
            | Started g -> run { g with Input=ti }
    
    let score = function
        | Started _ -> 0
        | Finished None -> 0
        | Finished (Some (i,h)) -> 
            i * (h.Board |> Array2D.toSeq |> Seq.map (function (i, UnMarked) -> i | _ -> 0) |> Seq.fold (+) 0)

let input = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

input |> Game.parse |> Game.run |> Game.score = 4512

__SOURCE_DIRECTORY__ + "/part1.txt"
|> System.IO.File.ReadAllText
|> Game.parse |> Game.run |> Game.score = 32844 

