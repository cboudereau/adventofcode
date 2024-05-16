let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int|_|) (input:string)  =
    match System.Int32.TryParse(input) with
    | true, v -> Some v
    | _ -> None

let (|Char|_|) (input:string)  =
    if input.Length > 1 then None
    else Some input[0]

let (|Direction|_|) (input:string) = 
    if input.Length > 1 then None
    else
        match input[0] with
        | 'R' -> Some (0, 1)
        | 'L' -> Some (0, -1)
        | 'U' -> Some (-1, 0)
        | 'D' -> Some (1, 0)
        | other -> failwithf "unexpected direction '%c'" other

let direction =
    function
    | 'R' -> ( 0,  1)
    | 'L' -> ( 0, -1)
    | 'U' -> (-1,  0)
    | 'D' -> ( 1,  0)
    | other -> failwithf "unexpected direction '%c'" other


let move directionChar (h, t) = 
    let add (xi, xj) (yi, yj) = (xi + yi, xj + yj)
    let isConnected (xi, xj) (yi, yj) = 
        let di = abs (xi - yi)
        let dj = abs (xj - yj)
        di < 2 && dj < 2
    let isDiagonal (xi, xj) (yi, yj) = abs (xi - yi) = 1 && abs (xj - yj) = 1
    let diagonal (hi, hj) (ti, tj) = hi - ti, hj - tj
    let coords = direction directionChar
    let newH = add coords h
    if isConnected newH t then 
        // printfn "connected"
        newH, t
    elif isDiagonal h t then 
        // printfn "diagonal"
        let d = diagonal h t
        newH, add d t
    else 
        // printfn "follow"
        newH, add coords t

let h = (4, 0)
let t = (4, 0)

(h, t) 

|> move 'R'
|> move 'R'
|> move 'R'
|> move 'R'

|> move 'U'
|> move 'U'
|> move 'U'
|> move 'U'

|> move 'L'
|> move 'L'
|> move 'L'

|> move 'D'

|> move 'R'
|> move 'R'
|> move 'R'
|> move 'R'

|> move 'D'

|> move 'L'
|> move 'L'
|> move 'L'
|> move 'L'
|> move 'L'

|> move 'R'
|> move 'R' = ((2,2),(2,1))

let parse = 
    function
    | Regex """([R|L|U|D])\s(\d+)""" [Char direction; Int count] -> (direction, count)
    | other -> failwithf "unexpected command '%s'" other 

parse "R 4" = ('R', 4)

let moveN line (visited, ht) =
    let (direction, count) = parse line
    [ 1..count ]
    |> List.fold (fun (tailCoords, ht) _ -> 
        let ht = move direction ht
        let tailCoords = Set.add (snd ht) tailCoords
        tailCoords, ht
    ) (visited, ht)

let moveM line ht =
    let (direction, count) = parse line
    [ 1..count ]
    |> List.fold (fun ht _ -> 
        move direction ht
    ) ht


let readAllLines filePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllLines

let flip f x y = f y x

"""day9-2024.example1.txt"""
|> readAllLines
|> Array.fold (flip moveN) (Set.empty, ((0,0),(0,0)))
|> fst
|> Seq.length = 13

"""day9.txt"""
|> readAllLines
|> Array.fold (flip moveN) (Set.empty, ((0,0),(0,0)))
|> fst
|> Seq.length = 5907