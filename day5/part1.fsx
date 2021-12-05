module Parsing = 
    let tryInt x = 
        match System.Int32.TryParse(x:string) with
        | true, v -> Some v
        | _ -> None

    let (|Int|_|) = tryInt

let parse (input:string) = 
    let lines = input.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    let line (x:string) = 
        let coord (x:string) : Option<int * int> = 
            match x.Split(',', System.StringSplitOptions.RemoveEmptyEntries) with
            | [|Parsing.Int x; Parsing.Int y|] -> Some (x, y)
            | _ -> None

        let range ((x1,y1), (x2,y2)): (int * int) list = 
            if x1 = x2 then 
                let miny = min y1 y2
                let maxy = max y1 y2
                [miny..maxy] |> List.map (fun y -> x1, y)

            elif y1=y2 then
                let minx = min x1 x2
                let maxx = max x1 x2
                [minx..maxx] |> List.map (fun x -> x, y1)

            else []

        match x.Split("->", System.StringSplitOptions.RemoveEmptyEntries) with
        | [|l; r|] -> 
            coord l 
            |> Option.bind (fun l -> 
                coord r 
                |> Option.map (fun r -> l, r)) 
                |> Option.map range |> Option.defaultValue []
        | _ -> [] 
    // line "0,9 -> 5,9"
    // line "8,0 -> 0,8"
    // line "9,4 -> 3,4"
    // line "2,2 -> 2,1"
    // line "7,0 -> 7,4"
    // line "6,4 -> 2,0"
    // line "0,9 -> 2,9"
    // line "3,4 -> 1,4"
    // line "0,0 -> 8,8"
    // line "5,5 -> 8,2"
    
    lines |> Array.toList |> List.collect line

let matrix l = 
    let l1 = 1 + (l |> List.map fst |> List.max)
    let l2 = 1 + (l |> List.map snd |> List.max)
    printfn "l1 %A l2 %A" l1 l2
    let m = Array2D.zeroCreate l1 l2

    l 
    |> List.iter(fun (x, y) -> 
        printfn "(%A, %A)" x y
        let v = Array2D.get m y x
        v + 1 |> Array2D.set m y x)

    m

let overlap m = 
    let l1 = Array2D.length1 m
    let l2 = Array2D.length2 m

    let range x = [0..x-1]
    let l1r = range l1
    let l2r = range l2

    let r = l1r |> List.collect (fun x -> l2r |> List.map (fun y -> Array2D.get m x y)) 

    r |> List.filter ((<) 1) |> List.length

let horv l = l |> List.filter (fun (x, y) -> x = y)

let input = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

input 
|> parse 
|> matrix
|> overlap = 5

__SOURCE_DIRECTORY__ + "/part1.txt"
|> System.IO.File.ReadAllText
|> parse 
|> matrix
|> overlap = 7318


let m = Array2D.zeroCreate 9 9

Array2D.set m 0 0 1
Array2D.set m 8 8 9

Array2D.set m 0 8 -5
Array2D.set m 8 0 5

Array2D.set m 0 1 -1
Array2D.set m 1 0 2

m