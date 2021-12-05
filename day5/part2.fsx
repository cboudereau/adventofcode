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

        let range (l, r) = 
            let diag (x1, y1) (x2, y2) = 
                if (y1 - y2 |> abs) <> (x1 - x2 |> abs) then 
                    // printfn "not a square"
                    []
                else 
                    let (l, r) = if x1 < x2 then ((x1, y1), (x2, y2)) else ((x2, y2), (x1, y1))

                    List.unfold (fun ((x1, y1), (x2, y2)) -> 
                        if x1 >= x2 && y1 >= y2 then
                            // printfn "none %A" (x1, y2)
                            None
                        else 
                            let nx = x1 + 1
                            let ny = if y1 > y2 then y1 - 1 else y1 + 1
                            // printfn "ok %A" (nx, ny)
                            Some ((nx, ny), ((nx, ny), (x2, y2)))
                    ) (l, r)
                    |> fun x -> l::x
            
            // diag (1,1) (3,3)
            // diag (9,7) (7,9)

            // diag (1,4) (4,1)
            // let m = Array2D.zeroCreate 6 6
            // Array2D.set m 4 1 1
            // Array2D.set m 1 4 1
            // m
            
            // diag (1,1) (4,4)
            // let n = Array2D.zeroCreate 6 6
            // Array2D.set n 1 1 1
            // Array2D.set n 4 4 1
            // m, n

            let horv (x1,y1) (x2,y2)  = 
                if x1 = x2 then 
                    let miny = min y1 y2
                    let maxy = max y1 y2
                    [miny..maxy] |> List.map (fun y -> x1, y)

                elif y1=y2 then
                    let minx = min x1 x2
                    let maxx = max x1 x2
                    [minx..maxx] |> List.map (fun x -> x, y1)

                else []

            horv l r
            |> List.append (diag l r)
            

        match x.Split("->", System.StringSplitOptions.RemoveEmptyEntries) with
        | [|l; r|] -> 
            coord l 
            |> Option.bind (fun l -> 
                coord r 
                |> Option.map (fun r -> l, r)) 
                |> Option.map range |> Option.defaultValue []
        | _ -> [] 
    // line "1,1 -> 3,3"
    // line "9,7 -> 7,9"
    
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
|> overlap = 12

__SOURCE_DIRECTORY__ + "/part2.txt"
|> System.IO.File.ReadAllText
|> parse 
|> matrix
|> overlap = 19939


let m = Array2D.zeroCreate 9 9

Array2D.set m 0 0 1
Array2D.set m 8 8 9

Array2D.set m 0 8 -5
Array2D.set m 8 0 5

Array2D.set m 0 1 -1
Array2D.set m 1 0 2

m