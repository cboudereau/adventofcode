// floodfill

// y -> index1 -> line
// x -> index2 -> column

let adjencies mny mnx mxy mxx (y, x) =
    [ 
        y, x-1
        // y-1, x-1
        y-1, x
        // y-1, x+1
        y, x+1
        // y+1, x+1
        y+1, x
        // y+1, x-1
    ]
    |> List.filter (fun (y, x)-> x >= mnx && y >= mny && x < mxx && y < mxy )
    |> List.distinct

// let y = 2
// let x = 0
// let a = Array2D.zeroCreate<int> 3 3
// Array2D.set a y x 1
// a
// let coords = adjencies 0 0 3 3 y x
// coords |> List.iter (fun (y, x) -> Array2D.set a y x -1)
// a

let parse (x:string) : int [,] = 
    let lines = x.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    let l1 = lines |> Array.length
    let l2 = lines |> Array.head |> fun x -> x.Length
    let a = Array2D.zeroCreate l1 l2
    lines
    |> Array.iteri (fun y xs -> xs |> Seq.iteri (fun x v -> Array2D.set a y x (System.Char.GetNumericValue v |> int)))
    a

let lowpoints (a: int [,]) =
    let my = a |> Array2D.length1
    let mx = a |> Array2D.length2
    [0..(my-1)]
    |> List.collect(fun y -> 
        [0..(mx-1)]
        |> List.choose (fun x -> 
            let coords = adjencies 0 0 my mx (y, x)
            let v = Array2D.get a y x 
            // printfn "(%i, %i) %i" y x v
            // printfn "(%i, %i) %A" my mx coords
            if coords |> List.forall (fun (y, x) -> v < Array2D.get a y x ) then
                Some ((y,x), v)
            else None
        )
    )
    |> List.map fst

let part2 (a: int [,]) = 
    let lowpoints = lowpoints a

    let my = a |> Array2D.length1
    let mx = a |> Array2D.length2

    let rec bassins visited r coords = 
        let rec bassin visited (r:(int*int) list) coords =
            match coords with
            | [] -> 
                // printfn "bassin %A, (%A)" r visited
                r, visited
            | coord::coords ->  
                let adjs = adjencies 0 0 my mx coord |> List.filter (fun c -> visited |> List.contains c |> not)
                let visited = coord::visited
                let r = coord::r
                
                let news = adjs |> List.filter (fun (y, x) -> Array2D.get a y x < 9)
                let visited = List.append visited news
                let r = List.append r news
                let coords = List.append coords news

                // printfn "(%i, %i) %A" y x news
                bassin visited r coords

        match coords with
        | [] -> r
        | c::ct -> 
            let (bassin, visited) = bassin visited [] [c]
            bassins visited (bassin::r) ct

    bassins [] [] lowpoints

let input = """2199943210
3987894921
9856789892
8767896789
9899965678"""

parse input |> part2 |> List.map (List.distinct) |> List.rev
parse input |> part2 |> List.map (List.distinct >> List.length) |> List.rev

let run = parse >> part2 >> List.map (List.distinct >> List.length) >> List.sortDescending >> List.take 3 >> List.fold (*) 1

run input = 1134

__SOURCE_DIRECTORY__ + "/part1.txt"
|> System.IO.File.ReadAllText
|> run = 1263735