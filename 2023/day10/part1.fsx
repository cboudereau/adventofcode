module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

module String = 
    let split c x = 
        (x:string).Split((c:string),System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
let (|Split|) = String.split

module Array2D = 
    let ofArray x = 
        let l1 = x |> Array.length
        let l2 = x |> Array.head |> Array.length

        let a = Array2D.zeroCreate l1 l2
        
        for i in 0..l1 - 1 do
            for j in 0..l2 - 1 do
                a[i,j] <- x[i][j]
        a

    let tryFindIndex f x = 
        let l1 = x |> Array2D.length1
        let l2 = x |> Array2D.length2

        let rec find f i j x = 
            if j = l2 then None
            elif i = l1 then find f 0 (j+1) x
            else
                if Array2D.get x i j |> f then Some (i,j)
                else find f (i+1) j x
            
        find f 0 0 x

let readAllLines path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllLines

module Coords = 
    let add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

let children coords a =
    let l1 = a |> Array2D.length1
    let l2 = a |> Array2D.length2
    
    [ ( 0,  1)
      ( 0, -1)
      ( 1,  0)
      (-1,  0) ]
    |> List.choose (fun offset -> 
        let i, j = Coords.add coords offset

        if i >= l1 || i < 0 || j < 0 || j >= l2 then None
        else
            match Array2D.get a i j, offset with
            | '|', (-1, 0 |  1, 0) 
            | '-', ( 0,-1 |  0, 1) 
            | 'L', ( 1, 0 |  0,-1)
            | 'J', ( 1, 0 |  0, 1)
            | '7', ( 0, 1 | -1, 0)
            | 'F', ( 0,-1 |  -1, 0)
                -> Some (i, j)
            | _ -> None
    ) 
    |> set


Set.difference Set.empty (set [1])
Set.difference (set [1]) (set [1])
Set.difference (set [1]) (Set.empty)

"example.txt"
"input.txt"
|> readAllLines
|> Array.map (fun x -> x.ToCharArray())
|> Array2D.ofArray
|> fun map ->
    let start = map |> Array2D.tryFindIndex ((=) 'S') |> Option.get

    let rec visit visited result map = 
        function
        | [] -> 
            // printfn "no coords"
            result
        | h::t -> 
            let coords = h |> List.head |> snd
            let children = children coords map
            let diffs = Set.difference children visited
            // printfn "[%A] result:%A visited:%A children: %A diffs: %A" coords result visited children diffs
            if diffs |> Set.isEmpty then 
                let result = h::result
                visit visited result map t
            else 
                let visited = Set.union visited diffs
                let l = diffs |> Set.toList |> List.map (fun (i,j) -> (Array2D.get map i j,(i,j))::h) |> List.append t
                visit visited result map l
    visit (Set.singleton start) [] map [['S',start]]
    |> List.map (fun x -> (x |> List.length) - 1)
    |> List.max