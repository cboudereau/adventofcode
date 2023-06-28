module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual
module Array2D =
    let tryGet y x a = 
        let l1 = Array2D.length1 a
        let l2 = Array2D.length2 a

        if y < 0 || y >= l1 || x < 0 || x >= l2 then None
        else Some (Array2D.get a y x)

    let tryFind v a = 
        let l1 = Array2D.length1 a
        let l2 = Array2D.length2 a

        let mutable result = None
        for y = 0 to l1 - 1 do
            for x =0 to l2 - 1 do
                let c = Array2D.get a y x
                if c = v then 
                    result <- Some (x,y)

        result


let readFile filePath = 
    let parsed = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllLines |> Array.map Seq.toArray
    let l1 = parsed |> Array.length
    let l2 = parsed |> Array.map Array.length |> Array.max

    let m = Array2D.zeroCreate l1 l2

    parsed
    |> Array.iteri(fun y a ->
        a |> Array.iteri(fun x c ->
                Array2D.set m y x c
            )
    )
    m

let minChar = 'a'
let maxChar = 'z'


let v x = if x='E' then 123 elif x='S' then 97 else int x

let neighbors visited (x,y) height a = 
    [ 
        x, y - 1
        x, y + 1
        x - 1, y
        x + 1, y
    ]
    |> List.choose (fun (x, y) -> Array2D.tryGet y x a |> Option.map (fun v -> ((x,y),v)))
    |> List.filter (snd >> fun x -> 
        // printfn "%c/%c / %i - %i" x height v (int height) 
        v x - v height <= 1)
    |> List.filter (fst >> fun x -> visited |> Set.contains x |> not)

"day12.example.txt" |> readFile |> neighbors Set.empty (0,0) 'a'

"day12.txt" 
|> readFile
|> fun input ->
    let startPos = Array2D.tryFind 'S' input |> Option.get

    let rec step visited (queue:System.Collections.Generic.Queue<_>) a =
        match queue.TryDequeue() with
        | false, _ -> []
        | true, (path: ((int*int)*char) list) -> 
            let head = path |> List.head
            let (x,y) = fst head
            let height = snd head

            // Due to advent of code pruning 'E' to 'z'
            if height = 'z' || height = 'E' then 
                path |> List.rev
            else
                let neighbors = neighbors visited (x, y) height a 
                let visited = 
                    neighbors 
                    |> List.fold (fun s x -> 
                        queue.Enqueue(x :: path)
                        s |> Set.add (fst x) 
                    ) visited

                step visited queue input                

    step (Set.singleton startPos) (System.Collections.Generic.Queue([[startPos, 'S']])) input
    |> List.length 
    |> fun x -> x - 2
|> Test.assertEq "part1" 420