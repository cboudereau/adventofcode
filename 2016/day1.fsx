module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let readFile filePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllText

let right = 
    function
    |  (0, 1) -> (1, 0)
    |  (1, 0) -> (0, -1)
    | (0, -1) -> (-1, 0)
    | (-1, 0) -> (0, 1)
    | other -> failwithf "unexpected %A" other

let left = 
    function
    |  (0, 1) -> (-1, 0)
    | (-1, 0) -> (0, -1)
    | (0, -1) -> (1, 0)
    |  (1, 0) -> (0, 1) 
    | other -> failwithf "unexpected %A" other

let north = (0, 1)

let add (x1,y1) (x2,y2) = (x1+x2), (y1+y2)

let times n (x, y) = (x*n, y*n)

let split (x:string) = x.Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim())

let part1 x = 
    x
    |> split
    |> Array.fold (fun (pos, s) x -> 
        let direction = 
            match (x:string)[0] with
            | 'L' -> left
            | 'R' -> right
            | other -> failwithf "unexpected direction '%c'" other

        let step = System.Int32.Parse(x[1..])

        let pos = direction pos
        let c = times step pos
        let s = add s c
        printfn "%A %A %A" step pos s
        (pos, s)
    ) (north, (0, 0))
    |> snd
    |> fun (x, y) -> abs x + abs y

"R2, L3" |> part1 |> Test.assertEq "example" 5
"R2, R2, R2" |> part1 |> Test.assertEq "example 2" 2
"R2, R2, R2, R2" |> part1 |> Test.assertEq "example 0" 0
"L2, L2, L2" |> part1 |> Test.assertEq "example 2" 2
"L2, L2, L2, L2" |> part1 |> Test.assertEq "example 0" 0
"R5, L5, R5, R3" |> part1 |> Test.assertEq "example 3" 12

"./day1.txt" |> readFile |> part1 |> Test.assertEq "part1" 146

let part2 = 
    let rec part2 visited pos coords = 
        function
        | [] -> failwith "unexpected"
        | x::t ->
            let direction = 
                match (x:string)[0] with
                | 'L' -> left
                | 'R' -> right
                | other -> failwithf "unexpected direction '%c'" other

            let step = System.Int32.Parse(x[1..])

            let pos = direction pos

            let rec walk visited step pos coords = 
                match step with
                | 0 -> false, (coords, visited)
                | step ->
                    let coords = add coords pos
                    if Set.exists ((=) coords) visited then true, (coords, visited)
                    else walk (Set.add coords visited) (step - 1) pos coords
            let (found, (coords, visited)) = walk visited step pos coords
            if found then coords
            else part2 (Set.add coords visited) pos coords t
    
    part2 Set.empty north (0,0) >> fun (x, y) -> abs x + abs y

"R8, R4, R4, R8" |> split |> Array.toList |> part2 |> Test.assertEq "part2" 4

"./day1.txt" |> readFile |> split |> Array.toList |> part2 |> Test.assertEq "part2" 131