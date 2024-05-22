module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let readAll filePath = System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int32|_|) x = 
    match System.Int32.TryParse(x:string) with
    | true, v -> Some v
    | false, _ -> None

let parse =
    function
    | Regex """(.*)\sto\s(.*)\s=\s([\d]+)""" [city1;city2; Int32 distance] -> (city1, city2), distance
    | other -> failwithf "cannot parse input '%s'" other


let neighbors current distances = 
    distances 
    |> List.choose (fun ((x,y),d) -> if x = current then Some (y,d) elif y = current then Some (x,d) else None)

let routes locations distances =
    let rec routes results (q:System.Collections.Generic.Queue<_>) = 
        match q.TryDequeue() with
        | false, _ -> results
        | true, (unvisited, (path:(string * int) list)) -> 
            if unvisited |> Set.isEmpty then routes ((path |> List.rev)::results) q
            else 
                let head = path |> List.head
                let current = fst head
                let neighbors = neighbors current distances |> List.filter (fst >> fun x -> unvisited |> Set.contains x)
                neighbors
                |> List.map (fun (x,d) -> ((unvisited |> Set.remove x), (x, d)::path))
                |> List.iter q.Enqueue
                routes results q
    
    let q = System.Collections.Generic.Queue()
    locations 
    |> Seq.map (fun x -> (locations |> Set.remove x), [(x,0)]) 
    |> Seq.iter q.Enqueue
    
    routes [] q

let distances = "day9.txt" |> readAll |> Array.map parse

let locations = distances |> Array.fold (fun s ((x,y), _) -> s |> Set.add x |> Set.add y ) Set.empty

let possibleRoutes = routes locations (distances |> Array.toList) |> List.map (fun path -> path |> List.fold (fun s x -> s + snd x) 0)
possibleRoutes |> List.min |> Test.assertEq "part1" 207
possibleRoutes |> List.max |> Test.assertEq "part2" 804