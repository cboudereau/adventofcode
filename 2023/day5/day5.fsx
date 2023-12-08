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

let flip f x y = f y x

let tryMap (dest:int64, source, range) x =
    if x >= source && x <= source + range then 
        let delta = x - source
        Some (dest + delta)
    else None

79L |> tryMap (50L, 98L, 2L)
79L |> tryMap (52L, 50L, 48L)
50L |> tryMap (52L, 50L, 48L)

let tryMapAll mappings = 
    fun x ->
        mappings
        |> List.choose (flip tryMap x)
        |> function [] -> [x] | found -> found

let readAllText path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllText

let (|List|) f l = l |> List.map f

let (|Mapping|) (m:string) = 
    m
    |> String.split "\r\n"
    |> List.tail
    |> List.map (
        function
        | Split " " [Int64 dest; Int64 source;Int64 range] -> (dest, source, range)
        | other -> failwithf "unexpected mapping '%s'" other
    )

let part1 = 
    readAllText
    >> function
        | Split "\r\n\r\n" (Split ":" [_ ; Split " " (List System.Int64.Parse seeds)]::[Mapping soilMap; Mapping fertilizerMap; Mapping waterMap; Mapping lightMap; Mapping temperatureMap; Mapping humidityMap; Mapping locationMap]) -> 
            let f x = 
                x
                |> tryMapAll soilMap
                |> List.collect(tryMapAll fertilizerMap)
                |> List.collect(tryMapAll waterMap)
                |> List.collect(tryMapAll lightMap)
                |> List.collect(tryMapAll temperatureMap)
                |> List.collect(tryMapAll humidityMap)
                |> List.collect(tryMapAll locationMap)
                |> List.min
            printfn "seeds: %A" seeds
            seeds 
            |> Seq.map f
            |> Seq.fold min System.Int64.MaxValue
        | other -> failwithf "unexpected input '%s'" other

"example.txt" |> part1 |> Test.assertEq "part1" 35L
"input.txt" |> part1 |> Test.assertEq "part1" 289863851L

module ClosedInterval = 
    let intersect (x1:int64,l1) (x2,l2) = 
        let xmax = max x1 x2
        let ymin = min (x1+l1) (x2+l2)
        if ymin >= xmax then Some (xmax, ymin - xmax)
        else None 

ClosedInterval.intersect (0,10) (0,10) |> Test.assertEq "identity intersect" (Some (0,10))
ClosedInterval.intersect (0,10) (10,10) |> Test.assertEq "identity intersect" (Some (10,0))
ClosedInterval.intersect (0,10) (2,5) |> Test.assertEq "identity intersect" (Some (2,5))
ClosedInterval.intersect (0,10) (-2,100) |> Test.assertEq "identity intersect" (Some (0,10))

let tryMapRange (dest, source, range) (x, length) =
    ClosedInterval.intersect (source, range) (x, length)
    |> Option.map (fun (x, length) -> 
        let offset = x - source
        (dest + offset), length
    )

tryMapRange (52, 50, 48) (79, 14) |> Test.assertEq "tryMapRange" (Some (81, 14))

let tryMapRangeAll mappings = 
    fun x ->
        mappings
        |> List.choose (flip tryMapRange x)
        |> function [] -> [x] | found -> found

let part2 = 
    readAllText
    >> function
        | Split "\r\n\r\n" (Split ":" [_ ; Split " " (List System.Int64.Parse seeds)]::[Mapping soilMap; Mapping fertilizerMap; Mapping waterMap; Mapping lightMap; Mapping temperatureMap; Mapping humidityMap; Mapping locationMap]) -> 
            let f x = 
                x
                |> tryMapRangeAll soilMap
                |> List.collect(tryMapRangeAll fertilizerMap)
                |> List.collect(tryMapRangeAll waterMap)
                |> List.collect(tryMapRangeAll lightMap)
                |> List.collect(tryMapRangeAll temperatureMap)
                |> List.collect(tryMapRangeAll humidityMap)
                |> List.collect(tryMapRangeAll locationMap)
                |> List.min
            printfn "seeds: %A" seeds
            seeds
            |> Seq.chunkBySize 2
            |> Seq.map (
                function
                | [|start; length|] -> (start, length) 
                | other -> failwithf "bad pairs %A" other
            )
            |> Seq.map f
            |> Seq.fold (flip (fst >> min)) System.Int64.MaxValue
        | other -> failwithf "unexpected input '%s'" other

"example.txt" |> part2 |> Test.assertEq "example part2" 60L
"input.txt" |> part2 |> Test.assertEq "part2" 60568880L