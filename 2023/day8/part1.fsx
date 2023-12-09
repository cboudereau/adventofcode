module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

module String = 
    let split c x = 
        (x:string).Split((c:string),System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
let (|Split|) = String.split

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module Seq = 
    let rec repeat s = 
        seq {
            yield! s
            yield! repeat s
        }

let readAllText path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, path) |> System.IO.File.ReadAllText

let choose (x,y) =
    function
    | 'L' -> x
    | 'R' -> y
    | other -> failwithf "unexpected direction '%c'" other

let flip f x y = f y x

let part1 = 
    readAllText
    >> function
        | Split "\r\n\r\n" [directions;Split "\r\n" map] -> 
            use directions = (Seq.repeat directions).GetEnumerator()
            let nextDirection () = 
                directions.MoveNext() |> ignore
                directions.Current

            let map = 
                map
                |> Seq.map (fun l -> 
                    let source = l.Substring(0,3)
                    let left = l.Substring(7, 3)
                    let right = l.Substring(12,3)
                    source, (left, right)
                )
                |> Map.ofSeq

            let rec traverse step point = 
                if point = "ZZZ" then step
                else 
                    let d = nextDirection ()
                    let t = map[point]
                    let point = t |> flip choose d

                    // printfn "d=%c; t=%A; point=%s" d t point
                    traverse (step + 1) point

            traverse 0 "AAA"        
        | other -> failwithf "parsing failed '%s'" other
"example.1.txt" |> part1 |> Test.assertEq "example 1 part1" 2
"example.2.txt" |> part1 |> Test.assertEq "example 2 part1" 6
"input.txt" |> part1 |> Test.assertEq "part1" 14681
