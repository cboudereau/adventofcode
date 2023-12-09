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

let lcm numbers=
    let rec gcd(n1:int64) (n2:int64) = 
        if n2 = 0 then n1
        else n1 % n2 |> gcd n2
    numbers |> List.reduce (fun s v -> s * v / gcd s v)

let part2 = 
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
                if (point:string)[2] = 'Z' then step
                else 
                    let d = nextDirection ()
                    let t = map[point]
                    let point = t |> flip choose d

                    // printfn "d=%c; t=%A; point=%s" d t point
                    traverse (step + 1L) point
            let ghosts = Map.keys map |> Seq.filter(fun x -> x[2] = 'A') |> Seq.toList
            ghosts |> List.map (traverse 0)
        | other -> failwithf "parsing failed '%s'" other
"part2.example.txt" |> part2 |> lcm |> Test.assertEq "part2 example" 6L
"input.txt" |> part2 |> lcm |> Test.assertEq "part2" 14321394058031L