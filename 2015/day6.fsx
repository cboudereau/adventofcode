module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

module Array2D = 
    let fold folder zero m = 
        let ymax = Array2D.length1 m
        let xmax = Array2D.length2 m
        let mutable result = zero
        for y = 0 to ymax - 1 do
            for x = 0 to xmax - 1 do
                result <- Array2D.get m y x |> folder result
        result

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int|_|) (input:string)  =
    match System.Int32.TryParse(input) with
    | true, v -> Some v
    | _ -> None

type Action = 
    | TurnOn
    | TurnOff
    | Toggle

let parse = function
    | Regex """([A-Za-z\s]+)\s(\d+),(\d+)\sthrough\s(\d+),(\d+)""" [action; Int x1; Int y1; Int x2; Int y2] -> 
        let action = 
            match action with
            | "turn on" -> TurnOn
            | "turn off" -> TurnOff
            | "toggle" -> Toggle
            | other -> failwithf "unexpected action '%s'" other
        Some (action, (x1,y1), (x2,y2))
    | other -> failwithf "unexpected input %s" other

"turn on 0,0 through 999,999" |> parse |> Test.assertEq "parsing 1" (Some (TurnOn, (0, 0), (999, 999)))
"toggle 0,0 through 999,0" |> parse |> Test.assertEq "parsing 2" (Some (Toggle, (0, 0), (999, 0)))
"turn off 499,499 through 500,500" |> parse |> Test.assertEq "parsing 3" (Some (TurnOff, (499, 499), (500, 500)))

let x = Array2D.zeroCreate<bool> 10 10
Array2D.set x 1 2 true
x
let part1 = 

    let part1 (m: bool array2d) action (xmin, ymin) (xmax, ymax) = 
        for y in [ymin .. ymax] do
            for x in [xmin .. xmax] do
                let v = 
                    match action with
                    | TurnOn -> true
                    | TurnOff -> false
                    | Toggle -> Array2D.get m y x |> not 
                Array2D.set m y x v
        m

    let m = Array2D.zeroCreate<bool> 1000 1000

    Array.map (parse >> Option.get)
    >> Array.fold (fun m (action, c1, c2) -> part1 m action c1 c2) m
    >> Array2D.fold (fun s -> function true -> s + 1 | false -> s) 0

let readFile file = System.IO.Path.Combine(__SOURCE_DIRECTORY__, file)|> System.IO.File.ReadAllLines

"day6.txt" |> readFile |> part1 |> Test.assertEq "part1" 377891
