module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

let shapeScore = function
    | 'A' | 'X' -> 1 // Rock
    | 'B' | 'Y' -> 2 // Paper
    | 'C' | 'Z' -> 3 // Scissors
    | other -> failwithf "unexpected '%c'" other

let parse (x:string[]) = x |> Array.map (fun x -> x.[0], x.[2])

let roundScore = 
    function
    | x, y when x = y -> x + 3, y + 3
    
    // Rock (1) defeats Scissors (3)
    | 1, 3 -> 1 + 6, 3
    | 3, 1 -> 3, 1 + 6

    // Scissors (3) defeats Paper (2)
    // Paper (2) defeats Rock (1)
    | x, y when x > y -> x + 6, y
    | x, y -> x, y + 6

let day21 = 
    parse
    >> Array.map (fun (x, y) -> shapeScore x, shapeScore y)
    >> Array.map roundScore
    >> Array.sumBy snd

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath)

"""A Y
B X
C Z"""
|> fun x -> x.Split('\n')
|> day21
|> Test.assertEq "day21 input" 15

"/day2-1.txt" 
|> readFile 
|> day21 
|> Test.assertEq "day21 full input" 14531

// 2

// X
let draw = id

let winMapping = 
    [ 'A', 'B'
      'B', 'C'
      'C', 'A' ]
// Y
let lose =
    let mapping = winMapping |> List.map (fun (x, y) -> y, x) 
    fun x -> mapping |> Map.ofList |> Map.find x 

// Z
let win = 
    let mapping = winMapping |> Map.ofList 
    fun x -> mapping |> Map.find x

let strategy (x, mode) = 
    x,
    match mode with
    | 'X' -> lose x
    | 'Y' -> draw x
    | 'Z' -> win x 
    | other -> failwithf "unexpected round '%c'" other

let day22 = parse >> Array.map strategy >> Array.map (fun (x, y) -> roundScore (shapeScore x, shapeScore y)) >> Array.sumBy snd 

"""A Y
B X
C Z"""
|> fun x -> x.Split('\n')
|> day22 
|> Test.assertEq "day22 input" 12

"""/day2-2.txt"""
|> readFile
|> day22 
|> Test.assertEq "day22 full input" 11258