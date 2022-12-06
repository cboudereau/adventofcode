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

let day11 = 
    parse
    >> Array.map (fun (x, y) -> shapeScore x, shapeScore y)
    >> Array.map roundScore
    >> Array.sumBy snd

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath)

"""A Y
B X
C Z"""
|> fun x -> x.Split('\n')
|> day11 = 15

"/day2-1.txt" 
|> readFile 
|> day11 = 14531

// not 13223