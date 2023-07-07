module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let keypad = Array2D.init 3 3 (fun i j -> 3 * i + j + 1 + 48 |> char)

let strangeKeypad = Array2D.init 5 5 (fun _ _ -> 'X')
Array2D.set strangeKeypad 0 2 '1'

Array2D.set strangeKeypad 1 1 '2'
Array2D.set strangeKeypad 1 2 '3'
Array2D.set strangeKeypad 1 3 '4'

Array2D.set strangeKeypad 2 0 '5'
Array2D.set strangeKeypad 2 1 '6'
Array2D.set strangeKeypad 2 2 '7'
Array2D.set strangeKeypad 2 3 '8'
Array2D.set strangeKeypad 2 4 '9'

Array2D.set strangeKeypad 3 1 'A'
Array2D.set strangeKeypad 3 2 'B'
Array2D.set strangeKeypad 3 3 'C'

Array2D.set strangeKeypad 4 2 'D'
strangeKeypad

module Array2D = 
    let tryFind x m = 
        let l1 = Array2D.length1 m
        let l2 = Array2D.length2 m
        let mutable result = None

        for i = 0 to l1 - 1 do
            for j = 0 to l2 - 1 do
                if Array2D.get m i j = x then result <- Some (i, j)

        result

let readFile filePath = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllLines

let part1 keypad x = 
    let start = keypad |> Array2D.tryFind '5' |> Option.get

    let check i j = Array2D.get keypad i j <> 'X'

    let l1 = Array2D.length1 keypad
    let l2 = Array2D.length2 keypad

    x
    |> Array.fold (fun (pos, r) l -> 
        let (i, j) = 
            l 
            |> Seq.fold (fun (i, j) c -> 
                let (ni, nj) = 
                    match c with
                    | 'U' -> (i - 1 |> max 0, j)
                    | 'L' -> (i, j - 1 |> max 0)
                    | 'R' -> (i, j + 1 |> min (l2-1))
                    | 'D' -> (i + 1 |> min (l1-1), j)
                    | other -> failwithf "unexpected '%c'" other

                if check ni nj then (ni,nj) else (i, j)
            ) pos
        let v = Array2D.get keypad i j
        (i,j), v::r
    ) (start, [])
    |> snd
    |> List.rev
    |> List.map (sprintf "%c")
    |> String.concat ""

"day2.example.txt"
|> readFile
|> part1 keypad
|> Test.assertEq "example, part 1" "1985"

"day2.txt"
|> readFile
|> part1 keypad 
|> Test.assertEq "part 1" "73597"

"day2.example.txt"
|> readFile
|> part1 strangeKeypad 
|> Test.assertEq "example, part 2" "5DB3"

"day2.txt"
|> readFile
|> part1 strangeKeypad 
|> Test.assertEq "part 2" "A47DA"