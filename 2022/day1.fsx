module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

module Int32 = 
    let tryParse x = 
        match System.Int32.TryParse(x:string) with
        | true, v -> Some v
        | false, _ -> None

let (|Int32|_|) = Int32.tryParse

let parse input =
    input
    |> Array.fold (fun s x ->
        match s, x with
        | [], Int32 x -> [x]
        | h::t, Int32 x -> (h + x) :: t
        | s, _ -> 0::s
    ) List.empty    

let day11 (input:string[]) = 
    input
    |> Array.fold (fun s x ->
        match s, x with
        | [], Int32 x -> [x]
        | h::t, Int32 x -> (h + x) :: t
        | s, _ -> 0::s
    ) List.empty
    |> List.max

let input = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000""" |> fun x -> x.Split('\n')

input |> day11 |> Test.assertEq "day11 input" 24000
 

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath)

let fullInput = "/day1-1.txt" |> readFile

fullInput |> day11 |> Test.assertEq "day11 full input" 68292

let day12 = parse >> List.sortDescending >> List.truncate 3 >> List.sum

day12 input |> Test.assertEq "day12 input" 45000

"/day1-2.txt" |> readFile |> day12 |> Test.assertEq "day22 full input" 203203