module Tuple = 
    let fold f (x, y) = f x y

module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

let priority (x:char) = 
    let v = int x
    if v > 96 then v - 96
    else v - 38

['A' .. 'Z']
|> List.mapi (fun i x -> x, (i + 27, priority x))
|> List.iter (fun (x, t) -> t |> Tuple.fold (Test.assertEq (sprintf "A-Z Test for '%c'" x)))

['a' .. 'z']
|> List.mapi (fun i x -> x, (i + 1, priority x))
|> List.iter (fun (x, t) -> t |> Tuple.fold (Test.assertEq (sprintf "A-Z Test for '%c'" x)))

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath)

let day31 =
    Array.map (fun (x:string) -> 
        let half = x.Length / 2
        x.[..half-1], x.[half..]
    )
    >> Array.map (fun (x,y) -> Set.intersect (set x) (set y) |> Seq.exactlyOne)
    >> Array.map priority
    >> Array.sum 

"""vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""
|> fun x -> x.Split('\n')
|> day31 
|> Test.assertEq "day31 input" 157

"""/day3.txt"""
|> readFile
|> day31 
|> Test.assertEq "day31 full input" 7746
