module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Split|_|) sep input = 
    Some ((input:string).Split(sep:string))

let (|Int32|_|) x = 
    match System.Int32.TryParse(x:string) with
    | true, v -> Some v
    | false, _ -> None

module Command = 
    let parse (input:string) = 
        match input with 
        | "noop" -> (1, 0)
        | Regex """addx (-?\d+)""" [Int32 x] -> (2, x)
        | other -> failwithf "unexpected input %s" input

Command.parse "noop" |> Test.assertEq "noop command parsing" (1, 0)
Command.parse "addx 2" |> Test.assertEq "addx command parsing" (2, 2)
Command.parse "addx -2" |> Test.assertEq "addx command parsing" (2, -2)

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath) 

let strength newCycles newRegister cycles register = 
    let strength newCycles newRegister cycles register threshold = 
        if cycles < threshold && newCycles >= threshold then
            let register = if newCycles < threshold then newRegister else register
            register * threshold
        else 0
    
    [20;60;100;140;180;220] |> List.tryPick (fun t -> let s = strength newCycles newRegister cycles register t in if s > 0 then Some s else None) |> Option.defaultValue 0

strength 20 2 18 1 |> Test.assertEq "20 strength test" 20
strength 21 3 19 2 |> Test.assertEq "21 strength test" 40
strength 30 2 28 1 |> Test.assertEq "no strength test" 0

strength 60 3 58 1 |> Test.assertEq "60 strength test" 60
strength 61 3 59 2 |> Test.assertEq "60 strength test" 120
strength 62 3 60 2 |> Test.assertEq "no strength test" 0

strength 220 3 218 1 |> Test.assertEq "220 strength test" 220
strength 221 3 219 2 |> Test.assertEq "221 strength test" 440
strength 300 3 299 3 |> Test.assertEq "no strength test" 0

let part1 = 
    readFile
    >> Array.fold (fun (sum, (cycles, register)) input -> 

        let (c, x) = Command.parse input

        let newCycles = cycles + c
        let newRegister = register + x
        let strength = strength newCycles newRegister cycles register
        if strength > 0 then printfn "strength: %i cycles: %i newCycles: %i register: %i newRegister: %i" strength cycles newCycles register newRegister
        let newSum = sum + strength
        (newSum, (newCycles, newRegister))
    ) (0,(0,1))
    >> fst

"/day10.example.txt" |> part1 |> Test.assertEq "part 1 example" 13140 
"/day10.txt" |> part1 |> Test.assertEq "part 1" 14720 