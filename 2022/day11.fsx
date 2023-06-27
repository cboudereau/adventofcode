module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let (|Regex|_|) options pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern, options)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int64|_|) x = 
    match System.Int64.TryParse(x:string) with
    | true, v -> Some v
    | false, _ -> None

type MonkeyId = int

type Monkey = {
    MonkeyId: int
    Items: System.Collections.Generic.Queue<int64>
    Operation: int64 -> int64
    Test: int64 -> MonkeyId
    mutable TotalInspections: int64
}

let parse = 
    function
    | Regex System.Text.RegularExpressions.RegexOptions.Multiline """Monkey (\d+):\s*Starting items: ([\d,\s]+)\s*Operation: new = old ([\+\*]) ([^\s]+)\s*Test: divisible by (\d+)\s*If true: throw to monkey (\d+)\s*If false: throw to monkey (\d+)\s*""" [monkeyId; items; sign; v; divisible; monkeyTrue; monkeyFalse] -> 
        { Monkey.MonkeyId = int monkeyId
          TotalInspections = 0
          Items = items.Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map(fun x -> int64 x) |> System.Collections.Generic.Queue
          Operation = 
            let op = match sign with "*" -> (*) | "+" -> (+) | other -> failwithf "unexpected op '%s'" other
            match v with
            | Int64 v -> op v
            | "old" -> fun x -> op x x
            | other -> failwithf "unexpected value op '%s'" other
          Test = 
            match divisible, monkeyTrue, monkeyFalse with
            | Int64 d, Int64 mTrue, Int64 mFalse -> 
                fun x -> 
                    if x % d = 0 then 
                        // printfn "  Current worry (%i) level is divisible by %i." x d
                        int mTrue
                    else 
                        // printfn "  Current worry level (%i) is not divisible by %i." x d
                        int mFalse
            | other -> failwithf "unexpected divisible '%A'" other
        }
    | other -> failwithf "unexpected %s" other

parse """Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""

let readFile filePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllText

let split x = (x:string).Split("\r\n\r\n", System.StringSplitOptions.RemoveEmptyEntries)

let part1 input = 
    let turn n (monkeys:Monkey[]) : Monkey[] = 
        [1..n]
        |> List.fold(fun monkeys _ -> 
            monkeys
            |> Array.fold (fun monkeys m -> 
                let rec turn monkeys (m:Monkey) = 
                    match m.Items.TryDequeue() with
                    | true, item -> 
                        m.TotalInspections <- m.TotalInspections + 1L
                        printfn " Monkey inspects an item with a worry level of %i" item
                        let item = m.Operation item
                        let item = item / 3L
                        printfn "  Monkey gets bored with item. Worry level is divided by 3 to %i." item
                        let mId = m.Test item
                        let target = monkeys |> Array.find(fun x -> x.MonkeyId = mId)
                        printfn "  Item with worry level %i is thrown to monkey %i" item mId
                        target.Items.Enqueue(item)
                        turn monkeys m
                    | false, _ -> monkeys            
                printfn "Monkey %i:" m.MonkeyId
                turn monkeys m
            ) monkeys
        ) monkeys
    input
    |> split
    |> Array.map parse
    |> turn 20
    |> Array.sortByDescending (fun x -> x.TotalInspections)
    |> Array.take 2
    |> Array.map (fun x -> x.TotalInspections)
    |> Array.fold (*) 1L

"./day11.example.txt" |> readFile |> part1 |> Test.assertEq "example" 10605
"./day11.txt" |> readFile |> part1 |> Test.assertEq "example" 55944

let part2 n input = 
    let checks = [1;20;1000;2000;3000;4000;5000;6000;7000;8000;9000;10000]
    let turn n (monkeys:Monkey[]) : Monkey[] = 
        [1..n]
        |> List.fold(fun monkeys i -> 
            let monkeys = 
                monkeys
                |> Array.fold (fun monkeys m -> 
                    let rec turn monkeys (m:Monkey) = 
                        match m.Items.TryDequeue() with
                        | true, item -> 
                            m.TotalInspections <- m.TotalInspections + 1L
                            let item = m.Operation item
                            let item = item % (7L * 19L * 13L * 3L * 2L * 11L * 17L * 5L)
                            let mId  = m.Test item
                            let target = monkeys |> Array.find(fun x -> x.MonkeyId = mId)
                            target.Items.Enqueue(item)
                            turn monkeys m
                        | false, _ -> monkeys            
                    turn monkeys m
                ) monkeys
            if checks |> List.contains i then 
                printfn "== After round %i ==" i
                monkeys |> Array.iter(fun m -> printfn "Monkey %i inspected items %i times." m.MonkeyId m.TotalInspections)
                printfn ""
            monkeys
        ) monkeys
    input
    |> turn n
    |> Array.sortByDescending (fun x -> x.TotalInspections)
    |> Array.take 2
    |> Array.map (fun x -> x.TotalInspections)
    |> Array.fold (*) 1L

"./day11.example.txt" |> readFile |> split |> Array.map parse |> part2 10000 
|> Test.assertEq "part2 example" 2567194800L

"./day11.txt" |> readFile |> split |> Array.map parse |> part2 10000
|> Test.assertEq "part2" 15117269860L
