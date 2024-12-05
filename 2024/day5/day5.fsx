let readFile filePath = System.IO.File.ReadAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath))

let (|Int64|_|) x = 
    match System.Int64.TryParse(x:string) with
    | true, v -> Some v
    | _ -> None

let parse (x:string) = 
    match x.Split("\n\n") with
    | [|part1;part2|] -> 
        part1.Split("\n") |> Array.map (fun x -> x.Split('|') |> function [|Int64 x; Int64 y|] -> (x,y) | other -> failwithf "unexpected page order '%A'" other) |> set, 
        part2.Split("\n") |> Array.map (fun x -> x.Split(',') |> Array.map System.Int64.Parse)
    | other -> failwithf "unexpected input '%A'" other

let part1 (pageOrderingRules, updates) = 
    let rec updateAll pageOrderingRules updates =
            let rec isValid = 
                function
                | [] -> true
                | h::t -> 
                    let r = t |> List.forall (fun x -> pageOrderingRules |> Set.contains (h,x))
                    if not r then false
                    else isValid t
            updates |> Array.filter (Array.toList >> isValid)
    let getMiddle a = 
        let l = a |> Array.length
        if l % 2 = 0 then failwithf "unexpected even length '%i'" l
        else a[l/2]
    updates 
    |> updateAll pageOrderingRules
    |> Array.map getMiddle
    |> Array.sum

"sample.txt" |> readFile |> parse |> part1 = 143
"input.txt" |> readFile |> parse |> part1  = 4814

let part2 (pageOrderingRules, updates) = 
    let rec isValid = 
        function
        | [] -> true
        | h::t -> 
            let r = t |> List.forall (fun x -> pageOrderingRules |> Set.contains (h,x))
            if not r then false
            else isValid t

    let rec updateAll updates = updates |> Array.filter (Array.toList >> (isValid >> not))

    let fix updates = 
        let rec isValid x update = 
            match update with
            | [] -> true
            | h::t -> 
                if h = x || pageOrderingRules |> Set.contains (x,h) then isValid x t
                else false
        let rec findHead update =
            function
            | [] -> failwith "findFirst: unexpected empty list"
            | h::t -> 
                if isValid h update then h
                else findHead update t
        let fix = 
            let rec fix current =
                function
                | [] -> current |> List.rev 
                | update -> 
                    let head = findHead update update
                    let update = update |> List.filter ((<>) head)
                    fix (head::current) update
            fix []

        updates |> Array.toList |> fix

    let getMiddle a = 
        let l = a |> List.length
        if l % 2 = 0 then failwithf "unexpected even length '%i'" l
        else a[l/2]
    updates 
    |> updateAll
    |> Array.map fix
    |> Array.map getMiddle
    |> Array.sum

"sample.txt" |> readFile |> parse |> part2 = 123
"input.txt" |> readFile |> parse |> part2 = 5448