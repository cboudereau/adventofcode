module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let lookAndSay s = 
    let rec lookAndSay result current l =
        match current, l with
        | None, [] -> result
        | Some c, [] -> c::result
        | None, h::t -> lookAndSay result (Some (h,1)) t
        | Some (h1,c), h2::t when h1 = h2 -> lookAndSay result (Some (h1, c + 1)) t
        | Some (h1,c), h2::t -> lookAndSay ((h1,c)::result) (Some (h2,1)) t
    (s:string) 
    |> Seq.toList 
    |> lookAndSay [] None
    |> List.rev
    |> List.fold (fun s (c, t) -> (s:System.Text.StringBuilder).Append(sprintf "%i%c" t c)) (System.Text.StringBuilder())
    |> fun x -> x.ToString()

let input = "1113122113"

input |> lookAndSay |> Test.assertEq "example" "311311222113"

[1..40]
|> Seq.fold (fun s _ -> lookAndSay s) input
|> fun x -> x.Length
|> Test.assertEq "part1" 360154

[1..50]
|> Seq.fold (fun s _ -> lookAndSay s) input
|> fun x -> x.Length
|> Test.assertEq "part1" 5103798