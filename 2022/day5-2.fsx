let split (delimiter:string) (x:string) =
    let pos = x.IndexOf(delimiter)
    if pos = -1 then failwith "delimiter not found"
    else x.Substring(0, pos), x.Substring(pos + delimiter.Length)

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int32|_|) x = 
    match System.Int32.TryParse(x:string) with
    | true, v -> Some v
    | false, _ -> None

let readFile filePath = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + filePath)

// let input = readFile "/day5.example.txt"
let input = readFile "/day5.txt"


let (stacksInput, opsInput) = input |> split "\r\n\r\n"

let lines = stacksInput |> fun x -> x.Split(System.Environment.NewLine) |> Array.rev |> Array.toList

let size = lines |> List.head |> fun x -> 1 + x.Length / 4

let stacks = 
    lines 
    |> List.tail 
    |> List.fold (fun (s: System.Collections.Generic.Stack<char> []) line -> 
        let rec add i (line:string) = 
            printfn "%s" line
            if line.Length <> 0 then 
                let c = line.[1]
                if c <> ' ' then s.[i].Push(c)
                if line.Length > 4 then line.Substring 4 |> add (i + 1)
        add 0 line
        s
    ) (Array.init size (fun _ -> System.Collections.Generic.Stack<char>()))

stacks |> Array.map Seq.toList = [|['N'; 'Z']; ['D'; 'C'; 'M']; ['P']|]

let ops = opsInput.Split(System.Environment.NewLine) |> Array.map ( 
    function
    | Regex """move (\d+) from (\d+) to (\d+)""" [ Int32 m; Int32 from; Int32 tO ] -> (m, from, tO)
    | other -> failwithf "unexpected '%s' value" other)


ops
|> Array.iter(fun (n, src, dst) -> 
        let l = [for _ = 1 to n do yield stacks.[src-1].Pop()] |> List.rev
        l |> List.iter (fun x -> stacks.[dst-1].Push(x)) 
    )

stacks 
|> Array.map (fun x -> x.Pop())
|> System.String = "TDGJQTZSL"