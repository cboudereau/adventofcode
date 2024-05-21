let x = 123us
let y = 456us

let d = x &&& y //AND
d = 72us
let e = x ||| y //OR
e = 507us
let f = x <<< 2 //LSHIFT
f = 492us
let g = y >>> 2 //RSHIFT
g = 114us
let h = ~~~x    //NOT
h = 65412us
let i = ~~~y    //NOT
i = 65079us

type Expression = 
    | AND of (string * string)
    | OR of (string * string)
    | LSHIFT of string
    | RSHIFT of string
    | NOT of string
    | VAR of string

type Value = 
    | Constant of uint16
    | Expression of Expression

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readAll (filePath:string) = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllText

let instructions = readAll "day7.txt"

let (|UInt16|_|) x =
    match System.UInt16.TryParse(x:string) with
    | true, v -> Some v
    | _ -> None

let (|Operation|_|) = function
    | Regex """(.*)\sAND\s(.*)""" [x; y] -> Some (AND(x, y))
    | Regex """(.*)\sOR\s(.*)""" [x; y] -> Some (OR(x, y))
    | Regex """LSHIFT\s(.*)""" [x] -> Some (LSHIFT x)
    | Regex """RSHIFT\s(.*)""" [x] -> Some (RSHIFT x)
    | Regex """NOT\s(.*)""" [x] -> Some (NOT x)
    | Regex """([^\s]+)""" [variable] -> Some (VAR variable)
    | _ -> None

let (|Value|) = function
    | Operation i -> Expression i
    | UInt16 constant -> Constant constant
    | other -> failwithf "unexpected value '%s'" other

let dependencies = 
    (function
    | OR (x, y)
    | AND (x, y) -> [x;y]
    | LSHIFT x
    | RSHIFT x
    | VAR x
    | NOT x -> [x])
    >> List.filter (function UInt16 _ -> false | _ -> true)

let assignments = 
    instructions.Split(System.Environment.NewLine)
    |> Array.map (
        function
        | Regex """(.*)\s->\s(.*)""" [Value value; target] -> target, value
        | other -> failwithf "unexpected instruction '%s'" other
    )
    |> Array.fold (fun s (target, value) -> 
        if s |> Map.containsKey target then failwithf "assignment for '%s' already exists" target
        else s |> Map.add target value
    ) Map.empty

assignments |> Map.find "aa"

let adjacencies = 
    assignments 
    |> Map.map (fun _ expression -> 
        match expression with
        | Expression exp -> dependencies exp
        | Constant _ -> []
    )

let initIndegree = adjacencies |> Map.map (fun _ _ -> 0)

adjacencies
|> Map.fold(fun s _ deps -> 
    deps |> List.fold (fun s dep -> 
        s |> Map.change dep (function Some c -> Some (c + 1) | None -> failwithf "unexpected '%s' dep" dep)
    ) s
) initIndegree
|> Map.toList