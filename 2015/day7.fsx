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

type Instruction = 
    | AND of (string * string)
    | OR of (string * string)
    | LSHIFT of string
    | RSHIFT of string
    | NOT of string

type Value = 
    | Constant of uint16
    | Variable of string
    | Instruction of Instruction

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

let (|Instruction|_|) = function
    | Regex """(.*)\sAND\s(.*)""" [x; y] -> Some (AND(x, y))
    | Regex """(.*)\sOR\s(.*)""" [x; y] -> Some (OR(x, y))
    | Regex """LSHIFT\s(.*)""" [x] -> Some (LSHIFT x)
    | Regex """RSHIFT\s(.*)""" [x] -> Some (RSHIFT x)
    | Regex """NOT\s(.*)""" [x] -> Some (NOT x)
    | _ -> None

let (|Value|) = function
    | Instruction i -> Instruction i
    | UInt16 constant -> Constant constant
    | Regex """([^\s]+)""" [variable] -> Variable variable
    | other -> failwithf "unexpected value '%s'" other

let dependencies = function
    | OR (x, y)
    | AND (x, y) -> [x;y]
    | LSHIFT x
    | RSHIFT x
    | NOT x -> [x]

let assignments = 
    instructions.Split(System.Environment.NewLine)
    |> Array.map (
        function
        | Regex """(.*)\s->\s(.*)""" [Value value; target] -> target, value
        | other -> failwithf "unexpected instruction '%s'" other
    )