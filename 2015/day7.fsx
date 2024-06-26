module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let x = 123us
let y = 456us

let d = x &&& y //AND
Test.assertEq "AND" d 72us
let e = x ||| y //OR
Test.assertEq "OR" e 507us
let f = x <<< 2 //LSHIFT
Test.assertEq "LSHIFT" f 492us
let g = y >>> 2 //RSHIFT
Test.assertEq "RSHIFT" g 114us
let h = ~~~x    //NOT
Test.assertEq "NOT" h 65412us
let i = ~~~y    //NOT
Test.assertEq "NOT" i 65079us

type Expression = 
    | AND of (string * string)
    | OR of (string * string)
    | LSHIFT of (string * string)
    | RSHIFT of (string * string)
    | NOT of string
    | VAR of string

type Value = 
    | Constant of uint16
    | Expression of Expression

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|UInt16|_|) x =
    match System.UInt16.TryParse(x:string) with
    | true, v -> Some v
    | _ -> None

let (|Operation|_|) = function
    | Regex """(.*)\sAND\s(.*)""" [x; y] -> Some (AND(x, y))
    | Regex """(.*)\sOR\s(.*)""" [x; y] -> Some (OR(x, y))
    | Regex """(.*)\sLSHIFT\s(.*)""" [x;y] -> Some (LSHIFT (x,y))
    | Regex """(.*)\sRSHIFT\s(.*)""" [x;y] -> Some (RSHIFT (x,y))
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
    | AND (x, y) 
    | LSHIFT (x,y)
    | RSHIFT (x,y) -> [x;y]
    | VAR x
    | NOT x -> [x])
    >> List.filter (function UInt16 _ -> false | _ -> true)

let allDependencies = function
    | Constant _ -> []
    | Expression e -> dependencies e

let rec stack assignments current s = 
    match current |> List.collect (fun x -> assignments |> Map.find x |> allDependencies) with
    | [] -> s
    | deps -> 
        let deps = List.distinct deps
        List.append deps s |> List.distinct |> stack assignments deps

let resolve (assignments:Map<string, Value>) = 
    let resolveExpression = 
        let resolveValue (assignments:Map<string, Value>) x = 
            match x with
            | UInt16 x -> x
            | variable -> 
                match assignments |> Map.find variable with
                | Constant x -> x
                | _ -> failwithf "cannot resolve value to constant '%s'" variable
        function
        | AND (x, y) -> (resolveValue assignments x) &&& (resolveValue assignments y)
        | OR (x, y) -> (resolveValue assignments x) ||| (resolveValue assignments y)
        | LSHIFT (x,y) -> (resolveValue assignments x) <<< (resolveValue assignments y |> int)  
        | RSHIFT (x,y) -> (resolveValue assignments x) >>> (resolveValue assignments y |> int)
        | NOT x -> ~~~(resolveValue assignments x)
        | VAR x -> resolveValue assignments x
    function
    | Expression exp -> resolveExpression exp |> Constant
    | Constant x -> Constant x

let compute assignments variable = 
    let stack = stack assignments [variable] [variable]
    let register = 
        stack |> List.fold (fun (s:Map<string, Value>) x -> 
            let exp = s |> Map.find x
            let value = resolve s exp
            s |> Map.add x value
        ) assignments
    register |> Map.find variable

// Part 1
let readAll (filePath:string) = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllText

let instructions = readAll "day7.txt"

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

let a = compute assignments "a" 
Constant 46065us |> Test.assertEq "part1" a

// Part 2
Constant 14134us |> Test.assertEq "part2" (compute (assignments |> Map.add "b" a) "a")