let parentheses = '(', ')'
let squareBrakets = '[', ']'
let curlyBraces = '{', '}'
let lessAndMore = '<', '>'

let validPairs = 
    [
        parentheses
        squareBrakets
        curlyBraces
        lessAndMore
    ]

let findClosing = 
    let mapping = validPairs |> Map.ofList
    fun x -> mapping |> Map.tryFind x
findClosing '(' = Some ')'
findClosing ')' = None

let findOpening = 
    let mapping = validPairs |> List.map (fun (x,y)-> y,x) |> Map.ofList
    fun x -> mapping |> Map.tryFind x
findOpening ')' = Some '('
findOpening '(' = None

let isOpening = findClosing >> Option.isSome
isOpening '(' = true
isOpening ')' = false


let isClosing = findOpening >> Option.isSome
isClosing ')' = true
isClosing '(' = false

let isClosingChunk x y =
    findClosing x |> Option.map ((=) y) |> Option.defaultValue false

isClosingChunk '(' ')' = true
isClosingChunk '(' ']' = false
isClosingChunk '(' 'x' = false

let closingWithWrongChar line = 
    let rec parse pendings chars = 
        match pendings, chars with
        | _, [] -> None
        | [], c::_ when c |> isClosing -> 
            (c, sprintf "cannot start with a closing char %A (%A)" c chars)
            |> Some
        | pendings, c::cs when c |> isOpening -> parse (c::pendings) cs 
        | p::pendings, c::cs when c |> isClosingChunk p -> parse pendings cs
        | p::_, c::_ -> 
            let expected = p |> findClosing |> Option.get
            (c, sprintf "Expected %c, but found %c instead" expected c)
            |> Some
        | _, _ -> 
            failwithf "unexpected %A / %A" pendings chars
    (line:string) |> Seq.toList |> parse []

let exceptCorrupted x = closingWithWrongChar x |> function None -> Some x | Some _ -> None

"{([(<{}[<>[]}>{[]{[(<()>" |> closingWithWrongChar 

let score x = 
    x 
    |> Option.map (fst >> function ')' -> 3 | ']'->57 | '}' -> 1197 | '>' -> 25137 | _ -> 0)
    |> Option.defaultValue 0

let parse x = (x:string).Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList 

let part1 = 
    List.map (fun x -> 
        closingWithWrongChar x 
        |> Option.map (fun y-> x, y)) 
    >> List.map (
            Option.map (
                snd 
                >> fst 
                >> function ')' -> 3 | ']'->57 | '}' -> 1197 | '>' -> 25137 | _ -> 0
            ) >> Option.defaultValue 0
        )
    >> List.sum


let input = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"""

input 
|> parse 
|> part1
 = 26397

__SOURCE_DIRECTORY__ + "/part1.txt"
|> System.IO.File.ReadAllText
|> parse
|> part1
 = 436497

let fixClosing x =
    let rec fix pendings chars = 
        match pendings, chars with
        | pendings, [] -> pendings |> List.choose (findClosing)
        | pendings, c::cs when c |> isOpening -> fix (c::pendings) cs
        | p::pendings, c::cs when c |> isClosingChunk p -> fix pendings cs
        | _, _ -> failwithf "unexpected case %A / %A" pendings chars

    (x:string) |> Seq.toList |>  fix []

let linescore x =
    let score = function ')' -> 1L | ']' -> 2L | '}' -> 3L | '>' -> 4L | _ -> 0L
    x |> List.fold (fun s x -> 
        s * 5L + score x
    ) 0L

let finalscore x = 
    let l = x |> List.length
    let middle = l / 2 
    x.[middle]

let part2 = 
    parse 
    >> List.choose exceptCorrupted 
    >> List.map(fun x -> fixClosing x |> fun y -> (x,y))
    >> List.map (snd >> fun x -> (x |> List.toArray |> System.String), linescore x)
    >> List.sortBy snd 
    >> finalscore
    >> snd

part2 input = 288957L

__SOURCE_DIRECTORY__ + "/part1.txt"
|> System.IO.File.ReadAllText
|> part2
 = 2377613374L
