let byLength x = 
    match (x:string).Length with
    | 2 -> Some 1 
    | 3 -> Some 7
    | 4 -> Some 4
    | 7 -> Some 8
    | _ -> None

let mapping = 
    [
     "abcefg", 0
     "cf", 1
     "acdeg", 2 
     "acdfg", 3
     "bcdf", 4
     "abdfg", 5
     "abdefg", 6
     "acf", 7
     "abcdefg", 8
     "abcdfg", 9
    ]

let findMapping notes digits = 
    // let notes = [|"fbegcd"; "cbd"; "adcefb"; "dageb"; "afcb"; "bc"; "aefdc"; "ecdab"; "fgdeca"; "fcdbega"|]
    // let digits = [|"efabcd"; "cedba"; "gadfec"; "cb"|]

    // let notes = [|"fbegcd"; "cbd"; "adcefb"; "dageb"; "afcb"; "bc"; "aefdc"; "ecdab"; "fgdeca"; "fcdbega"|]
    // let digits = [|"efabcd"; "cedba"; "gadfec"; "cb"|]

    let notes = Array.append notes digits |> Array.map (Seq.sort >> Seq.toArray >> System.String) |> Array.distinct

    // 1 4 7 8
    let mapping = notes |> Array.choose (fun x -> byLength x |> Option.map (fun y -> y, x)) |> Map.ofArray
    let remaining = notes |> Array.filter (byLength >> Option.isNone)
    
    let contains digit candidate = digit |> Seq.forall(fun x -> candidate |> Seq.contains x)

    // 1 4 7 8 9
    let l6 = remaining |> Array.filter (fun x -> x.Length = 6)
    let four = mapping |> Map.find 4
    let nine = l6 |> Array.filter (contains four) |> Array.exactlyOne
    let mapping = mapping |> Map.add 9 nine
    let remaining = remaining |> Array.filter ((<>)nine) 

    // 0 1 4 7 8 9
    let l6 = remaining |> Array.filter (fun x -> x.Length = 6)
    let one = mapping |> Map.find 1
    let zero = l6 |> Array.filter (contains one) |> Array.exactlyOne
    let mapping = mapping |> Map.add 0 zero
    let remaining = remaining |> Array.filter ((<>)zero)

    // 0 1 4 6 7 8 9
    let l6 = remaining |> Array.filter (fun x -> x.Length = 6)
    let six = l6 |> Array.filter (contains one >> not) |> Array.exactlyOne
    let mapping = mapping |> Map.add 6 six
    let remaining = remaining |> Array.filter ((<>)six)

    // 0 1 3 4 6 7 8 9
    let l5 = remaining |> Array.filter (fun x -> x.Length = 5)
    let three = l5 |> Array.filter (contains one) |> Array.exactlyOne 
    let mapping = mapping |> Map.add 3 three
    let remaining = remaining |> Array.filter ((<>)three)

    // all
    let five = 
        remaining 
        |> Array.filter (fun candidate -> 
            let sc = candidate |> Set.ofSeq
            let six = six |> Set.ofSeq

            Set.intersect sc six = sc
        )
        |> Array.exactlyOne

    let mapping = mapping |> Map.add 5 five
    let two = remaining |> Array.filter ((<>) five) |> Array.exactlyOne
    let mapping = mapping |> Map.add 2 two
    mapping
    |> Map.toList
    |> List.map (fun (x, y) -> y , x)
    |> Map.ofList

let encode = 
    let mapping = mapping |> List.map (fun (x, y) -> y, x) |> Map.ofList
    fun x -> mapping |> Map.tryFind x    

let score = 
    function
    | 1 | 7 | 4 | 8 -> 1
    | _ -> 0

let parse (x:string) = 
    x.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.choose (fun x -> x.Split('|', System.StringSplitOptions.RemoveEmptyEntries) |> Array.tryItem 1)
    |> Array.map (fun x -> x.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))

let input = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"""

input 
|> parse 
|> Array.map (Array.map byLength) 
|> Array.sumBy (Array.sumBy (Option.defaultValue 0 >> score))

let part1 =
    parse 
    >> Array.map (Array.map byLength) 
    >> Array.sumBy (Array.sumBy (Option.defaultValue 0 >> score))

let part2 x = 
    let parse (x:string) = 
        x.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun x -> 
            x.Split('|', System.StringSplitOptions.RemoveEmptyEntries) 
            |> (function 
                | [|notes; digits|] -> 
                    let split x = (x:string).Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                    Some (split notes, split digits) | _ -> None
            )
        )

    let transcode (s:string) x = 
        match encode x with
        | Some s0 -> Seq.zip s s0 |> Seq.toList
        | None -> List.empty

    let solve (notes: string[]) (digits: string[]) = 
        let mapping = findMapping notes digits
        let digits = digits |> Array.map (fun x -> mapping |> Map.find (x |> Seq.sort |> Seq.toArray |> System.String))
        digits
        

    parse x
    |> Array.map (fun (x,y) -> solve x y, (x, y))

let score2 x = 
    x
    |> Array.rev
    |> Array.mapi(fun i x -> x * (int (10. ** i)))
    |> Array.sum

input 
|> part2
|> Array.map (fst >> score2)
|> Array.sum
 = 61229

__SOURCE_DIRECTORY__ + "/part1.txt"
|> System.IO.File.ReadAllText
|> part1 
 = 330

__SOURCE_DIRECTORY__ + "/part1.txt"
|> System.IO.File.ReadAllText
|> part2
|> Array.map (fst >> score2)
|> Array.sum
 = 1010472 
