module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Int64|_|) x = 
    match System.Int64.TryParse(x:string) with
    | true, v -> Some v
    | false, _ -> None

let ls =
    Array.fold (fun ((cd:string option), s) x -> 
        match cd, x with
        | Some cd, """$ cd ..""" -> 
            let a = cd.Split('/', System.StringSplitOptions.RemoveEmptyEntries)    
            let parts = a[0..a.Length - 2] 
            let absolutePath = 
                if parts.Length > 0 then parts |> String.concat "/" |> sprintf "/%s/" 
                else "/" 
            Some absolutePath , s
        | _, Regex "cd (/.*)" [absolutePath] -> Some absolutePath, s
        | Some cd, Regex "cd (.*)" [relativePath] -> 
            let absolutePath = sprintf "%s%s/" cd relativePath
            Some (absolutePath), s
        | Some cd, Regex """(\d*) (.*)""" [Int64 l; file] -> 
            let s = 
                cd.Split ('/', System.StringSplitOptions.RemoveEmptyEntries) 
                |> Seq.fold (fun (cd,s) x -> 
                    let ncd = sprintf "%s%s/" cd x
                    let s = s |> Map.change ncd (function Some previous -> Some (previous + l) | None -> Some l)
                    ncd, s) ("/", s)
                |> snd
                |> Map.change "/" (function Some previous -> Some (previous+l)|None -> Some l)
            (Some cd, s)
        | _, _ -> cd, s
    ) (None, Map.empty)
    >> snd
    >> Map.toList

let day71 = ls >> List.filter (snd >> fun x -> x <= 100000) >> List.sumBy snd

let input = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k""" |> fun x -> x.Split("\n") 

input 
|> day71
|> Test.assertEq "input day71" 95437L

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath)

let fullInput = """/day7.txt""" |> readFile
fullInput 
|> day71 
|> Test.assertEq "full input day71" 1783610L

let day72 x = 
    let dirs = ls x
    let unused = 70000000L - (dirs |> List.head |> snd) 

    let freeup = 30000000L - unused

    dirs
    |> List.filter (snd >> fun x -> x >= freeup)
    |> List.sortBy snd
    |> List.head
    |> snd

input 
|> day72
|> Test.assertEq "input day72" 24933642

fullInput
|> day72
|> Test.assertEq "full input day72" 4370655L