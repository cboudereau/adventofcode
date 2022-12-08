module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual

let parse (input:string[]) = 
    let l1 = input |> Array.length
    let l2 = input |> Array.head |> fun x -> x.Length
    let aa = input |> Array.map (fun x -> x.ToCharArray() |> Array.map (int >> (+) -48))
    Array2D.init<int> l1 l2 (fun y x -> aa[y][x])

let day81 m =
    let h = m |> Array2D.length1
    let l = m |> Array2D.length2

    [|
        for y = 1 to h - 2 do
            for x = 1 to l - 2 do
                let v = m[y,x]
                
                let adjacencies = 
                    [
                        [for j=y-1 downto 0 do yield j, x]
                        [for j=y+1 to h-1 do yield j, x]
                        [for i=x-1 downto 0 do yield y, i]
                        [for i=x+1 to l-1 do yield y, i]
                    ]

                let rec isVisible1D =
                    function
                    | [] -> true
                    | (y,x)::t when v > m[y,x] -> isVisible1D t
                    | _::_ -> false

                let rec isVisible = 
                    function
                    | [] -> false
                    | h::_ when isVisible1D h -> true
                    | _::t -> isVisible t

                let isVisible = adjacencies |> isVisible
                if isVisible then yield v, (y,x)
    |] 
    |> Array.length
    |> fun x -> x + ((2 * h) + 2* (l-2))

let input = """30373
25512
65332
33549
35390""" |> fun x -> x.Split('\n')

let readFile filePath = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + filePath)

input |> parse |> day81 |> Test.assertEq "day 8-1 input" 21 
"""/day8.txt""" |> readFile |> parse |> day81 |> Test.assertEq "day 8-1 full input" 1698


let day82 m = 
    let h = m |> Array2D.length1
    let l = m |> Array2D.length2

    [|
        for y = 1 to h - 2 do
            for x = 1 to l - 2 do
                let v = m[y,x]

                let adjacencies = 
                    [
                        [for j=y-1 downto 0 do yield j, x]
                        [for j=y+1 to h-1 do yield j, x]
                        [for i=x-1 downto 0 do yield y, i]
                        [for i=x+1 to l-1 do yield y, i]
                    ]

                let rec scenicScore1D i =
                    function
                    | [] -> i
                    | (y,x)::t when v > m[y,x] -> scenicScore1D (i + 1L) t
                    | _::_ -> i + 1L

                let rec scenicScore total = 
                    function
                    | [] -> total
                    | h::t -> scenicScore (total * scenicScore1D 0 h) t

                let score = scenicScore 1 adjacencies
                if score > 0L then yield (v, score), (y,x)
    |] 
    |> Array.sortByDescending(fst >> snd)
    |> Array.head
    |> (fst >> snd)

input |> parse |> day82 |> Test.assertEq "input day 8-2" 8L
"""/day8.txt""" |> readFile |> parse |> day82 |> Test.assertEq "full input day 8-1" 672280L