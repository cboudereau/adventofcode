module Array2D = 
    let mapiset f m = 
        let b1 = Array2D.base1 m
        let b2 = Array2D.base2 m
        let l1 = Array2D.length1 m
        let l2 = Array2D.length2 m

        for i in b1..l1-1 do
            for j in b2..l2-1 do
                let v = Array2D.get m i j
                Array2D.set m i j (f i j v)
        m

let m = Array2D.init 5 10 (+)
m |> Array2D.mapiset (fun i j v -> v+1) 
m |> Array2D.mapi (fun i j v -> v+1) 

let parse (x:string): int[,] = 
    let lines = x.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    let l1 = lines |> Array.length
    let l2 = lines |> Array.head |> String.length
    let m = Array2D.zeroCreate l2 l1
    lines
    |> Array.indexed
    |> Array.fold(fun m (i,s) -> 
        Seq.fold (fun m (j, c) -> 
            System.Char.GetNumericValue c |> int |> Array2D.set m i j
            m
        ) m (Seq.indexed s)) m

let adjacencies maxi maxj i j =
    [| -1, -1
       -1, +0
       -1, +1
       +0, +1
       +1, +1
       +1, +0
       +1, -1
       +0, -1 |]
    |> Array.choose (fun (oi, oj) -> 
        let ni = i + oi
        let nj = j + oj
        if ni >= 0 && ni < maxi && nj >= 0 && nj < maxj then Some (ni, nj)
        else None)

let steps (m:int[,], (counter:int)) = 
    let l1 = Array2D.length1 m
    let l2 = Array2D.length2 m
    let adjacencies = adjacencies l1 l2

    let rec flash i j (counter:int) v m = 
        if v > 9 then
            let counter = counter + 1
            Array2D.set m i j -1
            adjacencies i j
            |> Array.fold (fun counter (i, j) -> 
                let v = Array2D.get m i j
                if v > 0 then
                    let v = v + 1
                    v |> Array2D.set m i j
                    flash i j counter v m
                else counter) counter
        else counter
    let mutable counter = counter
    m |> Array2D.iteri (fun i j v -> v + 1 |> Array2D.set m i j) 
    m |> Array2D.iteri (fun i j v -> counter <- flash i j counter v m)
    m |> Array2D.iteri (fun i j v -> if v < 0 then Array2D.set m i j 0)
    m, counter

let rec repeat n f x = 
    if n = 0 then x
    else repeat (n-1) f (f x)

let repeatWhile test f x = 
    let rec repeatWhile n test f x =
        let y = (f x)
        let n = n + 1
        if test x y then n
        else repeatWhile n test f y
    repeatWhile 0 test f x
    

let input = """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""

let input2="""11111
19991
19191
19991
11111"""

let input3="""11111
19111
19111
11111
11111"""

let m0 = input |> parse
// let m1 = steps m0
// let m2 = steps m1

(m0, 0) |> repeat 100 steps

let real = """3322874652
5636588857
7755117548
5854121833
2856682477
3124873812
1541372254
8634383236
2424323348
2265635842"""

(parse real, 0) |> repeat 100 steps

("""5877777777
8877777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777""" 
|> parse, 193)
|> repeatWhile (fun x y -> snd y - snd x = 100) steps

(parse real, 0)
|> repeatWhile (fun x y -> snd y - snd x = 100) steps