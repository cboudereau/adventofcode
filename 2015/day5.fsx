module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let flip f x y = f y x

let isVowel = 
    let vowel = "aeiou".ToCharArray()
    fun x -> vowel |> Array.contains x

let hasBadStrings = 
    let badStrings = [| ('a','b'); ('c','d'); ('p','q'); ('x','y') |]
    fun x -> badStrings |> Array.contains x
    
let part1 (x:string) = 

    let rec validate state l = 
        match state, l with
        | None, h::t -> 
            let n = if isVowel h then 1 else 0
            validate (Some (h,n,0)) t
        | None, [] -> false
        | Some (previous, _, _), h::_ when hasBadStrings (previous, h) -> false
        | Some (previous, numberOfVowels, numberOfDuplicates), h::t -> 
            let n = if isVowel h then numberOfVowels + 1 else numberOfVowels
            let m = if previous = h then numberOfDuplicates + 1 else numberOfDuplicates
            validate (Some (h,n,m)) t
        | Some (_, n, m), _ when n > 2 && m > 0 -> true
        | _ -> false

    x |> Seq.toList |> validate None

part1 "a" |> Test.assertEq "validate ko" false
part1 "jchzalrnumimnmhp" |> Test.assertEq "validate ko" false
part1 "haegwjzuvuyypxyu" |> Test.assertEq "validate ko" false
part1 "dvszwmarrgswjxmb" |> Test.assertEq "validate ko" false
part1 "ugknbfddgicrmopn" |> Test.assertEq "validate ok" true
part1 "aaa" |> Test.assertEq "validate ok" true

let readFile filePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, filePath) |> System.IO.File.ReadAllLines

"day5.txt" |> readFile |> Array.filter part1 |> Array.length |> Test.assertEq "part1" 236

let part2 (x:string) = 
    let rec validate state l = 
        match state, l with
        | None, h1::h2::t -> validate (Some ((h1,h2), Set.empty, 0, 0)) t
        | Some (_, _, n, m), _ when n > 0 && m > 0 -> true
        | Some ((h1, h2), pairs, numberOfPairs, numberOfLetterRepeatingBetweenOneLetter), h3::t ->
            let n = if pairs |> Set.contains (h2,h3) then 1 else 0
            let pairs = Set.add (h1, h2) pairs
            let m = if h1 = h3 then 1 else 0
            validate (Some ((h2, h3), pairs, numberOfPairs + n , numberOfLetterRepeatingBetweenOneLetter + m)) t
        | _ -> false
    x |> Seq.toList |> validate None

"qjhvhtzxzqqjkmpb" |> part2 |> Test.assertEq "part2 ok" true
"xxyxx" |> part2 |> Test.assertEq "part2 ok" true
"aaa" |> part2 |> Test.assertEq "part2 ko" false
"aaaa" |> part2 |> Test.assertEq "part2 (Extra example) ok" true
"uurcxstgmygtbstg" |> part2 |> Test.assertEq "part2 ko" false
"ieodomkazucvgmuy" |> part2 |> Test.assertEq "part2 ko" false

"day5.txt" |> readFile |> Array.filter part2 |> Array.length |> Test.assertEq "part2" 51