module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let chars = "abcdefghjkmnpqrstuvwxyz".ToCharArray()

let toint32 (c:char) = chars |> Array.findIndex ((=) c)

let is3LettersIncreasing (a,b,c) = 
    let ia = toint32 a
    let ib = toint32 b
    let ic = toint32 c

    ib - ia = 1 && ic - ib = 1

let isValid x = 
    let rec hasPairs pairs = function
        | _ when pairs |> Set.count = 2 -> true
        | [] -> false
        | a::b::t when pairs |> Set.count < 2 && a = b && pairs |> Set.contains a |> not -> hasPairs (Set.add a pairs) t
        | _::t -> hasPairs pairs t

    let rec has3LettersIncreasing = function
        | [] -> false
        | a::b::c::_ when is3LettersIncreasing(a,b,c) -> true
        | _::t -> has3LettersIncreasing t

    let vx = (x:string) |> Seq.toList
    let hasPairs = hasPairs Set.empty vx 
    // printfn "hasPairs: %b" hasPairs
    let has3LettersIncreasing = has3LettersIncreasing vx
    // printfn "has3LettersIncreasing: %b" has3LettersIncreasing
    hasPairs && has3LettersIncreasing

try isValid "hijklmmn" with _ -> false |> Test.assertEq "should fail since 'i' 'l' 'o' is not valid" false
isValid "abbceffg" |> Test.assertEq "does not contains 3 letters increasing in a row" false
isValid "abbcegjk" |> Test.assertEq "has only one double letter" false
isValid "abcdffaa" |> Test.assertEq "valid" true
isValid "ghjaabcc" |> Test.assertEq "valid but with ghj since 'i' does not count" true

let next (s:string):string = 
    let next (c:char) = 
        let idx = toint32 c
        let next = 
            if idx = chars.Length - 1 then 0
            else idx + 1
        chars[next]

    let rec nextString canIncrease result = function
        | [] -> result
        | h::t when canIncrease ->
            let nextH = next h
            let canIncrease = nextH = 'a' 
            nextString canIncrease (nextH::result) t
        | h::t -> nextString false (h::result) t
    
    s |> Seq.toList |> List.rev |> nextString true [] |> List.toArray |> System.String

next "xa" |> Test.assertEq "next xa" "xb"
next "xz" |> Test.assertEq "next xz" "ya"
next "zz" |> Test.assertEq "next zz" "aa"

let rec nextValid (password:string) =
    let nextPassword = next password
    if nextPassword |> isValid then nextPassword
    else nextValid nextPassword

nextValid "abcdefgh" |> Test.assertEq "example 1" "abcdffaa"
nextValid "vzbxkghb" |> Test.assertEq "part 1" "vzbxxyzz" |> nextValid |> Test.assertEq "part 2" "vzcaabcc"