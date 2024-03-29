#r "System.Security.Cryptography"
open System.Security.Cryptography
open System.Text

module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
        actual

let md5 (data : string) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(System.Text.UTF8Encoding.UTF8.GetBytes(data)))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let magicHash value secret =
    let rec magicHash n secret = 
        let md5 = md5 (sprintf "%s%i" secret n)
        if md5.StartsWith(value:string) then n
        else magicHash (n + 1) secret 
    magicHash 0 secret 

let part1 = magicHash "00000"
part1 "abcdef" |> Test.assertEq "abcdef609043" 609043
part1 "pqrstuv" |> Test.assertEq "pqrstuv1048970" 1048970

part1 "ckczppom" |> Test.assertEq "ckczppom117946" 117946

let part2 = magicHash "000000"
part2 "ckczppom"