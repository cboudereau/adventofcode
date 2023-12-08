module Test = 
    let assertEq msg expected actual =
        if expected = actual then printfn "Test %s Ok" msg
        else failwithf "Test %s failed: expected '%A' but got '%A'" msg expected actual
let example = 
    [ 7L, 9L
      15L, 40L
      30L, 200L ]

let input = 
    [ 46L, 214L
      80L, 1177L
      78L, 1402L
      66L, 1024L ]

let part1 = 
    List.map (fun (time, distance) -> 
        [ 1L .. time - 1L ]
        |> List.choose (fun tb ->
            let td = time - tb
            let d = td * tb
            if d > distance then Some(tb, d)
            else None
        )
        |> List.length
    )
    >> List.fold (*) 1

example |> part1 |> Test.assertEq "part1 example" 288
input |> part1 |> Test.assertEq "part1" 512295

[ 71530L, 940200L ] |> part1 |> Test.assertEq "realExample" 71503

[ 46807866L, 214117714021024L ] |> part1 |> Test.assertEq "part2" 36530883
