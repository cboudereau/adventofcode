
let findMarker length = 
    let rec findMarker i (line:string) =
        if line.Length < length then -1
        else 
            let marker = line.Substring(0, length)
            if marker |> Seq.toArray |> Array.distinct |> Array.length |> (=) length then i + length
            else line.Substring(1) |> findMarker (i+1)
    findMarker 0


"""bvwbjplbgvbhsrlpgdmjqwftvncz""" |> findMarker 4 = 5
"""nppdvjthqldpwncqszvftbrmjlhg""" |> findMarker 4 = 6
"""nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg""" |> findMarker 4 = 10
"""zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw""" |> findMarker 4 = 11

let readFile filePath = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + filePath)

let fullInput = readFile """/day6.txt"""

fullInput |> findMarker 4 = 1794

"mjqjpqmgbljsphdztnvjfqwrcgsmlb" |> findMarker 14 = 19
"bvwbjplbgvbhsrlpgdmjqwftvncz" |> findMarker 14 = 23
"nppdvjthqldpwncqszvftbrmjlhg" |> findMarker 14 = 23
"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |> findMarker 14 = 29
"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |> findMarker 14 = 26

fullInput |> findMarker 14 = 2851