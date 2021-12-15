let rnd = 
    let rnd = System.Random()
    fun i -> rnd.Next(-100, i)

let pairs = [1;27; 22; 6; -20; 48; 28; 0] |> List.sortBy rnd

let r = 
    pairs 
    |> List.fold (fun (previous, pairs) x -> 
        let y = 28 - x

        match previous |> Map.tryFind y with
        | Some y -> previous |> Map.remove y, (x, y) :: pairs
        | None -> previous |> Map.add x x, pairs
        
    ) (Map.empty, [])

r |> fst = Map.empty
(r |> snd |> List.length) = (pairs |> List.length) / 2