module Async =
    let map f x = async.Bind(x, f >> async.Return)

[<Struct>]
type HorizontalPosition = HorizontalPosition of int

[<Struct>]
type Depth = Depth of int

[<Struct>]
type Aim = Aim of int

type Position =
    { Position: HorizontalPosition
      Depth: Depth
      Aim: Aim }

module Position =
    let zero =
        { Position = HorizontalPosition 0
          Depth = Depth 0
          Aim = Aim 0 }

    let value (p: Position) =
        let (HorizontalPosition hp) = p.Position
        let (Depth d) = p.Depth
        hp * d

let down n (p: Position) =
    let (Aim a) = p.Aim in { p with Aim = Aim(a + n) }

let up n (p: Position) =
    let (Aim a) = p.Aim in { p with Aim = Aim(a - n) }

let forward n (p: Position) =
    { p with
        Position = let (HorizontalPosition hp) = p.Position in HorizontalPosition(hp + n)
        Depth =
            let (Depth d) = p.Depth
            let (Aim a) = p.Aim
            Depth(d + a * n) }

Position.zero
|> forward 5
|> down 5
|> forward 8
|> up 3
|> down 8
|> forward 2
|> Position.value = 900

let flip f x y = f y x

module Parser =
    type Command = int -> Position -> Position
    let (<|>) f g = fun x -> f x |> Option.orElse (g x)

    let commandParser =
        let tryInt (x: string) =
            match System.Int32.TryParse(x) with
            | true, v -> Some v
            | _ -> None

        let tryGet (command: string) (f: Command) (input: string) =
            if input.StartsWith(command) then
                input.Substring(command.Length + 1)
                |> tryInt
                |> Option.map f
            else
                None

        let c =
            tryGet "forward" forward
            <|> tryGet "down" down
            <|> tryGet "up" up

        fun x p ->
            c x
            |> Option.map (fun f -> f p)
            |> Option.defaultValue p


    let fileParser filePath =
        filePath
        |> System.IO.File.ReadAllLinesAsync
        |> Async.AwaitTask
        |> Async.map (
            Array.fold (flip commandParser) Position.zero
            >> Position.value
        )

__SOURCE_DIRECTORY__ + "/part2.csv"
|> Parser.fileParser
|> Async.RunSynchronously = 1960569556
