type Cell = Alive | Dead

let getCell world pos =
    let width = Array2D.length1 world
    let height = Array2D.length2 world
    match pos with
        | (x, y) when x < 0 -> None
        | (x, y) when y < 0 -> None
        | (x, y) when x >= width -> None
        | (x, y) when y >= height -> None
        | (x, y) -> Some world.[x, y]

let countAliveNeighbors world pos =
    let isAlive cell =
        match cell with
            | Some cell -> cell = Alive
            | None -> false

    let vicinity coord = seq { coord - 1 .. coord + 1 }

    let neighborPositions = seq {
        for x in vicinity (fst pos) do
            for y in vicinity (snd pos) do
                if (x, y) <> pos then yield (x, y)
    }

    neighborPositions
    |> Seq.map (getCell world)
    |> Seq.filter isAlive
    |> Seq.length

let nextGeneration world x y =
    let cell = getCell world (x, y)
    let count = countAliveNeighbors world (x, y)
    match cell, count with
        | Some Alive, (2 | 3) -> Alive
        | Some Dead, 3 -> Alive
        | _ -> Dead

open System.Windows
open System.Windows.Media.Imaging

let getCellColor cell =
    match cell with
        | Alive -> 0uy
        | Dead -> 255uy

let initializeRandomWorld size =
    let random = System.Random()

    let getRandomCell _ _ =
         match random.Next 3 = 0 with
            | true -> Alive
            | false -> Dead

    Array2D.init size size getRandomCell |> ref

[<EntryPoint>]
[<System.STAThread>]
let main argv =
    let n = 128
    let world = initializeRandomWorld n

    let pixels = Array.create (n * n) 255uy
    let format = Media.PixelFormats.Gray8
    let image = Controls.Image(Stretch = Media.Stretch.Uniform)

    let render _ =
        world := nextGeneration !world |> Array2D.init n n
        for x in 0 .. n - 1 do
            for y in 0 .. n - 1 do
                pixels.[x + y * n] <- getCellColor (!world).[x, y]
        image.Source <- BitmapSource.Create(n, n, 300.0, 300.0, format, null, pixels, n)

    Media.CompositionTarget.Rendering.Add render

    Window(Content = image, Title = "Game of Life", WindowState = WindowState.Maximized) |> Application().Run