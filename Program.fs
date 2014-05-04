open System
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Input
open System.Windows.Media
open System.Diagnostics

type TileState =
| Unknown
| Flagged
| Revealed

type TileType =
| Bomb
| NoBomb of int

type Tile = ToggleButton * TileState * TileType
   
let mutable resetFieldOnNextClick = true
let numRows, numColumns = 16, 32 
let numBombs = 100
let tiles = Array.init numRows (fun i -> Array.zeroCreate<Tile> numColumns)
let coords = [|for i = 0 to numRows - 1 do
                   for j = 0 to numColumns - 1 do
                       yield (i,j)|]

let color = function
| 0 -> Colors.White
| 1 -> Colors.Blue
| 2 -> Colors.Green
| 3 -> Colors.Red
| 4 -> Colors.DarkBlue
| 5 -> Colors.Maroon
| 6 -> Colors.Turquoise
| 7 -> Colors.Black
| 8 -> Colors.DarkGray
| _ -> failwith "invalid number"

let revealTile disable (row, column)  =
    let button, tileState, bombState = tiles.[row].[column]
    match bombState with
    | Bomb -> button.Content <- "B"
    | NoBomb(x) -> 
        button.Content <- if x = 0 then "" else string x
        button.Foreground <- SolidColorBrush(color x)
    button.IsChecked <- Nullable(true)
    button.IsEnabled <- not disable
    tiles.[row].[column] <- button, Revealed, bombState

let revealAll() = Array.iter (revealTile true) coords

let neighbours (row, column) =
    [for i = -1 to 1 do
        for j = -1 to 1 do
            let r, c = row + i, column + j
            if not(r = row && c = column) && 
               0 <= r && r < numRows && 
               0 <= c && c < numColumns then 
                yield r, c]

let rec expandTile (row, column) =
    match tiles.[row].[column] with
    | _, Unknown, NoBomb(x) ->
        revealTile false (row, column)
        if x = 0 then
            neighbours (row, column) |> List.iter expandTile
    | _ -> ()

let countNeighboursBy predicate =
    neighbours >> List.filter (fun (r,c) -> predicate (tiles.[r].[c])) >> List.length
    
let numNeighbourBombs = 
    countNeighboursBy (fun (_,_,b) -> b = Bomb)

let numNeighbourFlags = 
    countNeighboursBy (fun (_,s,_) -> s = Flagged)

let checkVictory currentTileHasBomb =
    if currentTileHasBomb then
        revealAll()
        MessageBox.Show("You lose!") |> ignore
        true
    elif tiles |> Array.forall (Array.forall (fun (_,s,b) -> b = Bomb || s = Revealed)) then
        revealAll()
        MessageBox.Show("You win!") |> ignore
        true
    else 
        false
  

let resetField (i, j) =
    let noBombZone = (i,j)::(neighbours (i,j))
    let bombs = ResizeArray<int>()
    let rand = Random()
    while bombs.Count < numBombs do
        let location = rand.Next(numRows * numColumns - 1)
        if not (bombs.Contains(location)) && 
           not (List.exists (fun (ci, cj) -> ci *numColumns + cj = location) noBombZone) then
            bombs.Add(location)   

    // First pass: add the bombs to the field
    for (i, j) in coords do
        let button, state, _ = tiles.[i].[j]
        button.IsChecked <- Nullable(false)
        button.IsEnabled <- true
        button.Content <- ""
        tiles.[i].[j] <- button, Unknown, if bombs.Contains(i * numColumns + j) then Bomb else NoBomb(0)

    // In a second pass, compute neighbours for each tile
    for (i, j) in coords do
        match tiles.[i].[j] with
        | button, state, NoBomb(_) -> tiles.[i].[j] <- button, state, NoBomb(numNeighbourBombs (i, j))
        | _ -> ()   

let onLeftClick (row, column) =
    if resetFieldOnNextClick then
        resetField(row, column)
        resetFieldOnNextClick <- false
    let button, tileState, bombState = tiles.[row].[column]    
    if tileState = Unknown then
        expandTile (row, column)
        checkVictory (bombState = Bomb) |> ignore

let onRightClick (row, column) =
    let button, tileState, b = tiles.[row].[column]
    match tileState with
    | Unknown -> 
        button.Content <- "¶"
        button.Foreground <- SolidColorBrush(Colors.DarkRed)
        tiles.[row].[column] <- button, Flagged, b
    | Flagged -> 
        button.Content <- ""
        tiles.[row].[column] <- button, Unknown, b
    | _ -> ()
    

let onDualClick (row, column) =
    let _,state,bomb = tiles.[row].[column]
    match state, bomb with
    | Revealed, NoBomb(x) ->
        if x <= (numNeighbourFlags (row, column)) then
            for (r,c) in (neighbours (row ,column)) do
                let _,state,bomb = tiles.[r].[c]
                match state, bomb with
                | Unknown, Bomb -> checkVictory true |> ignore
                | Unknown, NoBomb(x) -> 
                    revealTile false (r,c)                
                    if not (checkVictory false) && x = 0 then
                        neighbours (r,c) |> List.iter expandTile
                | _ -> ()
    | _ -> ()

let mutable dualClick = false

let onMouseUp clickLocation (e:MouseButtonEventArgs) =
    dualClick <- 
        match e.ChangedButton with
        | MouseButton.Left ->
            if Mouse.RightButton = MouseButtonState.Pressed then true
            elif dualClick then onDualClick clickLocation; false
            else onLeftClick clickLocation; false
        | MouseButton.Right ->
            if Mouse.LeftButton = MouseButtonState.Pressed then true
            elif dualClick then onDualClick clickLocation; false
            else onRightClick clickLocation; false
        | _ -> dualClick
    
[<EntryPoint;STAThread>]
let main argv = 

    let addToGrid (grid:Grid) (uie:UIElement) row column =
        grid.Children.Add(uie) |> ignore
        Grid.SetColumn(uie, column)
        Grid.SetRow(uie, row)

    let addDefaultColumn (grid:Grid) =
        grid.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength(0.0, GridUnitType.Auto)))

    let addDefaultRow (grid:Grid) =
        grid.RowDefinitions.Add(RowDefinition(Height = GridLength(0.0, GridUnitType.Auto)))

    let gameGrid = Grid()
    [1 .. numColumns] |> List.iter(fun _ -> addDefaultColumn gameGrid)
    [1 .. numRows] |> List.iter(fun _ -> addDefaultRow gameGrid)

    for (i, j) in coords do
        let button = ToggleButton(Width = 32.0, Height = 32.0)
        button.FontWeight <- FontWeights.ExtraBold
        button.MouseUp.Add(onMouseUp (i, j))
        button.PreviewMouseLeftButtonDown.Add(fun e -> e.Handled <- true)
        button.PreviewMouseLeftButtonUp.Add(fun e -> onMouseUp (i, j) e; e.Handled <- true)
        tiles.[i].[j] <- button, Unknown, NoBomb(0)
        addToGrid gameGrid button i j        

    let controlGrid = Grid()
    addDefaultRow controlGrid
    let newGameButton = Button(Width = 64.0, Height = 64.0, Content="Reset")
    newGameButton.Click.Add(fun _ -> resetField(0,0); resetFieldOnNextClick <- true)
    addToGrid controlGrid newGameButton 0 0

    let mainGrid = Grid()
    addDefaultRow mainGrid
    addDefaultRow mainGrid
    addToGrid mainGrid controlGrid 0 0
    addToGrid mainGrid gameGrid 1 0
    
    let window = Window(Title = "MineSweeper",
                        Content = mainGrid,
                        SizeToContent = SizeToContent.WidthAndHeight,
                        ResizeMode = ResizeMode.NoResize)
    Application().Run(window)