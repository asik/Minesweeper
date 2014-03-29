open System
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Input
open System.Diagnostics

type TileState =
| Unknown
| Flagged
| Revealed

type TileType =
| Bomb
| NoBomb of int

type Tile = ToggleButton * TileState * TileType
    
let numRows, numColumns = 16, 16 
let numBombs = 32
let tiles = Array.init numRows (fun i -> Array.zeroCreate<Tile> numColumns)
let coords = [|for i = 0 to numRows - 1 do
                   for j = 0 to numColumns - 1 do
                       yield (i,j)|]

let revealTile disable (row, column)  =
    let button, tileState, bombState = tiles.[row].[column]
    match bombState with
    | Bomb -> button.Content <- "B"
    | NoBomb(x) -> button.Content <- if x = 0 then "" else string x
    button.IsChecked <- Nullable(true)
    if disable then
        button.IsEnabled <- false
    tiles.[row].[column] <- button, Revealed, bombState

let revealAll() = coords |> Array.iter (revealTile true)

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

let countNeighboursBy func =
    neighbours >> List.sumBy (fun (r,c) -> func (tiles.[r].[c]))
    
let numNeighbourBombs = 
    countNeighboursBy (function _,_,Bomb -> 1 | _ -> 0)

let numNeighbourFlags = 
    countNeighboursBy (function _,Flagged,_ -> 1 | _ -> 0)

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
        

let onLeftClick (row, column) =
    let button, tileState, bombState = tiles.[row].[column]    
    if tileState = Unknown then
        expandTile (row, column)
        checkVictory (bombState = Bomb) |> ignore

let onRightClick (row, column) =
    let button, tileState, b = tiles.[row].[column]
    match tileState with
    | Unknown -> 
        button.Content <- "¶"
        tiles.[row].[column] <- button, Flagged, b
    | Flagged -> 
        button.Content <- ""
        tiles.[row].[column] <- button, Unknown, b
    | _ -> ()
    

let onDualClick (row, column) =
    let rec dualClickExpand (row,column) firstLevel =
        let _,state,bomb = tiles.[row].[column]
        if not (firstLevel && state <> Revealed) then
            match bomb with
            | NoBomb(x) ->
                if firstLevel && x <= (numNeighbourFlags (row, column)) || x = 0 then
                    for (r,c) in (neighbours (row ,column)) do
                        let _,state,bomb = tiles.[r].[c]                    
                        if state = Unknown then
                            revealTile false (r, c)
                            if not (checkVictory (bomb = Bomb)) then
                                dualClickExpand (r,c) false
            | _ -> Debug.Fail("Cannot expand a tile where there is a bomb")

    dualClickExpand (row, column) true

let mutable dualClick = false

let onMouseUp coords (e:MouseButtonEventArgs) =
    dualClick <- 
        match e.ChangedButton with
        | MouseButton.Left ->
            if Mouse.RightButton = MouseButtonState.Pressed then true
            elif dualClick then onDualClick coords; false
            else onLeftClick coords; false
        | MouseButton.Right ->
            if Mouse.LeftButton = MouseButtonState.Pressed then true
            elif dualClick then onDualClick coords; false
            else onRightClick coords; false
        | _ -> dualClick

let resetField() =
    let bombs = ResizeArray<int>()
    let rand = Random()
    while bombs.Count < numBombs do
        let location = rand.Next(numRows * numColumns - 1)
        if not (bombs.Contains(location)) then
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
        button.MouseUp.Add(onMouseUp (i, j))
        button.PreviewMouseLeftButtonDown.Add(fun e -> e.Handled <- true)
        button.PreviewMouseLeftButtonUp.Add(fun e -> onMouseUp (i, j) e; e.Handled <- true)
        tiles.[i].[j] <- button, Unknown, NoBomb(0)
        addToGrid gameGrid button i j
            
    resetField()            

    let controlGrid = Grid()
    addDefaultRow controlGrid
    let newGameButton = Button(Width = 64.0, Height = 64.0, Content="Reset")
    newGameButton.Click.Add(fun _ -> resetField())
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