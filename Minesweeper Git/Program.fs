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

let neighbours row column =
    [for i = -1 to 1 do
        for j = -1 to 1 do
            let r, c = row + i, column + j
            if not(r = row && c = column) && 
               0 <= r && r < numRows && 
               0 <= c && c < numColumns then 
                yield r, c]

let revealTile row column disable =
    let button, tileState, bombState = tiles.[row].[column]
    match bombState with
    | Bomb -> button.Content <- "B"
    | NoBomb(x) -> button.Content <- if x = 0 then "" else string x
    button.IsChecked <- Nullable(true)
    if disable then
        button.IsEnabled <- false
    tiles.[row].[column] <- button, Revealed, bombState

let revealAll() =
    for i = 0 to numRows - 1 do
        for j = 0 to numColumns - 1 do
            revealTile i j true

let rec expandTile row column =
    match tiles.[row].[column] with
    | _, Unknown, NoBomb(x) ->
        revealTile row column false
        if x = 0 then
            for r, c in neighbours row column do
                expandTile r c
    | _ -> ()

    
let numNeighbourBombs row column = 
    neighbours row column 
    |> List.sumBy(fun (r, c) -> match tiles.[r].[c] with
                                | _,_,Bomb -> 1 
                                | _ -> 0)

let numNeighbourFlags row column = 
    neighbours row column 
    |> List.sumBy(fun (r, c) -> match tiles.[r].[c] with
                                | _,Flagged,_ -> 1 
                                | _ -> 0)

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
        

let onLeftClick row column =
    let button, tileState, bombState = tiles.[row].[column]    
    if tileState = Unknown then
        expandTile row column
        checkVictory (bombState = Bomb) |> ignore

let onRightClick row column =
    let button, tileState, b = tiles.[row].[column]
    match tileState with
    | Unknown -> 
        button.Content <- "¶"
        tiles.[row].[column] <- button, Flagged, b
    | Flagged -> 
        button.Content <- ""
        tiles.[row].[column] <- button, Unknown, b
    | _ -> ()
    

let onDoubleButtonClick row column =
    let rec expandTilesManual (row,column) firstLevel =
        let _,state,bomb = tiles.[row].[column]
        if not (firstLevel && state <> Revealed) then
            match bomb with
            | NoBomb(x) ->
                if firstLevel && x <= (numNeighbourFlags row column) || x = 0 then
                    for (r,c) in (neighbours row column) do
                        let _,state,bomb = tiles.[r].[c]                    
                        if state = Unknown then
                            revealTile r c false
                            if not (checkVictory (bomb = Bomb)) then
                                expandTilesManual (r,c) false
            | _ -> Debug.Fail("Cannot expand a tile where there is a bomb")

    expandTilesManual (row, column) true

let mutable doubleButtonPress = false

let onMouseUp row column (e:MouseButtonEventArgs ) =
    doubleButtonPress <- 
        match e.ChangedButton with
        | MouseButton.Left ->
            if Mouse.RightButton = MouseButtonState.Pressed then true
            elif doubleButtonPress then onDoubleButtonClick row column; false
            else onLeftClick row column; false
        | MouseButton.Right ->
            if Mouse.LeftButton = MouseButtonState.Pressed then true
            elif doubleButtonPress then onDoubleButtonClick row column; false
            else onRightClick row column; false
        | _ -> doubleButtonPress

let ResetField() =
    let bombs = ResizeArray<int>()
    let rand = Random()
    while bombs.Count < numBombs do
        let location = rand.Next(numRows * numColumns - 1)
        if not (bombs.Contains(location)) then
            bombs.Add(location)

    // First pass: add the bombs to the field
    for i = 0 to numRows - 1 do
        for j = 0 to numColumns - 1 do
            let button, state, _ = tiles.[i].[j]
            button.IsChecked <- Nullable(false)
            button.IsEnabled <- true
            button.Content <- ""
            tiles.[i].[j] <- button, Unknown, if bombs.Contains(i * numColumns + j) then Bomb else NoBomb(0)

    // In a second pass, compute neighbours for each tile
    for i = 0 to numRows - 1 do
        for j = 0 to numColumns - 1 do
            match tiles.[i].[j] with
            | button, state, NoBomb(_) -> tiles.[i].[j] <- button, state, NoBomb(numNeighbourBombs i j)
            | _ -> ()
    
[<EntryPoint;STAThread>]
let main argv = 

    let gameGrid = Grid()
    for i = 1 to numColumns do
        gameGrid.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength(0.0, GridUnitType.Auto)))
    for i = 1 to numRows do
        gameGrid.RowDefinitions.Add(RowDefinition(Height = GridLength(0.0, GridUnitType.Auto)))

    for i = 0 to numRows - 1 do
        for j = 0 to numColumns - 1 do
            let button = ToggleButton(Width = 32.0, Height = 32.0)
            button.MouseUp.Add(onMouseUp i j)
            button.PreviewMouseLeftButtonDown.Add(fun e -> e.Handled <- true)
            button.PreviewMouseLeftButtonUp.Add(fun e -> onMouseUp i j e; e.Handled <- true)
            tiles.[i].[j] <- button, Unknown, NoBomb(0)
            gameGrid.Children.Add(button) |> ignore
            Grid.SetColumn(button, j)
            Grid.SetRow(button, i)
            
    ResetField()            

    let controlGrid = Grid()
    controlGrid.RowDefinitions.Add(RowDefinition(Height = GridLength(0.0, GridUnitType.Auto)))
    let newGameButton = Button(Width = 64.0, Height = 64.0, Content="Reset")
    newGameButton.Click.Add(fun _ -> ResetField())
    controlGrid.Children.Add(newGameButton) |> ignore
    Grid.SetRow(newGameButton, 0)

    let mainGrid = Grid()
    mainGrid.RowDefinitions.Add(RowDefinition(Height = GridLength(0.0, GridUnitType.Auto)))
    mainGrid.RowDefinitions.Add(RowDefinition(Height = GridLength(0.0, GridUnitType.Auto)))
    mainGrid.Children.Add(controlGrid) |> ignore
    mainGrid.Children.Add(gameGrid) |> ignore
    Grid.SetRow(controlGrid, 0)
    Grid.SetRow(gameGrid, 1)
    
    let window = Window(Title = "MineSweeper",
                        Content = mainGrid,
                        SizeToContent = SizeToContent.WidthAndHeight,
                        ResizeMode = ResizeMode.NoResize)
    Application().Run(window)