// single-module files can use this syntax [Namespace].[Module]
module Checkers.Types

//Sets properties of any individual Checker
type Color = Red | Black    //each checker has a color
type Rank = Soldier | King  //each checker is either normal or king
type Checker = Color * Rank

//A column can be any of the following values, and only these values
type Column = A | B | C | D | E | F | G | H
    with static member List = [A; B; C; D; E; F; G; H]
//A row can be any of the following values, and only these values
type Row = One | Two | Three | Four | Five | Six | Seven | Eight
    with static member List = [One; Two; Three; Four; Five; Six; Seven; Eight]

//each cell consists of both a row and column
type Cell = { Column: Column; Row: Row }
    with
        // it's generally more clear if you define a named member first, then just include an alias as the operator overload
        static member MoveCell target from =
            let findCellIndex list item1 item2 =
                let result =
                    list
                    |> List.zip [0..7]
                    |> List.filter (fun (index, item) -> item = item1 || item = item2)
                    // what on earth is happening here? A comment would be extremely helpful as this logic is unusual
                    |> List.averageBy (fun (index, item) -> float index)
                // F# cast operators are :> and :?>. Wrapping int in (int) does nothing other than look weird
                // In addition, a lot of "casting" C# does isn't actually casting. In this case, F# is doing a
                // _conversion_ to an int, just like C# would.
                int result

            let resultCol = findCellIndex Column.List target.Column from.Column
            let resultRow = findCellIndex Row.List target.Row from.Row

            { Column = Column.List.[resultCol]; Row = Row.List.[resultRow] }

        // much more clear this way, now I don't have to review the code to figure out what it does
        // the function name it calls tells me instantly
        static member (</>) (target: Cell, from: Cell) = Cell.MoveCell target from


//the board is a dictionary of every cell and a checker MAYBE (some cells will be open)
//the Cell is the key, and you will retrieve an option type of either the Checker on that space or None
type Board = Map<Cell, Checker option>
type GameStatus = InProgress | Completed
type GameState = { Board: Board; ColorToMove: Color; Message: string; GameStatus: GameStatus}

//for move validation, use an AttemptedMove -> CompletedMove
type AttemptedMove = { FromCell: Cell; ToCell: Cell }
type PieceCapture = NoCapture | Capture
type Move = { Piece: Checker; FromCell: Cell; ToCell: Cell; CaptureType: PieceCapture }

