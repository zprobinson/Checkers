namespace Checkers

module CheckerTypes = 
    //Sets properties of any individual Checker
    type Color = Red | Black    //each checker has a color
    type Rank = Soldier | King  //each checker is either normal or king
    type Checker = Color * Rank

    //A column can be any of the following values, and only these values
    type Column = A | B | C | D | E | F | G | H
        with static member List = [A;B;C;D;E;F;G;H]
    //A row can be any of the following values, and only these values
    type Row = One | Two | Three | Four | Five | Six | Seven | Eight
        with static member List = [One; Two; Three; Four; Five; Six; Seven; Eight]

    type Cell = { Column: Column; Row: Row }   //each cell consists of both a row and column
        with static member (</>) (target: Cell, from: Cell) = 

                let findCellIndex list item1 item2 =
                    let result = 
                        list
                        |> List.zip [0..7]
                        |> List.filter (fun (index, item) -> item = item1 || item = item2)
                        |> List.averageBy (fun (index, item) -> (float)index)
                    (int) result

                let resultRow = findCellIndex Row.List target.Row from.Row
                let resultCol = findCellIndex Column.List target.Column from.Column

                //let resultRow : int =
                //    let result =
                //        Row.List
                //        |> List.zip [0..7]
                //        |> List.filter (fun (index, item) -> item = target.Row || item = from.Row)
                //        |> List.averageBy (fun (index, item) -> (float)index)
                //    (int) result

                //let resultCol : int = 
                //    let result = 
                //        Column.List
                //        |> List.zip [0..7]
                //        |> List.filter (fun (index, item) -> item = target.Column || item = from.Column)
                //        |> List.averageBy (fun (index, item) -> (float)index)
                //    (int) result

                { Column = Column.List.[resultCol]; Row = Row.List.[resultRow] }

    //the board is a dictionary of every cell and a checker MAYBE (some cells will be open)
    //the Cell is the key, and you will retrieve an option type of either the Checker on that space or None
    type Board = Map<Cell, Checker option>
    type GameStatus = InProgress | Completed
    type GameState = { Board: Board; ColorToMove: Color; Message: string; GameStatus: GameStatus}

    //for move validation, use an AttemptedMove -> CompletedMove
    type AttemptedMove = { FromCell: Cell; ToCell: Cell }
    type PieceCapture = NoCapture | Capture
    type Move = { Piece: Checker; FromCell: Cell; ToCell: Cell; CaptureType: PieceCapture }

