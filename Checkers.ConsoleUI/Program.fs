// Learn more about F# at http://fsharp.org

open System
open Checkers.CheckerTypes
open Checkers

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let generateEmptyRow maxRows =
        [for x in 1..maxRows do
         yield sprintf " [%d]" x]

    let generateColumns maxCols f =
        [for y in 1..maxCols do
         yield! f]

    let generateGrid x y= generateColumns y (generateEmptyRow x)

    let findIndexFromList list cell =
        list |> List.findIndex (fun c -> c = cell)
        
    let deserializeCoordinate row col =
        (findIndexFromList Row.List row, findIndexFromList Column.List col)

    let checkersGrid = 
        [for row in Row.List do
         for col in Column.List do
         match col with
         | H -> printfn " [%A]" (deserializeCoordinate row col)
         | _ -> printf " [%A]" (deserializeCoordinate row col) ]

    //let testList =
    //    [for x in 2..4 do
    //     for y in 2..4 do
    //     yield (x, y) ]

    //printfn "%A" (generateGrid 8 8)






    0 // return an integer exit code
