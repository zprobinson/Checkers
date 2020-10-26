// Learn more about F# at http://fsharp.org

open System

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


    let testList =
        [for x in 2..4 do
         for y in 2..4 do
         yield (x, y) ]

    printfn "%A" (generateGrid 8 8)




    0 // return an integer exit code
