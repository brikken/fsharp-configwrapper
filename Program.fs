// Learn more about F# at http://fsharp.org

open IOUse

[<EntryPoint>]
let main argv =
    writeStream "test.txt"
    0 // return an integer exit code
