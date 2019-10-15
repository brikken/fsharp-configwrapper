// Learn more about F# at http://fsharp.org

open System
open BinarySerialization

[<EntryPoint>]
let main argv =
    serializeToFile "test.bin" 10000000
    0 // return an integer exit code
