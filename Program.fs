// Learn more about F# at http://fsharp.org

open System
open SimpleIO
open SimpleSerialization
open Microsoft.FSharpLu.Json
open VersionedDTOs
open BinarySerialization

let serialize serializer path contents name =
    try    
        contents |> serializer |> fun c -> System.IO.File.WriteAllText(path, c)
        printfn "Serialized %s" name
    with
        ex -> printfn "%s serialization failed: %s" name ex.Message    

[<EntryPoint>]
let main argv =
    printfn "%s" (BinarySerialization.serialize CaseA)
    printfn "%s" (BinarySerialization.serialize (CaseB "test"))
    printfn "%s" (BinarySerialization.serialize (CaseC (DateTime(2000,1,1), 10.2M)))
    printfn "%s" (BinarySerialization.serialize (CaseD { value = 10; name = "something"; }))
    0 // return an integer exit code
