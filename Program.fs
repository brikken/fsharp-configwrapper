// Learn more about F# at http://fsharp.org

open System
open SimpleIO
open SimpleSerialization
open Microsoft.FSharpLu.Json
open VersionedDTOs
open BinarySerialization
open AtomicIO

let serialize serializer path contents name =
    try    
        contents |> serializer |> fun c -> System.IO.File.WriteAllText(path, c)
        printfn "Serialized %s" name
    with
        ex -> printfn "%s serialization failed: %s" name ex.Message    

[<EntryPoint>]
let main argv =
    match AtomicIO.writeAllText "test.txt" "Dette er min vigtige tekst!" with
    | Ok _ -> printfn "Ok!"
    | Error errors ->
        printfn "Error!"
        errors |> List.iter (fun err -> printfn "%s" (err.ToString()))
    0 // return an integer exit code
