// Learn more about F# at http://fsharp.org

open System
open SimpleIO
open SimpleSerialization
open Microsoft.FSharpLu.Json
open VersionedDTOs

[<EntryPoint>]
let main argv =
    // [ ("test.txt", "Hej med dig"); ("?\\", "This won't save!") ]
    //     |> List.iter (fun (path, contents) ->
    //         match (writeToFile path contents) with
    //             | Ok msg -> sprintf "OK: %s" msg
    //             | Error msg -> sprintf "Error: %s" msg
    //         |> printfn "%s")

    // List.iter (fun f ->
    //     f { text = "Noget skør dansk LÅLÅ tekst! <bløb>"; value = 12; items = [ OptionOne; OptionTwo ("test", 14); OptionThree (12.34) ] }
    //     |> printfn "%s") [ getXml; Default.serialize; Compact.serialize ]
    
    try    
        { name = "Test Testersen"; age = 34; boss = Some "Bossy Bosson" } |> Compact.serialize |> fun c -> System.IO.File.WriteAllText("DTO1.json", c)
        printfn "Serialized DTO1"
    with
        ex -> printfn "DTO1 serialization failed: %s" ex.Message    
    0 // return an integer exit code
