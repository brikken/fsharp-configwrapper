// Learn more about F# at http://fsharp.org

open System
open SimpleIO
open SimpleSerialization
open Microsoft.FSharpLu.Json
open VersionedDTOs

let serialize serializer path contents name =
    try    
        contents |> serializer |> fun c -> System.IO.File.WriteAllText(path, c)
        printfn "Serialized %s" name
    with
        ex -> printfn "%s serialization failed: %s" name ex.Message    

[<EntryPoint>]
let main argv =
    [ ("test.txt", "Hej med dig"); ("?\\", "This won't save!") ]
        |> List.iter (fun (path, contents) ->
            match (writeToFile path contents) with
                | Ok msg -> sprintf "OK: %s" msg
                | Error msg -> sprintf "Error: %s" msg
            |> printfn "%s")

    List.iter (fun f ->
        f { text = "Noget skør dansk LÅLÅ tekst! <bløb>"; value = 12; items = [ OptionOne; OptionTwo ("test", 14); OptionThree (12.34) ] }
        |> printfn "%s") [ getXml; Default.serialize; Compact.serialize ]

    let compactSerializer = serialize Compact.serialize

    let dto1File = "DTO1.json"
    serialize Compact.serialize dto1File { DTO1.name = "Test Testersen"; age = 34; boss = Some "Bossy Bosson" } "DTO1"

    let (dto1: DTO1) = System.IO.File.ReadAllText(dto1File) |> Compact.deserialize
    match dto1.boss with
        | None -> printfn "No boss"
        | Some boss -> printfn "The boss is %s" boss

    let dto2 = getDTO2 dto1
    let dto2New = { dto2 with boss = "Mr. Leader" :: dto2.boss }
    dto2New |> Compact.serialize |> printfn "%s"

    let dto2File = "DTO2.json"
    serialize Compact.serialize dto2File dto2New "DTO2"

    try
        System.IO.File.ReadAllText(dto1File) |> Compact.deserialize<DTO2> |> ignore
    with
        ex -> printfn "%s is not of type DTO2: %s" dto1File ex.Message

    0 // return an integer exit code
