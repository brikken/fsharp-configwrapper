// Learn more about F# at http://fsharp.org

open System
open SimpleIO
open SimpleSerialization

[<EntryPoint>]
let main argv =
    [ ("test.txt", "Hej med dig"); ("?\\", "This won't save!") ]
        |> List.iter (fun (path, contents) ->
            match (writeToFile path contents) with
                | Ok msg -> sprintf "OK: %s" msg
                | Error msg -> sprintf "Error: %s" msg
            |> printfn "%s")
    List.iter (fun f ->
        f { text = "Noget skør dansk LÅLÅ tekst! <bløb>"; value = 12; items = [ OptionOne; OptionTwo ("test", 14) ] }
        |> printfn "%s") [ getXml; getSerializerXml ]
    0 // return an integer exit code
