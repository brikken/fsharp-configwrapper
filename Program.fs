// Learn more about F# at http://fsharp.org

open System
open SimpleIO

[<EntryPoint>]
let main argv =
    [ ("test.txt", "Hej med dig"); ("?\\", "This won't save!") ]
        |> List.iter (fun (path, contents) ->
            match (writeToFile path contents) with
                | Ok msg -> sprintf "OK: %s" msg
                | Error msg -> sprintf "Error: %s" msg
            |> printfn "%s")        
    0 // return an integer exit code
