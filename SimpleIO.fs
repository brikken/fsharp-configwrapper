module SimpleIO

open System.IO

let writeToFile path contents =
    try
        File.WriteAllText(path, contents)
        Ok (sprintf "contents saved to %s" path)
    with
        ex -> Error ex.Message 