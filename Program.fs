// Learn more about F# at http://fsharp.org

open System
open RobustIO.Directory

[<EntryPoint>]
let main argv =
    let spec = {
        name = "test";
        directories = [
            {
                name = "foldermatch";
                directories = [];
                files = [ "foldermatchmissingfile" ];
            };
            {
                name = "foldermissing";
                directories = [];
                files = [];
            };
        ];
        files = [ "test"; "missing"; ];
    }
    let compare = Specification.compareDirectory (IO.DirectoryInfo("test")) spec
    let printFilePaths specOpt title =
        match specOpt with
        | Some spec ->
            printfn "%s" title
            Specification.getFilePaths spec |> List.iter (fun p -> printfn "%s" p)
        | None -> ()
    match compare with
    | Ok comparison ->
        printFilePaths comparison.missing "Missing files"
        printFilePaths comparison.additional "Additional files"
    | Error _ -> ()
    0 // return an integer exit code
