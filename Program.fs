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
    0 // return an integer exit code
