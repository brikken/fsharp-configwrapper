module RobustIO

module Directory =
    type Name = DirectoryName of string
    type FileName = FileName of string
    type ListingEntry =
        | DirectoryEntry of Name
        | FileEntry of FileName
    type Listing = Listing of ListingEntry list

    [<NoComparison>]
    type Specification = {
        name: string
        directories: Specification list
        files: string list
    }

    [<RequireQualifiedAccess>]
    module Specification =
        open ResultComputationExpression

        type ComparisonResult = {
            missing: Specification
            additional: Specification
        }

        type GetSpecificationError =
            | IOError of string
        type CompareError =
            | GetSpecificationError of GetSpecificationError
        type CompareDirectory = System.IO.DirectoryInfo -> Specification -> Result<ComparisonResult,CompareError>

        let private get (dir: System.IO.DirectoryInfo) =
            let rec getRec (dir: System.IO.DirectoryInfo) =
                let files = dir.EnumerateFiles() |> List.ofSeq |> List.map (fun fi -> fi.Name)
                let directories = dir.EnumerateDirectories() |> List.ofSeq |> List.map getRec
                { name = dir.Name; directories = directories; files = files; }
            try
                Ok (getRec dir)
            with
                ex -> Error (IOError ex.Message)

        let compare spec1 spec2 : Specification =
            let rec compareRec spec1 spec2 : Specification option =
                let compareDirectory dir =
                    let dirToCompareWith = spec2.directories |> List.tryFind (fun d -> d.name = dir.name)
                    match dirToCompareWith with
                    | None -> [ dir ]
                    | Some dir2 ->
                        match compareRec dir dir2 with
                        | None -> []
                        | Some spec -> [ spec ]
                let directories = spec1.directories |> List.collect compareDirectory
                let files = spec1.files |> List.filter (fun f -> spec2.files |> List.contains f |> not)
                if List.isEmpty directories && List.isEmpty files then
                    None
                else
                    Some { name = spec1.name; directories = directories; files = files; }
            match compareRec spec1 spec2 with
            | Some spec -> spec
            | None -> { name = spec1.name; directories = []; files = []; }

        let compareDirectory : CompareDirectory = fun dir spec ->
            result {
                let! dirSpec = get dir |> Result.mapError GetSpecificationError
                let missing = compare spec dirSpec
                let additional = compare dirSpec spec
                return { missing = missing; additional = additional; }
            }

type FileNew = FileNew of string
type FileOpen = FileOpen of string
type FileClosed = FileClosed of string

type ActionResult =
    | FileOpened of FileOpen

type Action =
    | CreateAction of FileNew
    | MoveAction of FileClosed * FileNew

type CreateFileError =
    | AlreadyExists

type FileCreate = System.IO.FileInfo -> Result<FileOpen,CreateFileError>

let createFile : FileCreate = fun fileNew ->
    failwith "not implemented"