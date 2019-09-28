namespace ConfigWrapper

module DTO =
    open Microsoft.FSharpLu.Json
    open System
    open ResultComputationExpression

    type SettingsDTOv1 = {
        date: DateTime
    }
    type SettingsDTOv2 = {
        date: DateTime
        server: string
    }
    type Server = private Server of string
    module Server =
        type CreateServerError =
            | Null
            | IllegalName of Char
            | TooLong
        let errorMessage err =
            match err with
            | Null -> "Null is not allowed"
            | IllegalName char -> sprintf "Illegal character: %c" char
            | TooLong -> "Name too long"
        let private idMaxlength = 128
        let private legalCharacters = [|'_'; '-'|]
        let value (Server server) = server
        let isNull server = if isNull server then Error CreateServerError.Null else Ok ()
        let isTooLong server = if String.length server > idMaxlength then Error CreateServerError.TooLong else Ok ()
        let hasIllegalName server =
            let illegalChar (server: string) = server.ToCharArray() |> Array.tryFind (fun c -> (Char.IsLetterOrDigit c || Array.contains c legalCharacters) |> not)
            match illegalChar server with
            | Some char -> Error (CreateServerError.IllegalName char)
            | None -> Ok ()
        let create server = 
            result {
                do! isNull server
                do! isTooLong server
                do! hasIllegalName server
                return Server server
            }

    type Settingsv2 = {
        date: DateTime
        server: Server
    }
    type UpgradeDTOv1v2 = string -> SettingsDTOv1 -> SettingsDTOv2
    type GetAsNewestDTO = string -> Result<SettingsDTOv2,string>
    type FromDTOError =
        | ServerError of Server.CreateServerError

    let serialize = Compact.serialize

    let upgradeDTOv0v1 date =
        { date = date }
    let upgradeDTOv1v2 : UpgradeDTOv1v2 = fun server dto1 ->
        { date = dto1.date; server = server }

    let upgradeDTOv0v1Default = upgradeDTOv0v1 DateTime.Now.Date
    let upgradeDTOv1v2Default = upgradeDTOv1v2 ""

    let getAsNewestDTO : GetAsNewestDTO = fun json ->
        try
            let (dto: SettingsDTOv2) = Compact.deserialize(json)
            Ok dto
        with _ ->
            try
                let (dto: SettingsDTOv1) = Compact.deserialize(json)
                Ok (dto |> upgradeDTOv1v2Default)
            with        
                ex -> Error ex.Message

    let fromDto (dto: SettingsDTOv2) =
        result {
            let! server = Server.create dto.server |> Result.mapError FromDTOError.ServerError
            return { Settingsv2.date = dto.date; server = server }
        }

    let toDto settings =
        { SettingsDTOv2.date = settings.date; server = Server.value settings.server }