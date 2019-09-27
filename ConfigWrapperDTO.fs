namespace ConfigWrapper

module DTO =
    open Microsoft.FSharpLu.Json
    open System

    type SettingsDTOv1 = {
        date: DateTime
    }
    type SettingsDTOv2 = {
        date: DateTime
        server: string
    }
    type UpgradeDTOv1v2 = string -> SettingsDTOv1 -> SettingsDTOv2
    type GetAsNewestDTO = string -> Result<SettingsDTOv2,string>

    let tryDeserializeV1 json =
        try
            let (dto: SettingsDTOv1) = Compact.deserialize json
            Ok dto
        with
            ex -> Error ex.Message

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
