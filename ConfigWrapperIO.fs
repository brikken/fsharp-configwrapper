namespace ConfigWrapper

module IO =
    open System
    open ResultComputationExpression
    open ConfigWrapper.DTO

    type SaveSettingError =
        | IOError of string
    type LoadSettingsError =
        | IOError of string
        | FileFormatError of string
    type LoadSettings = string -> Result<SettingsDTOv2,LoadSettingsError>
    type SaveSettings = string -> SettingsDTOv2 -> Result<unit,SaveSettingError>
    type LoadSettingsOrDefault = string -> Result<SettingsDTOv2,LoadSettingsError>

    let tryReadAllText path =
        try
            IO.File.ReadAllText(path) |> Ok
        with
            ex -> Error ex.Message

    let tryWriteAllText path contents =
        try
            IO.File.WriteAllText(path, contents)
            Ok ()
        with
            ex -> Error ex.Message


    let loadSettings : LoadSettings = fun path ->
        result {
            let! contents =
                tryReadAllText(path)
                |> Result.mapError LoadSettingsError.IOError
            let! dto =
                getAsNewestDTO contents
                |> Result.mapError LoadSettingsError.FileFormatError
            return dto
        }

    let loadSettingsOrDefault : LoadSettingsOrDefault = fun path ->
        if IO.File.Exists(path)
        then loadSettings path
        else Ok (upgradeDTOv0v1Default |> upgradeDTOv1v2Default)

    let saveSettings : SaveSettings = fun path dto ->
        serialize dto
        |> tryWriteAllText path
        |> Result.mapError SaveSettingError.IOError