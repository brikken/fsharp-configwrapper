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
        | InvalidDataError of FromDTOError
    type LoadSettings = string -> Result<Settingsv2,LoadSettingsError>
    type SaveSettings = string -> Settingsv2 -> Result<unit,SaveSettingError>
    type LoadSettingsOrDefault = string -> Result<Settingsv2,LoadSettingsError>

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
            let! contents = tryReadAllText(path) |> Result.mapError LoadSettingsError.IOError
            let! dto = getAsNewestDTO contents |> Result.mapError LoadSettingsError.FileFormatError
            let! settings = fromDto dto |> Result.mapError LoadSettingsError.InvalidDataError
            return settings
        }

    let ``default`` =
        upgradeDTOv0v1Default
        |> upgradeDTOv1v2Default
        |> fromDto |> Result.mapError LoadSettingsError.InvalidDataError

    let loadSettingsOrDefault : LoadSettingsOrDefault = fun path ->
        if IO.File.Exists(path)
        then loadSettings path
        else ``default``

    let saveSettings : SaveSettings = fun path settings ->
        settings
        |> toDto
        |> serialize
        |> tryWriteAllText path |> Result.mapError SaveSettingError.IOError