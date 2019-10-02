namespace ConfigWrapper

module Core =
    open ConfigWrapper.IO
    open ConfigWrapper.DTO
    open ConfigWrapper.List
    open ConfigWrapper.CLI

    open System
    open ResultComputationExpression

    type Application = string [] -> int
    type Direction =
        | Get
        | Set
    type GetValuesError =
        | LoadError of LoadSettingsError
    type SetValuesError =
        | LoadError of LoadSettingsError
        | SaveError of SaveSettingError
    type ApplicationError =
        | ParseError of ParseError
        | DateFormatError of string
        | SetDateError of SetValuesError
        | GetDateError of GetValuesError
    type ApplicationSuccess =
        | PrintUsage
        | ValuesGotten of Value list
        | ValuesSet of Value list
    type ApplicationAction = ValidatedArguments -> Result<ApplicationSuccess,ApplicationError>
    type SetValuesAction = SetValuesSpecification -> Result<ApplicationSuccess,SetValuesError>
    type GetValuesAction = NonEmptyList<GetValue> -> Result<ApplicationSuccess,GetValuesError>
    type GetDateAction = string -> Result<ApplicationSuccess,GetValuesError>
    type ApplicationResult = {
        returnCode: int
        consoleOutput: string list
    }
    type ApplicationFail = ApplicationError -> ApplicationResult
    type ApplicationSucceed = ApplicationSuccess -> ApplicationResult
    type ApplicationReturn = ApplicationResult -> int

    let settingsFile = "settings.json"


    let setValues : SetValuesAction = fun spec ->
        let folder (s: Settingsv2) (t: Value) =
            match t with
            | Date date -> { s with date = date }
            | Server server -> { s with server = server }
        result {
            let! settings =
                match spec.useExisting with
                | Ignore -> ``default`` |> Result.mapError SetValuesError.LoadError
                | Use -> loadSettingsOrDefault settingsFile |> Result.mapError SetValuesError.LoadError
            let settings = NonEmptyList.value spec.values |> List.fold folder settings
            do! saveSettings settingsFile settings |> Result.mapError SetValuesError.SaveError
            let valuesSet = NonEmptyList.value spec.values
            return ValuesSet valuesSet
        }

    let getValues : GetValuesAction = fun values ->
        let folder (settings: Settingsv2) (s: Value list) (t: GetValue) =
            match t with
            | GetValue.Date -> Value.Date settings.date::s
            | GetValue.Server -> Value.Server settings.server::s
        result {
            let! settings = loadSettings settingsFile |> Result.mapError GetValuesError.LoadError
            let gottenValues = values |> NonEmptyList.value |> List.fold (folder settings) []
            return ValuesGotten gottenValues
        }

    let performAction : ApplicationAction = fun vArgs ->
        match vArgs with
        | ValidatedArguments.Get values -> getValues values |> Result.mapError GetDateError
        | ValidatedArguments.Set spec -> setValues spec |> Result.mapError SetDateError

    let formatValue direction (value: Value) =
        let formatValue name value =
            match direction with
            | Direction.Get -> sprintf "%s is %s" name value
            | Direction.Set -> sprintf "%s is set to %s" name value
        match value with
        | Date date -> date.ToShortDateString() |> formatValue "Date"
        | Server server -> server |> Server.value |> formatValue "Server"

    let applicationSucceed : ApplicationSucceed = fun success ->
        let consoleOutput =
            match success with
            | PrintUsage -> [parser.PrintUsage()]
            | ValuesSet values -> values |> List.map (formatValue Direction.Set)
            | ValuesGotten values -> values |> List.map (formatValue Direction.Get)
        { returnCode = 0; consoleOutput = consoleOutput }

    let fromDtoFail (err: FromDTOError) =
        match err with
        | FromDTOError.ServerError err -> { returnCode = 7; consoleOutput = [sprintf "Could not load settings file. Error in server definition: %s" (Server.errorMessage err)]}

    let loadSettingsFail (loadSettingsError: LoadSettingsError) =
        match loadSettingsError with
        | LoadSettingsError.IOError err -> { returnCode = 3; consoleOutput = [sprintf "Could not load settings from file: %s" err] }
        | LoadSettingsError.FileFormatError err -> { returnCode = 4; consoleOutput = [sprintf "Settings file has invalid format: %s" err] }
        | LoadSettingsError.InvalidDataError err -> fromDtoFail err

    let saveSettingsFail saveSettingsError =
        match saveSettingsError with
        | SaveSettingError.IOError err -> { returnCode = 6; consoleOutput = [sprintf "Could not save settings to file: %s" err] }

    let getDateFail getDateError =
        match getDateError with
        | GetValuesError.LoadError err -> loadSettingsFail err

    let setDateFail setDateError =
        match setDateError with
        | LoadError err -> loadSettingsFail err
        | SaveError err -> saveSettingsFail err

    let validateGetArgumentErrorMessage (err: ValidateGetArgumentsError) =
        match err with
        | ValidateGetArgumentsError.Empty -> "At least one value must be specified"

    let validateSetArgumentErrorMessage (err: ValidateSetArgumentsError) =
        match err with
        | Empty _ -> "At least one value must be specified"
        | InvalidDate date -> sprintf "Invalid date format: %s" date
        | InvalidServer server -> sprintf "Invalid server name: %s" server

    let parseFail (err: CLI.ParseError) =
        match err with
        | UsageRequested usage -> { returnCode = 0; consoleOutput = [usage] }
        | ArgumentParsingError err -> { returnCode = 1; consoleOutput = [sprintf "Error in arguments: %s" err] }
        | ArgumentValidationError -> { returnCode = 2; consoleOutput = ["Invalid arguments"] }
        | GetArgumentValidationError err -> { returnCode = 8; consoleOutput = [validateGetArgumentErrorMessage err] }
        | SetArgumentValidationError err -> { returnCode = 9; consoleOutput = [validateSetArgumentErrorMessage err] }

    let applicationFail : ApplicationFail = fun error ->
        match error with
        | ParseError err -> parseFail err
        | GetDateError err -> getDateFail err
        | SetDateError err -> setDateFail err
        | DateFormatError dateString -> { returnCode = 5; consoleOutput = [sprintf "Illegal date format: %s" dateString]}

    [<EntryPoint>]
    let main : Application = fun argv ->
        let applicationResult = result {
            let! parsedArguments = parseArguments argv |> Result.mapError ApplicationError.ParseError
            let! validatedArguments = validateArguments parsedArguments |> Result.mapError ApplicationError.ParseError
            let! actionResult = performAction validatedArguments
            return actionResult
        }
        let (writer, returnValues) =
            match applicationResult with
            | Ok success -> (printfn, applicationSucceed success)
            | Error err -> (eprintfn, applicationFail err)
        returnValues.consoleOutput |> List.iter (writer "%s")
        returnValues.returnCode
