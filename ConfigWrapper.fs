namespace ConfigWrapper

module Core =
    open ConfigWrapper.IO
    open ConfigWrapper.DTO
    open ConfigWrapper.List

    open System
    open ResultComputationExpression
    open Argu

    type Application = string [] -> int
    type UnvalidatedArgumentGet =
        | [<CliPrefix(CliPrefix.None); Unique>] Date
        | [<CliPrefix(CliPrefix.None); Unique>] Server
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Date -> "Get date from settings"
                | Server -> "Get server from settings"
    type UnvalidatedArgumentSet =
        | [<CliPrefix(CliPrefix.None); Unique>] Date of string
        | [<CliPrefix(CliPrefix.None); Unique>] Server of string
        | [<CliPrefix(CliPrefix.None); Unique; CustomCommandLine("ignore-existing")>] IgnoreExisting of bool
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Date _ -> "Save date to settings"
                | IgnoreExisting _ -> "Ignore existing settings"
                | Server _ -> "Save server to settings"
    type UnvalidatedArgument =
        | [<CliPrefix(CliPrefix.None)>] Get of ParseResults<UnvalidatedArgumentGet>
        | [<CliPrefix(CliPrefix.None)>] Set of ParseResults<UnvalidatedArgumentSet>
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Get _ -> "Get values from settings"
                | Set _ -> "Save values to settings"
    type Direction =
        | Get
        | Set
    type Existing =
        | Use
        | Ignore
    type GetValue =
        | Date
        | Server
    type Value =
        | Date of DateTime
        | Server of Server
    type SetValuesSpecification = {
        values: NonEmptyList<Value>
        useExisting: Existing
    }
    type ValidatedArguments =
        | Get of NonEmptyList<GetValue>
        | Set of SetValuesSpecification
    type GetValuesError =
        | LoadError of LoadSettingsError
    type SetValuesError =
        | LoadError of LoadSettingsError
        | SaveError of SaveSettingError
    type ValidateGetArgumentsError =
        | Empty
    type ValidateSetArgumentsError =
        | Empty of NonEmptyList.CreateError
        | InvalidDate of string
        | InvalidServer of string
    type ApplicationError =
        | UsageRequested of string
        | ArgumentParsingError of string
        | ArgumentValidationError
        | GetArgumentValidationError of ValidateGetArgumentsError
        | SetArgumentValidationError of ValidateSetArgumentsError
        | DateFormatError of string
        | SetDateError of SetValuesError
        | GetDateError of GetValuesError
    type ParseArguments = string [] -> Result<UnvalidatedArgument list,ApplicationError>
    type ValidateArguments = UnvalidatedArgument list -> Result<ValidatedArguments,ApplicationError>
    type ValidateGetArguments = UnvalidatedArgumentGet list -> Result<ValidatedArguments,ValidateGetArgumentsError>
    type ValidateSetArguments = UnvalidatedArgumentSet list -> Result<ValidatedArguments,ValidateSetArgumentsError>
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

    let parser = ArgumentParser.Create<UnvalidatedArgument>(programName = AppDomain.CurrentDomain.FriendlyName)
    let settingsFile = "settings.json"

    let parseArguments : ParseArguments = fun args ->
        try
            let parseResult = parser.Parse(inputs = args, raiseOnUsage = true)
            Ok (parseResult.GetAllResults())
        with
            :? Argu.ArguParseException as ex -> // Error (ArgumentParsingError ex.Message)
                match ex.ErrorCode with
                | ErrorCode.HelpText -> Error (UsageRequested ex.Message)
                | _ -> Error (ApplicationError.ArgumentParsingError ex.Message)

    let validateDateArgument = fun dateString ->
        try
            let date = DateTime.Parse(dateString)
            Some date
        with
            _ -> None

    let validateGetArguments : ValidateGetArguments = fun uvGetArgs ->
        let mapArg arg =
            match arg with
            | UnvalidatedArgumentGet.Date -> GetValue.Date
            | UnvalidatedArgumentGet.Server -> GetValue.Server
        let vGetArgs = List.map mapArg uvGetArgs |> NonEmptyList.create
        match vGetArgs with
        | Error NonEmptyList.CreateError.Empty -> Error ValidateGetArgumentsError.Empty
        | Ok neArgs -> Ok (Get neArgs)

    let validateSetArguments : ValidateSetArguments = fun uvSetArgs ->
        let folder s (t: UnvalidatedArgumentSet) =
            match s with
            | Error err -> Error err
            | Ok state ->
                match t with
                | UnvalidatedArgumentSet.Date date ->
                    match validateDateArgument date with
                    | None -> Error (ValidateSetArgumentsError.InvalidDate date)
                    | Some vDate -> Ok (Value.Date vDate::fst state, snd state)
                | UnvalidatedArgumentSet.Server server ->
                    match Server.create server with
                    | Ok server -> Ok (Value.Server server::fst state, snd state)
                    | Error err -> Error (ValidateSetArgumentsError.InvalidServer server)
                | IgnoreExisting true -> Ok (fst state, Ignore)
                | IgnoreExisting false -> Ok (fst state, Use)
        let vSetArgs = List.fold folder (Ok ([], Use)) uvSetArgs
        match vSetArgs with
        | Error err -> Error err
        | Ok (values, existing) ->
            match NonEmptyList.create values with
            | Error err -> Error err |> Result.mapError ValidateSetArgumentsError.Empty
            | Ok neValues -> Ok (Set { values = neValues; useExisting = existing })

    let validateArguments : ValidateArguments = fun uvArgs ->
        match uvArgs with
        | [x] ->
            match x with
            | UnvalidatedArgument.Get uvGetArgs ->
                uvGetArgs.GetAllResults()
                |> validateGetArguments
                |> Result.mapError ApplicationError.GetArgumentValidationError
            | UnvalidatedArgument.Set uvSetArgs ->
                uvSetArgs.GetAllResults()
                |> validateSetArguments
                |> Result.mapError ApplicationError.SetArgumentValidationError
        | _ -> Error ArgumentValidationError

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
        | Get values -> getValues values |> Result.mapError GetDateError
        | Set spec -> setValues spec |> Result.mapError SetDateError

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

    let applicationFail : ApplicationFail = fun error ->
        match error with
        | UsageRequested usage -> { returnCode = 0; consoleOutput = [usage] }
        | ArgumentParsingError err -> { returnCode = 1; consoleOutput = [sprintf "Error in arguments: %s" err] }
        | ArgumentValidationError -> { returnCode = 2; consoleOutput = ["Invalid arguments"] }
        | GetDateError err -> getDateFail err
        | SetDateError err -> setDateFail err
        | DateFormatError dateString -> { returnCode = 5; consoleOutput = [sprintf "Illegal date format: %s" dateString]}
        | GetArgumentValidationError err -> { returnCode = 8; consoleOutput = [validateGetArgumentErrorMessage err] }
        | SetArgumentValidationError err -> { returnCode = 9; consoleOutput = [validateSetArgumentErrorMessage err] }

    [<EntryPoint>]
    let main : Application = fun argv ->
        let applicationResult = result {
            let! parsedArguments = parseArguments argv
            let! validatedArguments = validateArguments parsedArguments
            let! actionResult = performAction validatedArguments
            return actionResult
        }
        let (writer, returnValues) =
            match applicationResult with
            | Ok success -> (printfn, applicationSucceed success)
            | Error err -> (eprintfn, applicationFail err)
        returnValues.consoleOutput |> List.iter (writer "%s")
        returnValues.returnCode
