namespace ConfigWrapper

module Core =
    open ConfigWrapper.IO
    open ConfigWrapper.DTO

    open System
    open ResultComputationExpression
    open Argu

    type Application = string [] -> int
    type UnvalidatedArgument =
        | Get_Date
        | Set_Date of date:string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Get_Date _ -> "get date from settings"
                | Set_Date _ -> "save date to settings"
    type ValidatedArguments =
        | GetDate
        | SetDate of DateTime
    type GetDateError =
        | LoadError of LoadSettingsError
    type SetDateError =
        | LoadError of LoadSettingsError
        | SaveError of SaveSettingError
    type ApplicationError =
        | UsageRequested
        | ArgumentParsingError of string
        | ParameterValidationError
        | DateFormatError of string
        | SetDateError of SetDateError
        | GetDateError of GetDateError
    type ParseArguments = string [] -> Result<UnvalidatedArgument list,ApplicationError>
    type ValidateArguments = UnvalidatedArgument list -> Result<ValidatedArguments,ApplicationError>
    type ApplicationSuccess =
        | PrintUsage
        | DateRetrieved of DateTime
        | DateSet of DateTime
    type ApplicationAction = ValidatedArguments -> Result<ApplicationSuccess,ApplicationError>
    type SetDateAction = string -> DateTime -> Result<ApplicationSuccess,SetDateError>
    type GetDateAction = string -> Result<ApplicationSuccess,GetDateError>
    type ApplicationResult = {
        returnCode: int
        consoleOutput: string
    }
    type ApplicationFail = ApplicationError -> ApplicationResult
    type ApplicationSucceed = ApplicationSuccess -> ApplicationResult
    type ApplicationReturn = ApplicationResult -> int

    let parser = ArgumentParser.Create<UnvalidatedArgument>(programName = AppDomain.CurrentDomain.FriendlyName)
    let settingsFile = "settings.json"

    let parseArguments : ParseArguments = fun args ->
        let parseResult = parser.Parse(inputs = args, raiseOnUsage = false)
        try
            if parseResult.IsUsageRequested then
                Error UsageRequested
            else        
                Ok (parseResult.GetAllResults())
        with
            ex -> Error (ArgumentParsingError ex.Message)

    let validateDateArgument = fun dateString ->
        try
            let date = DateTime.Parse(dateString)
            Some date
        with
            _ -> None

    let validateArguments : ValidateArguments = fun uvArgs ->
        match uvArgs with
        | [] -> Error UsageRequested
        | [x] ->
            match x with
            | Get_Date -> Ok GetDate
            | Set_Date dateString ->
                match validateDateArgument dateString with
                | Some date -> Ok (SetDate date)
                | None -> Error (DateFormatError dateString)
        | _ -> Error ParameterValidationError

    let setDate : SetDateAction = fun path date ->
        result {
            let! dto =
                loadSettingsOrDefault path
                |> Result.mapError SetDateError.LoadError
            let dtoNew = { dto with date = date }
            let! _ =
                saveSettings path dtoNew
                |> Result.mapError SetDateError.SaveError
            return DateSet date        
        }

    let getDate : GetDateAction = fun path ->
        result {
            let! dto =
                loadSettings path
                |> Result.mapError GetDateError.LoadError
            return (DateRetrieved dto.date)
        }

    let performAction : ApplicationAction = fun vArgs ->
        match vArgs with
        | GetDate ->
            getDate settingsFile
            |> Result.mapError GetDateError
        | SetDate date ->
            setDate settingsFile date
            |> Result.mapError SetDateError

    let formatDate (date: DateTime) =
        date.ToShortDateString()

    let applicationSucceed : ApplicationSucceed = fun success ->
        let consoleOutput =
            match success with
            | PrintUsage ->  parser.PrintUsage()
            | DateSet date -> date |> formatDate |> sprintf "Date set to %s"
            | DateRetrieved date -> date |> formatDate
        { returnCode = 0; consoleOutput = consoleOutput }

    let loadSettingsFail (loadSettingsError: LoadSettingsError) =
        match loadSettingsError with
        | LoadSettingsError.IOError err -> { returnCode = 3; consoleOutput = sprintf "Could not load settings from file: %s" err }
        | LoadSettingsError.FileFormatError err -> { returnCode = 4; consoleOutput = sprintf "Settings file has invalid format: %s" err }

    let saveSettingsFail saveSettingsError =
        match saveSettingsError with
        | SaveSettingError.IOError err -> { returnCode = 6; consoleOutput = sprintf "Could not save settings to file: %s" err }

    let getDateFail getDateError =
        match getDateError with
        | GetDateError.LoadError err -> loadSettingsFail err

    let setDateFail setDateError =
        match setDateError with
        | LoadError err -> loadSettingsFail err
        | SaveError err -> saveSettingsFail err

    let applicationFail : ApplicationFail = fun error ->
        match error with
        | UsageRequested -> { returnCode = 0; consoleOutput = parser.PrintUsage() }
        | ArgumentParsingError err -> { returnCode = 1; consoleOutput = sprintf "Error in arguments: %s" err }
        | ParameterValidationError -> { returnCode = 2; consoleOutput = "Invalid arguments" }
        | GetDateError err -> getDateFail err
        | SetDateError err -> setDateFail err
        | DateFormatError dateString -> { returnCode = 5; consoleOutput = sprintf "Illegal date format: %s" dateString}

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
        writer "%s" returnValues.consoleOutput
        returnValues.returnCode
