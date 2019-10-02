namespace ConfigWrapper

module CLI =
    open ConfigWrapper.List
    open ConfigWrapper.DTO
    open Argu

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

    type ValidateGetArgumentsError =
        | Empty
    type ValidateSetArgumentsError =
        | Empty of NonEmptyList.CreateError
        | InvalidDate of string
        | InvalidServer of string

    type ParseError =
        | UsageRequested of string
        | ArgumentParsingError of string
        | ArgumentValidationError
        | GetArgumentValidationError of ValidateGetArgumentsError
        | SetArgumentValidationError of ValidateSetArgumentsError
    
    type Existing =
        | Use
        | Ignore
    type GetValue =
        | Date
        | Server
    type Value =
        | Date of System.DateTime
        | Server of Server
    type SetValuesSpecification = {
        values: NonEmptyList<Value>
        useExisting: Existing
    }
    type ValidatedArguments =
        | Get of NonEmptyList<GetValue>
        | Set of SetValuesSpecification

    type ParseArguments = string [] -> Result<UnvalidatedArgument list,ParseError>
    type ValidateArguments = UnvalidatedArgument list -> Result<ValidatedArguments,ParseError>
    type ValidateGetArguments = UnvalidatedArgumentGet list -> Result<ValidatedArguments,ValidateGetArgumentsError>
    type ValidateSetArguments = UnvalidatedArgumentSet list -> Result<ValidatedArguments,ValidateSetArgumentsError>

    let parser = ArgumentParser.Create<UnvalidatedArgument>(programName = System.AppDomain.CurrentDomain.FriendlyName)

    let parseArguments : ParseArguments = fun args ->
        try
            let parseResult = parser.Parse(inputs = args, raiseOnUsage = true)
            Ok (parseResult.GetAllResults())
        with
            :? Argu.ArguParseException as ex -> // Error (ArgumentParsingError ex.Message)
                match ex.ErrorCode with
                | ErrorCode.HelpText -> Error (UsageRequested ex.Message)
                | _ -> Error (ParseError.ArgumentParsingError ex.Message)

    let validateDateArgument = fun dateString ->
        try
            let date = System.DateTime.Parse(dateString)
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
                |> Result.mapError ParseError.GetArgumentValidationError
            | UnvalidatedArgument.Set uvSetArgs ->
                uvSetArgs.GetAllResults()
                |> validateSetArguments
                |> Result.mapError ParseError.SetArgumentValidationError
        | _ -> Error ArgumentValidationError
