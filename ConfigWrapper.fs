module ConfigWrapper

open System
open ResultComputationExpression
open Argu
open Microsoft.FSharpLu.Json

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
type ApplicationError =
    | UsageRequested
    | ArgumentParsingError of string
    | ParameterValidationError
    | DateFormatError of string
    | SetDateError of string
    | GetDateError of string
type ParseArguments = string [] -> Result<UnvalidatedArgument list,ApplicationError>
type ValidateArguments = UnvalidatedArgument list -> Result<ValidatedArguments,ApplicationError>
type ApplicationSuccess =
    | PrintUsage
    | DateRetrieved of DateTime
    | DateSet of DateTime
type ApplicationAction = ValidatedArguments -> Result<ApplicationSuccess,ApplicationError>
type SetDateAction = string -> DateTime -> Result<ApplicationSuccess,ApplicationError>
type GetDateAction = string -> Result<ApplicationSuccess,ApplicationError>
type ApplicationResult = {
    returnCode: int
    consoleOutput: string
}
type ApplicationFail = ApplicationError -> ApplicationResult
type ApplicationSucceed = ApplicationSuccess -> ApplicationResult
type ApplicationReturn = ApplicationResult -> int
type SettingsDTOv1 = {
    date: DateTime
}

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
    try
        Compact.serialize({ SettingsDTOv1.date = date }) |> fun c -> IO.File.WriteAllText(path, c)
        Ok (DateSet date)
    with
        ex -> Error (SetDateError ex.Message)

let getDate : GetDateAction = fun path ->
    try
        let (dto: SettingsDTOv1) = IO.File.ReadAllText(path) |> Compact.deserialize
        Ok (DateRetrieved dto.date)
    with
        ex -> Error (GetDateError ex.Message)

let performAction : ApplicationAction = fun vArgs ->
    match vArgs with
    | GetDate -> getDate settingsFile
    | SetDate date -> setDate settingsFile date

let formatDate (date: DateTime) =
    date.ToShortDateString()

let applicationSucceed : ApplicationSucceed = fun success ->
    let consoleOutput =
        match success with
        | PrintUsage ->  parser.PrintUsage()
        | DateSet date -> date |> formatDate |> sprintf "Date set to %s"
        | DateRetrieved date -> date |> formatDate
    { returnCode = 0; consoleOutput = consoleOutput }

let applicationFail : ApplicationFail = fun error ->
    match error with
    | UsageRequested -> { returnCode = 0; consoleOutput = parser.PrintUsage() }
    | ArgumentParsingError err -> { returnCode = 1; consoleOutput = sprintf "Error in arguments: %s" err }
    | ParameterValidationError -> { returnCode = 2; consoleOutput = "Invalid arguments" }
    | GetDateError err -> { returnCode = 3; consoleOutput = sprintf "Could not read date from file: %s" err }
    | SetDateError err -> { returnCode = 4; consoleOutput = sprintf "Could not save date to file: %s" err }
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
