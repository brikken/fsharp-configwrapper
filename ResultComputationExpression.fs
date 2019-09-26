module ResultComputationExpression

open System

type ResultBuilder() =
    member __.Map(f, x) = Result.map f x
    member __.Return(x) = Ok x
    member __.Bind(x, f) = Result.bind f x
    member __.Zero() = Ok []

let result = ResultBuilder()
