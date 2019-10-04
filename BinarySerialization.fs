module BinarySerialization

open System

[<Serializable>]
type OtherType = {
    value: int
    name: string
}

[<Serializable>]
type SomeType =
    | CaseA
    | CaseB of string
    | CaseC of DateTime * decimal
    | CaseD of OtherType

let formatter = Runtime.Serialization.Formatters.Binary.BinaryFormatter()

let serialize x =
    let stream = new IO.MemoryStream()
    formatter.Serialize(stream, x)
    stream.ToArray() |> System.Text.Encoding.UTF8.GetString