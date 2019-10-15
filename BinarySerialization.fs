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

let getSomeType n =
    match (n % 4) with
    | 0 -> CaseA
    | 1 -> CaseB "This is a case B"
    | 2 -> CaseC (DateTime.Now, 123.45m)
    | 3 -> CaseD { value = 12; name = "This is a name"; }
    | _ -> failwith "not possible"

let getSomeTypes n =
    Seq.map getSomeType [0..n]

let serializeToFile path n =
    let stream = IO.File.OpenWrite(path)
    formatter.Serialize(stream, getSomeTypes n)
    stream.Close()