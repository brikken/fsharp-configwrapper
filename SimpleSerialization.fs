module SimpleSerialization

open System.Xml
open System.Xml.Serialization

[<XmlRoot("MyData")>]
[<CLIMutable>]
type MyData = {
    [<XmlElement("Text")>]
    text: string
    [<XmlElement("Value")>]
    value: int
    [<XmlElement("Items")>]
    items: SubType list
}

and SubType =
    | [<XmlElement("OptionOne")>] OptionOne
    | [<XmlElement("OptionTwo")>] OptionTwo of string * int

let getXml myData =
    let x = XmlDocument()
    let getSimpleElement name value =
        let element = x.CreateElement(name)
        element.InnerText <- value
        element
    x.AppendChild(
        let m = x.CreateElement("MyData")
        getSimpleElement "Text" myData.text |> m.AppendChild |> ignore
        getSimpleElement "Value" (sprintf "%i" myData.value) |> m.AppendChild |> ignore
        m.AppendChild(
            let items = x.CreateElement("Items")
            let getXml subType =
                match subType with
                    | SubType.OptionOne -> x.CreateElement("OptionOne")
                    | SubType.OptionTwo (a, b) ->
                        x.CreateElement("OptionTwo")
                        |> fun z ->
                            z.SetAttribute("a", a)
                            z.SetAttribute("b", sprintf "%i" b)
                            z
            List.iter (getXml >> items.AppendChild >> ignore) myData.items
            items) |> ignore
        m
    ) |> ignore
    x.OuterXml

let getSerializerXml (myData:MyData) =
    let serializer = XmlSerializer(typeof<MyData>)
    let writer = new System.IO.StringWriter()
    serializer.Serialize(writer, myData)
    writer.ToString()