module SimpleSerialization

open System.Xml
open System.Xml.Serialization

type SubType =
    | OptionOne
    | OptionTwo of string * int
    | OptionThree of MyField: float

type MyData = {
    text: string
    value: int
    items: SubType list
}

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
                    | SubType.OptionThree myField ->
                        x.CreateElement("OptionThree")
                        |> fun z ->
                            z.SetAttribute("MyField", sprintf "%f" myField)
                            z
            List.iter (getXml >> items.AppendChild >> ignore) myData.items
            items) |> ignore
        m
    ) |> ignore
    x.OuterXml
