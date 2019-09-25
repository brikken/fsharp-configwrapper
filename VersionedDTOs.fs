module VersionedDTOs

type DTO1 = {
    name: string
    age: int
    boss: string option
}

type DTO2 = {
    name: string
    age: int
    boss: string list
}

let getDTO2 (dto1: DTO1) =
    { DTO2.name = dto1.name; age = dto1.age; boss = match dto1.boss with None -> [] | Some boss -> [boss] }