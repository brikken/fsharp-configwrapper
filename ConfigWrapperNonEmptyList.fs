namespace ConfigWrapper

module List =
    type NonEmptyList<'a> = private NonEmptyList of 'a list
    module NonEmptyList =
        type CreateError =
            | Empty
        let create xs =
            if List.isEmpty xs
            then Error Empty
            else Ok (NonEmptyList xs)
        let value (NonEmptyList neList) = neList