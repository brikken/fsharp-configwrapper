namespace ConfigWrapper

module Test =
    type Server = Server of string
    type Value =
        | Date of System.DateTime
        | Server of Server