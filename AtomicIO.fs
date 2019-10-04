module AtomicIO

type FinalState =
    | TempExists
    | BackupExists
    | TargetUpdated
    with
        override this.ToString() =
            match this with
            | TempExists -> "TempExists"
            | BackupExists -> "BackupExists"
            | TargetUpdated -> "TargetUpdated"

let writeAllText path (text: string) =
    try
        let fileInfo = System.IO.FileInfo(path)
        let fileInfoTemp = System.IO.FileInfo(fileInfo.FullName + ".tmp")
        let fileInfoBackup = System.IO.FileInfo(fileInfo.FullName + ".backup")
        let bytes = System.Text.Encoding.UTF8.GetBytes(text)
        use stream = System.IO.File.Create(fileInfoTemp.FullName, bytes.Length, System.IO.FileOptions.WriteThrough)
        try
            stream.Write(System.ReadOnlySpan(bytes))
            stream.Close()
            try
                fileInfoTemp.MoveTo(fileInfo.FullName)
                Ok ()
            with
                _ ->
                    try
                        fileInfoTemp.Replace(fileInfo.FullName, fileInfoBackup.FullName) |> ignore
                        try
                            fileInfoBackup.Delete()
                            Ok ()
                        with
                            _ -> Error [TargetUpdated; BackupExists; ]
                    with
                        _ -> Error [TempExists; ]
        with
            _ -> Error [TempExists; ]
    with
        _ -> Error []