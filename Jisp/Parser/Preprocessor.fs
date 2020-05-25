module Jisp.Parser.Preprocessor

let preprocess (src:string) =
    let stdlib =
        System.AppDomain.CurrentDomain.SetupInformation.ApplicationBase + "stdlib.jisp"
        |> System.IO.File.ReadAllText
        |> fun x -> x + "\n"
    (stdlib + src).Split '\n'
    |> Array.map (fun x -> x.TrimEnd '\r')
    |> Array.map (fun x -> x + " ;")
    |> Array.map (fun x -> 
        let i = x.IndexOf ';'
        x.[..i-1].TrimEnd ())
    |> Array.filter (not << System.String.IsNullOrWhiteSpace)
    |> Array.reduce (fun a b -> a + "\n" + b)


