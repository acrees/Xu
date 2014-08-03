module Xu.Main

open Xunit.Abstractions

let rec parseArgs acc args =
    match args with
    | []            -> acc
    | ("-l"::xs)    -> parseArgs { acc with Mode = LogFailures } xs
    | ("-v"::xs)    -> parseArgs { acc with Mode = Verbose } xs
    | ("-f"::xs)    -> parseArgs { acc with Locator = Find } xs
    | ("-r"::r::xs) -> parseArgs { acc with Locator = Regex r } xs
    | (x::xs)       -> parseArgs { acc with Path = Some x } xs

[<EntryPoint>]
let main argv =
    let args = argv |> List.ofArray |> parseArgs Arguments.Identity
    match args.Path with
    | None   -> printfn "Please enter a path"; -1
    | Some p ->
        let result = TestRunner.locateAndRunTests args.Mode p args.Locator
        printResults result
        if result.Failed = 0 then 0 else -1