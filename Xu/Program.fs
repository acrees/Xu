module Xu.Main

open System
open System.IO
open System.Text.RegularExpressions
open System.Reflection
open Xunit.Sdk
open Xunit.Abstractions

type Options () =
    interface ITestFrameworkOptions with
        member x.GetValue(name, defaultValue) = defaultValue
        member x.SetValue(name, value) = ()

type Locator = | Exact | Find | Regex of string

type Arguments = { Mode: Mode; Locator: Locator; Path: string option; }
    with static member Identity = { Mode = Terse; Locator = Exact; Path = None; }

let concatMap f = List.fold (fun acc n -> f n |> List.append acc) []

let rec findDlls pattern cwd path = 
    let dllsInDir =
        Directory.EnumerateFiles path
        |> List.ofSeq
        |> List.filter (fun x ->
            Path.GetExtension x = ".dll"
            && (pattern = "" || Regex.IsMatch(x, pattern)))
        |> List.map (Path.GetFullPath)
    Directory.EnumerateDirectories path
    |> List.ofSeq
    |> concatMap (findDlls pattern (Path.Combine(cwd, path)))
    |> List.append dllsInDir

let rec parseArgs acc args =
    match args with
    | [] -> acc
    | ("-l"::xs) -> parseArgs { acc with Mode = LogFailures } xs
    | ("-v"::xs) -> parseArgs { acc with Mode = Verbose } xs
    | ("-f"::xs) -> parseArgs { acc with Locator = Find } xs
    | ("-r"::r::xs) -> parseArgs { acc with Locator = Regex r } xs
    | (x::xs) -> parseArgs { acc with Path = Some x } xs

let handleRunningMessage (agent:XuAgent) (x:IMessageSinkMessage) =
    match x with
    | :? TestResultMessage as m -> agent.Result m
    | :? IFinishedMessage as m -> agent.Finished m
    | _ -> ()

let runTests mode path = async {
    let agent = new XuAgent(mode)
    use msg = new DelegatingMessageSink(null, (fun x -> handleRunningMessage agent x))
    use fx = new XunitTestFramework()
    let asn = AssemblyName.GetAssemblyName(path)
    use exec = fx.GetExecutor asn
    exec.RunAll(msg, Options (), Options ())
    return! agent.GetExitCode ()}

let printResultsAndExit (x:Output) =
    let time = x.Time.ToString("0.000")
    printfn "Ran %d tests in %s seconds; %d passed and %d failed."
            x.Tests time x.Passed x.Failed
    if x.Failed = 0 then 0 else -1

let rec asyncMap f s = async {
    match s with
    | [] -> return []
    | (r::rs) ->
        let! x = f r
        let! xs = asyncMap f rs
        return x::xs }

[<EntryPoint>]
let main argv =
        let args = argv |> List.ofArray |> parseArgs Arguments.Identity
        match args.Path with
        | None -> printfn "Please enter a path"; -1
        | Some p ->
            async {
                match args.Locator with
                | Exact -> return! runTests args.Mode p
                | Find ->
                    let dlls = findDlls "" "" p
                    let! results = asyncMap (runTests args.Mode) dlls
                    return List.fold combine Output.Identity results
                | Regex r ->
                    let! dlls = findDlls r "" p |> asyncMap (runTests args.Mode)
                    return List.fold combine Output.Identity dlls }
            |> Async.RunSynchronously
            |> printResultsAndExit