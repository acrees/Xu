﻿[<RequireQualifiedAccessAttribute>]
module Xu.TestRunner

open System
open System.IO
open System.Text.RegularExpressions
open System.Reflection
open Xunit.Sdk
open Xunit.Abstractions

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

let runTests mode path = async {
    let agent = new XuAgent(mode)
    use msg =
        new DelegatingMessageSink(null, (fun x ->
            match x with
            | :? TestResultMessage as m -> agent.Result m
            | :? IFinishedMessage as m -> agent.Finished m
            | _ -> ()))
    use fx = new XunitTestFramework()
    use exec = AssemblyName.GetAssemblyName(path) |> fx.GetExecutor
    exec.RunAll(msg, XunitOptions (), XunitOptions ())
    return! agent.GetExitCode ()}

let locateAndRunTests mode path locator =
    async {
        match locator with
        | Exact -> return! runTests mode path
        | Find ->
            let! results = findDlls "" "" path |> asyncMap (runTests mode)
            return List.fold combine Output.Identity results
        | Regex r ->
            let! results = findDlls r "" path |> asyncMap (runTests mode)
            return List.fold combine Output.Identity results }
    |> Async.RunSynchronously