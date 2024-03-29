﻿[<AutoOpen>]
module Xu.Core

open Xunit.Abstractions
open Xunit.Sdk

type XunitOptions () =
    interface ITestFrameworkOptions with
        member x.GetValue(name, defaultValue) = defaultValue
        member x.SetValue(name, value) = ()

type Output = { Time: decimal; Tests: int; Passed: int; Failed: int; Skipped: int; }
    with static member Identity = { Time = 0m; Tests = 0; Passed = 0; Failed = 0; Skipped = 0;}

type Message =
    | Result of TestResultMessage
    | Finished of TestAssemblyFinished
    | OnDone of AsyncReplyChannel<Output>

type Locator = | Exact | Find | Regex of string

type Mode =
    | Terse
    | LogFailures
    | Verbose

type Arguments = { Mode: Mode; Locator: Locator; Path: string option; }
    with static member Identity = { Mode = Terse; Locator = Exact; Path = None; }

let combine (o:Output) (p:Output) =
    { Time = o.Time + p.Time
      Tests = o.Tests + p.Tests
      Passed = o.Passed + p.Passed
      Failed = o.Failed + p.Failed
      Skipped = o.Skipped + p.Skipped }

let printFailure v (m:TestFailed) =
    printfn "Failure: %s#%s" m.TestClass.Class.Name m.TestMethod.Method.Name
    if v then
      Array.iter (fun x -> printfn "%s" x) m.Messages
      Array.iter (fun x -> printfn "%s" x) m.StackTraces
      printfn ""

let printResults (x:Output) =
    let time = if x.Time >= 0.1m then x.Time.ToString("0.0") else "< 0.1"
    let skipped = if x.Skipped > 0 then sprintf " (%d skipped)." x.Skipped else "."
    printfn "Ran %d tests in %s seconds; %d passed and %d failed%s"
            x.Tests time x.Passed x.Failed skipped

let concatMap f = List.fold (fun acc n -> f n |> List.append acc) []

let asyncMap f x =
    let rec map g y acc = async {
        match y with
        | []      -> return acc
        | (r::rs) ->
            let! z = f r
            return! map f rs (z::acc) }
    map f x []

let asyncListSingleton x = async { let! x' = x in return [x'] } 