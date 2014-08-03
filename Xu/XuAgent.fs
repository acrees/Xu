﻿[<AutoOpen>]
module Xu.XuAgent

open Xunit.Abstractions
open Xunit.Sdk

let messageToOutput (m:IFinishedMessage) =
    { Time = m.ExecutionTime
      Tests = m.TestsRun + m.TestsFailed + m.TestsSkipped
      Passed = m.TestsRun
      Failed = m.TestsFailed
      Skipped = m.TestsSkipped }

type XuAgent (mode:Mode) =
    let logFailures = match mode with | LogFailures -> true | Verbose -> true | _ -> false
    let verbose = match mode with | Verbose -> true | _ -> false
    let agent = MailboxProcessor<Message>.Start(fun inbox ->
        let rec loop (x, ch:AsyncReplyChannel<Output> option) = async {
            let! msg = inbox.Receive()
            match (msg, x, ch) with
                | (Result m, _, _) when (m :? TestFailed) ->
                    if logFailures then printFailure verbose (m :?> TestFailed)
                    return! loop (x, ch)
                | (Finished m, _, Some c) ->
                    messageToOutput m |> c.Reply
                | (Finished m, _, _) ->
                    return! loop ((Some (messageToOutput m)), ch)
                | (OnDone c, Some i, _) ->
                    c.Reply i
                | (OnDone c, _, _) ->
                    return! loop (x, Some c)
                | _ -> return! loop (x, ch) }
        loop (None, None) )

    member x.Result = agent.Post << Result
    member x.Finished = agent.Post << Finished
    member x.GetExitCode () = agent.PostAndAsyncReply (fun ch -> OnDone ch)