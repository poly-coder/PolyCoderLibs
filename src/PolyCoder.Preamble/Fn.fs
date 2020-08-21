namespace PolyCoder

open System.Threading.Tasks

type Fn<'a, 'b> = 'a -> 'b

[<RequireQualifiedAccess>]
module Fn =
  ()

type Sink<'a> = Fn<'a, unit>

module Sink =
  let protect sink a = try sink a with _ -> ()

  let ofAsync sink fn =
    async {
      let! result = fn()
      (protect sink) result
    } |> Async.Start

  let toAsync (processFn: Sink<Sink<'value>>) =
    let source = TaskCompletionSource()
    let sink value = source.TrySetResult(value) |> ignore
    (protect processFn) sink
    source.Task |> Async.AwaitTask

  let postToMailbox (fn: (Sink<'result>) -> 'command) (mailbox: MailboxProcessor<'command>) =
    async {
      let! value = mailbox.PostAndAsyncReply(fun reply -> fn reply.Reply)
      return value
    }



type ResultSink<'a> = Sink<Result<'a, exn>>

module ResultSink =
  let ofAsync sink fn =
    async {
      try
        let! result = fn()
        (Sink.protect sink) (Ok result)
      with
        exn -> (Sink.protect sink) (Error exn)
    } |> Async.Start

  let toAsync (processFn: Sink<ResultSink<'value>>) =
    let source = TaskCompletionSource()
    let sink = function
      | Ok value -> source.TrySetResult(value) |> ignore
      | Error exn -> source.TrySetException(exn: exn) |> ignore
    (Sink.protect processFn) sink
    source.Task |> Async.AwaitTask

  let postToMailbox (fn: (ResultSink<'result>) -> 'command) (mailbox: MailboxProcessor<'command>) =
    async {
      match! mailbox.PostAndAsyncReply(fun reply -> fn reply.Reply) with
      | Ok value -> return value
      | Error exn -> return Exn.reraise exn
    }
