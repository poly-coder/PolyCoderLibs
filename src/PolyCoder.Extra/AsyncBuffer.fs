namespace PolyCoder.Extra.Collections

open System.Collections.Generic
open FSharp.Control
open PolyCoder

type AsyncGetResult<'v> =
  | ValueWasGet of 'v
  | GetBufferIsFull

type AsyncPutResult =
  | ValueWasPut
  | PutBufferIsFull


type IAsyncGetter<'v> =
  abstract get: unit -> Async<AsyncGetResult<'v>>

type IAsyncPutter<'v> =
  abstract put: 'v -> Async<AsyncPutResult>

type IAsyncBuffer<'v> =
  inherit IAsyncPutter<'v>
  inherit IAsyncGetter<'v>


type AsyncGetter<'v> = {
  get: unit -> Async<AsyncGetResult<'v>>
}

type AsyncPutter<'v> = {
  put: 'v -> Async<AsyncPutResult>
}

type AsyncBuffer<'v> = {
  get: unit -> Async<AsyncGetResult<'v>>
  put: 'v -> Async<AsyncPutResult>
}


type AsyncBufferCommand<'v> =
  | Get of ResultSink<AsyncGetResult<'v>>
  | Put of 'v * ResultSink<AsyncPutResult>

type AsyncGetterSink<'v> = Sink<ResultSink<AsyncGetResult<'v>>>
type AsyncPutterSink<'v> = Sink<'v * ResultSink<AsyncPutResult>>
type AsyncBufferSink<'v> = Sink<AsyncBufferCommand<'v>>


module IAsyncGetter =
  let ofRecord (record: AsyncGetter<'v>) : IAsyncGetter<'v> =
    { new IAsyncGetter<'v> with
      member _.get() = record.get() }

  let toRecord (instance: IAsyncGetter<'v>) : AsyncGetter<'v> =
    { get = instance.get }

  let ofSink (getterSink: AsyncGetterSink<'v>) : IAsyncGetter<'v> =
    { new IAsyncGetter<'v> with
      member _.get() = ResultSink.toAsync getterSink }

  let toSink (instance: IAsyncGetter<'v>) : AsyncGetterSink<'v> =
    fun sink -> ResultSink.ofAsync sink instance.get

  let bindValue fn (getter: IAsyncGetter<'a>) : IAsyncGetter<'b> =
    { new IAsyncGetter<'b> with
      member _.get() = async {
        match! getter.get() with
        | GetBufferIsFull -> return GetBufferIsFull
        | ValueWasGet value ->
          let! value' = fn(value)
          return ValueWasGet(value')
      }}

  let mapValue fn = bindValue (fn >> async.Return)


module IAsyncPutter =
  let ofRecord (record: AsyncPutter<'v>) : IAsyncPutter<'v> =
    { new IAsyncPutter<'v> with
      member _.put v = record.put v }

  let toRecord (instance: IAsyncPutter<'v>) : AsyncPutter<'v> =
    { put = instance.put }

  let ofSink (putterSink: AsyncPutterSink<'v>) : IAsyncPutter<'v> =
    { new IAsyncPutter<'v> with
      member _.put v = ResultSink.toAsync (fun sink -> putterSink(v, sink)) }

  let toSink (instance: IAsyncPutter<'v>) : AsyncPutterSink<'v> =
    fun (v, sink) -> ResultSink.ofAsync sink (fun () -> instance.put v)

  let bindValue fn (putter: IAsyncPutter<'a>) : IAsyncPutter<'b> =
    { new IAsyncPutter<'b> with
      member _.put(value) = async {
        let! value' = fn value
        return! putter.put value'
      }}

  let mapValue fn = bindValue (fn >> async.Return)


module AsyncGetter =
  let ofInterface = IAsyncGetter.toRecord
  let toInterface = IAsyncGetter.ofRecord
  let ofSink<'v> : (AsyncGetterSink<'v> -> AsyncGetter<'v>) = IAsyncGetter.ofSink >> ofInterface
  let toSink<'v> : (AsyncGetter<'v> -> AsyncGetterSink<'v>) = toInterface >> IAsyncGetter.toSink

  let bindValue fn (getter: AsyncGetter<'a>) : AsyncGetter<'b> =
    { 
      get = fun () -> async {
        match! getter.get() with
        | GetBufferIsFull -> return GetBufferIsFull
        | ValueWasGet value ->
          let! value' = fn(value)
          return ValueWasGet(value')
      }
    }

  let mapValue fn = bindValue (fn >> async.Return)


module AsyncPutter =
  let ofInterface = IAsyncPutter.toRecord
  let toInterface = IAsyncPutter.ofRecord
  let ofSink<'v> : (AsyncPutterSink<'v> -> AsyncPutter<'v>) = IAsyncPutter.ofSink >> ofInterface
  let toSink<'v> : (AsyncPutter<'v> -> AsyncPutterSink<'v>) = toInterface >> IAsyncPutter.toSink

  let bindValue fn (putter: AsyncPutter<'a>) : AsyncPutter<'b> =
    { 
      put = fun value -> async {
        let! value' = fn value
        return! putter.put value'
      }
    }

  let mapValue fn = bindValue (fn >> async.Return)


module AsyncGetterSink =
  let ofInterface = IAsyncGetter.toSink
  let toInterface = IAsyncGetter.ofSink
  let ofRecord<'v> : (AsyncGetter<'v> -> AsyncGetterSink<'v>) = IAsyncGetter.ofRecord >> ofInterface
  let toRecord<'v> : (AsyncGetterSink<'v> -> AsyncGetter<'v>) = toInterface >> IAsyncGetter.toRecord

  let bindValue fn (getter: AsyncGetterSink<'a>) : AsyncGetterSink<'b> =
    fun sink -> getter(fun result ->
      match result with
      | Ok(GetBufferIsFull) ->
        sink(Ok(GetBufferIsFull))
      | Ok(ValueWasGet value) ->
        async {
          let! value' = fn(value)
          sink(Ok(ValueWasGet(value')))
        } |> Async.Start
      | Error(exn) ->
        sink(Error(exn)))

  let mapValue fn (getter: AsyncGetterSink<'a>) : AsyncGetterSink<'b> =
     fun sink -> getter(fun result ->
       match result with
       | Ok(GetBufferIsFull) ->
         sink(Ok(GetBufferIsFull))
       | Ok(ValueWasGet value) ->
           sink(Ok(ValueWasGet(fn value)))
       | Error(exn) ->
         sink(Error(exn)))



module AsyncPutterSink =
  let ofInterface = IAsyncPutter.toSink
  let toInterface = IAsyncPutter.ofSink
  let ofRecord<'v> : (AsyncPutter<'v> -> AsyncPutterSink<'v>) = IAsyncPutter.ofRecord >> ofInterface
  let toRecord<'v> : (AsyncPutterSink<'v> -> AsyncPutter<'v>) = toInterface >> IAsyncPutter.toRecord

  let bindValue fn (putter: AsyncPutterSink<'a>) : AsyncPutterSink<'b> =
    fun (value, sink) -> 
      async {
        let! value' = fn value
        putter(value', sink)
      } |> Async.Start

  let mapValue fn (putter: AsyncPutterSink<'a>) : AsyncPutterSink<'b> =
    fun (value, sink) -> putter(fn value, sink)


module IAsyncBuffer =
  let combine (getter: IAsyncGetter<'v>) (putter: IAsyncPutter<'v>) =
    { new IAsyncBuffer<'v> with
      member _.get() = getter.get()
      member _.put v = putter.put v }

  let split (buffer: IAsyncBuffer<'v>) : IAsyncGetter<'v> * IAsyncPutter<'v> =
    { new IAsyncGetter<'v> with
      member _.get() = buffer.get() },
    { new IAsyncPutter<'v> with
      member _.put v = buffer.put v }

  let ofRecord (record: AsyncBuffer<'v>) : IAsyncBuffer<'v> =
    { new IAsyncBuffer<'v> with
      member _.get() = record.get()
      member _.put v = record.put v }

  let toRecord (instance: IAsyncBuffer<'v>) : AsyncBuffer<'v> =
    { get = instance.get
      put = instance.put }

  let ofSink (bufferSink: AsyncBufferSink<'v>) : IAsyncBuffer<'v> =
    { new IAsyncBuffer<'v> with
      member _.get() = ResultSink.toAsync (fun sink -> bufferSink(Get(sink)))
      member _.put v = ResultSink.toAsync (fun sink -> bufferSink(Put(v, sink)))}

  let toSink (instance: IAsyncBuffer<'v>) : AsyncBufferSink<'v> =
    function
    | Get sink -> ResultSink.ofAsync sink instance.get
    | Put (v, sink) -> ResultSink.ofAsync sink (fun () -> instance.put v)

  let bindValue fnIn fnOut (buffer: IAsyncBuffer<'a>) : IAsyncBuffer<'b> =
    { new IAsyncBuffer<'b> with
      member _.get() = async {
        match! buffer.get() with
        | GetBufferIsFull ->
          return GetBufferIsFull
        | ValueWasGet value ->
          let! value' = fnOut(value)
          return ValueWasGet(value')
      }
      member _.put(value) = async {
        let! value' = fnIn value
        return! buffer.put value'
      }
    }

  let mapValue fnIn fnOut = bindValue (fnIn >> async.Return) (fnOut >> async.Return)


module AsyncBuffer =
  let combine (getter: AsyncGetter<'v>) (putter: AsyncPutter<'v>) : AsyncBuffer<'v> =
    { get = getter.get
      put = putter.put }

  let split (buffer: AsyncBuffer<'v>) : AsyncGetter<'v> * AsyncPutter<'v> =
    { get = buffer.get },
    { put = buffer.put }

  let ofInterface = IAsyncBuffer.toRecord
  let toInterface = IAsyncBuffer.ofRecord
  let ofSink<'v> : (AsyncBufferSink<'v> -> AsyncBuffer<'v>) = IAsyncBuffer.ofSink >> ofInterface
  let toSink<'v> : (AsyncBuffer<'v> -> AsyncBufferSink<'v>) = toInterface >> IAsyncBuffer.toSink

  let bindValue fnIn fnOut (buffer: AsyncBuffer<'a>) : AsyncBuffer<'b> =
    { 
      get = fun () -> async {
        match! buffer.get() with
        | GetBufferIsFull ->
          return GetBufferIsFull
        | ValueWasGet value ->
          let! value' = fnOut(value)
          return ValueWasGet(value')
      }
      put = fun (value) -> async {
        let! value' = fnIn value
        return! buffer.put value'
      }
    }

  let mapValue fnIn fnOut = bindValue (fnIn >> async.Return) (fnOut >> async.Return)


module AsyncBufferSink =
  let combine (getter: AsyncGetterSink<'v>) (putter: AsyncPutterSink<'v>) : AsyncBufferSink<'v> =
    function
    | Get sink -> getter sink
    | Put (v, sink) -> putter (v, sink)

  let split (buffer: AsyncBufferSink<'v>) : AsyncGetterSink<'v> * AsyncPutterSink<'v> =
    (fun sink -> buffer(Get sink)),
    (fun (v, sink) -> buffer(Put(v, sink)))

  let ofInterface = IAsyncBuffer.toSink
  let toInterface = IAsyncBuffer.ofSink
  let ofRecord<'v> : (AsyncBuffer<'v> -> AsyncBufferSink<'v>) = IAsyncBuffer.ofRecord >> ofInterface
  let toRecord<'v> : (AsyncBufferSink<'v> -> AsyncBuffer<'v>) = toInterface >> IAsyncBuffer.toRecord

  let bindValue fnIn fnOut (buffer: AsyncBufferSink<'a>) : AsyncBufferSink<'b> =
    function
    | Get sink ->
        buffer(Get(fun result ->
          match result with
          | Ok(GetBufferIsFull) ->
            sink(Ok(GetBufferIsFull))
          | Ok(ValueWasGet value') ->
            async {
              let! value = fnOut(value')
              sink(Ok(ValueWasGet value))
            } |> Async.Start
          | Error e ->
            sink(Error e)
        ))
    | Put (value, sink) ->
      async {
        let! value' = fnIn value
        buffer(Put(value', sink))
      } |> Async.Start

  let mapValue fnIn fnOut = bindValue (fnIn >> async.Return) (fnOut >> async.Return)


module BufferMailbox =
  type Options =
    {
      bufferSize: int
      getBufferSize: int option
      putBufferSize: int option
    }

  let withBufferSize n = {
    bufferSize = n
    getBufferSize = None
    putBufferSize = None
  }

  let withGetBufferSize n options = { options with getBufferSize = Some n }
  let withPutBufferSize n options = { options with putBufferSize = Some n }

  type Command<'a> =
    | PutValue of 'a * AsyncReplyChannel<Result<AsyncPutResult, exn>>
    | GetValue of AsyncReplyChannel<Result<AsyncGetResult<'a>, exn>>

  let create (options: Options) : MailboxProcessor<Command<'a>> =
    MailboxProcessor.Start (fun mb ->
      let putters = Queue<'a * AsyncReplyChannel<Result<AsyncPutResult, exn>>>()
      let values = Queue<'a>(options.bufferSize)
      let getters = Queue<AsyncReplyChannel<Result<AsyncGetResult<'a>, exn>>>()

      let rec loop () = async {
        let! cmd = mb.Receive()

        match cmd with
        | PutValue (value, putReply) ->
          try
            if values.Count < options.bufferSize then
              // If there is space available in the values buffer, put the value immediately
              values.Enqueue(value)
              putReply.Reply(Ok(ValueWasPut))
            elif getters.Count > 0 then
              // If there is a getter waiting, then the buffer has size 0, so give the value directly to the first getter
              let getReply = getters.Dequeue()
              getReply.Reply(Ok(ValueWasGet value))
              putReply.Reply(Ok(ValueWasPut))
            else
              // Check if there is space to put the operation in standBy
              match options.putBufferSize with
              | None ->
                putters.Enqueue(value, putReply)

              | Some size when size > putters.Count ->
                putters.Enqueue(value, putReply)

              | Some _ ->
                putReply.Reply(Ok(PutBufferIsFull))
          with exn -> putReply.Reply(Error(exn))

        | GetValue getReply ->
          try
            if values.Count > 0 then
              // There are values in the buffer, take one and leave
              let value = values.Dequeue()
              getReply.Reply(Ok(ValueWasGet value))
            elif putters.Count > 0 then
              // If there is a putter waiting, then the buffer has size 0, so take the value directly from the first putter
              let value, putReply = putters.Dequeue()
              putReply.Reply(Ok(ValueWasPut))
              getReply.Reply(Ok(ValueWasGet value))
            else
              // Check if there is space to put the operation in standBy
              match options.getBufferSize with
              | None ->
                getters.Enqueue(getReply)

              | Some size when size > putters.Count ->
                getters.Enqueue(getReply)

              | Some _ ->
                getReply.Reply(Ok(GetBufferIsFull))
          with exn -> getReply.Reply(Error(exn))

        return! loop ()
      }

      loop()
    )

  let toSink (mailbox: MailboxProcessor<Command<_>>) : AsyncBufferSink<_> =
    function
    | Get sink ->
      Async.Start(async {
        let! result = mailbox.PostAndAsyncReply(fun reply -> GetValue(reply))
        sink(result)
      })

    | Put (v, sink) ->
      Async.Start(async {
        let! result = mailbox.PostAndAsyncReply(fun reply -> PutValue(v, reply))
        sink(result)
      })

  let toInterface<'a> : MailboxProcessor<Command<'a>> -> IAsyncBuffer<'a> = toSink >> AsyncBufferSink.toInterface
  let toRecord<'a> : MailboxProcessor<Command<'a>> -> AsyncBuffer<'a> = toSink >> AsyncBufferSink.toRecord
