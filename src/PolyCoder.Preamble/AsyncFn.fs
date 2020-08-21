namespace PolyCoder

type AsyncFn<'a, 'b> = 'a -> Async<'b>

[<RequireQualifiedAccess>]
module AsyncFn =
  let result v : AsyncFn<_, _> = fun _ -> Async.result v
  let raise e : AsyncFn<_, _> = fun _ -> Async.raise e

  let bind f ma : AsyncFn<_, _> = ma >> (Async.bind f)
  let map f ma : AsyncFn<_, _> = ma >> (Async.map f)
  let ignore ma = ma |> map ignore

  let ofTask ma : AsyncFn<_, _> = ma >> Async.ofTask
  let ofTaskVoid ma : AsyncFn<_, _> = ma >> Async.ofTaskVoid
  let toTask ma = ma >> Async.toTask
  let toTaskVoid ma = ma >> Async.toTaskVoid

  let ofFn fn : AsyncFn<_, _> = fn >> Async.result

  let bindTask f ma : AsyncFn<_, _> = ma >> (Async.bindTask f)
  let bindTaskVoid f ma : AsyncFn<_, _> = ma >> (Async.bindTaskVoid f)
  let mapTask f ma : AsyncFn<_, _> = ma >> (Async.mapTask f)
  let mapTaskVoid f ma : AsyncFn<_, _> = ma >> (Async.mapTaskVoid f)
