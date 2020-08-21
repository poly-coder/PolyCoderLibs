namespace PolyCoder

type AsyncResult<'a, 'b> = Async<Result<'a, 'b>>
type AsyncResult<'a> = AsyncResult<'a, exn>

[<RequireQualifiedAccess>]
module AsyncResult =
  let ok a : AsyncResult<_, _> = Ok a |> Async.result

  let error e : AsyncResult<_, _> = Error e |> Async.result
  
  let catch fn a : AsyncResult<_> = async {
    try
      let! result = fn a
      return Ok result
    with
    | exn -> return Error exn
  }

  let bind (f: _ -> AsyncResult<_, _>) (ma: AsyncResult<_, _>) : AsyncResult<_, _> = async {
    match! ma with
    | Ok b -> return! f b
    | Error e -> return Error e
  }

  let unsafeGet ma = async {
    let! result = ma
    return Result.unsafeGet result
  }

  let pipeTo fb fa =
    fa >> bind fb
