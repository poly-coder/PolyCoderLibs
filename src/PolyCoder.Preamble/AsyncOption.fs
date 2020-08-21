namespace PolyCoder

open System.Threading.Tasks

type AsyncOption<'a> = Async<Option<'a>>

[<RequireQualifiedAccess>]
module AsyncOption =
  let ofOption a: AsyncOption<_> = Async.result a
  
  let ofAsync a: AsyncOption<_> = a |> Async.map Some
  
  let ofTask a: AsyncOption<_> = a |> Async.AwaitTask |> Async.map Some

  let some a = ofOption (Some a)

  let none<'a> : AsyncOption<'a> = ofOption None

  let matchWith fSome fNone (ma: AsyncOption<_>) = async {
    let! a = ma
    return a |> Option.matchWith fSome fNone
  }

  let matchWithAsync fSome fNone (ma: AsyncOption<_>) = async {
    let! a = ma
    return! a |> Option.matchWith fSome fNone
  }

  let bind (f: _ -> AsyncOption<_>) ma : AsyncOption<_> =
    ma |> matchWithAsync f (konst none)

  let bindTask (f: _ -> Task<Option<_>>) ma : AsyncOption<_> =
    ma |> matchWithAsync (f >> Async.ofTask) (konst none)

  let map f = bind (f >> some)

  let mapOption f = bind (f >> Async.result)
  
  let mapAsync f = bind (f >> Async.map Some)
  
  let mapTask f = bind (f >> ofTask)

  let ignore ma = ma |> map ignore

[<RequireQualifiedAccess>]
module MaybeAsync =
  let result value = Async.result (Some value)

  let zero = result()
  
  let returnFrom x = x

  let bind f ma = AsyncOption.bind f ma
  
  let bindTask f ma = AsyncOption.bindTask f ma

  let map f = AsyncOption.map f
  
  let mapOption f = AsyncOption.mapOption f
  
  let mapAsync f = AsyncOption.mapAsync f
  
  let mapTask f = AsyncOption.mapTask f
  
  let combine mb = bind (konst mb)
  
  let combineOption mb = mapOption (konst mb)
  
  let combineAsync mb = mapAsync (konst mb)
  
  let combineTask mb = mapTask (konst mb)

  type Builder() =
    member _.Zero() = zero

    member _.Return x = result x

    member _.ReturnFrom x = x

    member _.Delay f = f
    
    member _.Run f = f()

    member _.Bind(ma, f) = ma |> bind f

    member _.Combine(ma, mb) = ma |> combine mb

    member _.TryWith(body, handler) = async {
      try return! body()
      with exn -> return! handler exn
    }

    member _.TryFinally(body, handler) = async {
      try
        return! body()
      finally
        handler ()
    }

    member _.Using(expression, body) = async {
      use expr = expression
      return! body expr
    }

    member _.While(guard, body) = async {
      while guard() do
        do! body() |> Async.Ignore
      return! zero
    }

    member _.For(source: _ seq, body) = async {
      use enumerator = source.GetEnumerator()
      while enumerator.MoveNext() do
        do! body enumerator.Current |> Async.Ignore // or combine?
      return! zero
    }


[<AutoOpen>]
module DefaultMaybeAsyncBuilder =
  let maybeAsync = MaybeAsync.Builder()
