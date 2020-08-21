namespace PolyCoder

[<RequireQualifiedAccess>]
module Result =
  let ok a = Ok a
  
  let error e = Error e

  let matchWith fOk fError ma =
    match ma with
    | Ok a -> fOk a
    | Error error -> fError error

  let firstOk source =
    let folder _prev = matchWith (Some >> BreakWith) (konst Continue)
    source |> Seq.foldWhile folder None

  let catch f x = try f x |> Ok with e -> Error e

  let ignore ma = ma |> Result.map ignore

  let toSeq ma = ma |> matchWith Seq.singleton (konst Seq.empty)
  
  let toList ma = ma |> matchWith List.singleton (konst List.empty)
  
  let toArray ma = ma |> matchWith Array.singleton (konst Array.empty)

  let unsafeGet = function
    | Ok a -> a
    | Error exn -> Exn.reraise exn

  let pipeTo fb fa =
    fa >> Result.bind fb

[<RequireQualifiedAccess>]
module ResultAnd =
  let result x = Ok x
  
  let zero<'e> : Result<unit, 'e> = result ()
  
  let returnFrom x = x

  let bind f ma = Result.bind f ma

  let map f = bind (f >> result)
  
  let combine mb = bind (konst mb)

  type Builder() =
    member _.Zero() = zero

    member _.Return x = result x

    member _.ReturnFrom x = x

    member _.Delay f = f
    
    member _.Run f = f()

    member _.Bind(ma, f) = ma |> bind f

    member _.Combine(ma, mb) = ma |> combine mb

    member _.TryWith(body, handler) =
      try body()
      with exn -> handler exn

    member _.TryFinally(body, handler) =
      try body()
      finally handler ()

    member _.Using(expression, body) =
      use expr = expression
      body expr

    member _.While(guard, body) =
      while guard() do
        body() |> ignore
      zero

    member _.For(source: _ seq, body) =
      use enumerator = source.GetEnumerator()
      while enumerator.MoveNext() do
        body enumerator.Current |> ignore // or combine?
      zero

[<AutoOpen>]
module DefaultResultAndBuilder =
  let resultAnd = ResultAnd.Builder()
