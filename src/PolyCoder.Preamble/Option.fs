namespace PolyCoder

[<RequireQualifiedAccess>]
module Option =
  let matchWith fSome fNone ma =
    match ma with
    | Some a -> fSome a
    | _ -> fNone ()

  let firstSome source =
    let folder _prev = matchWith (Some >> BreakWith) (konst Continue)
    source |> Seq.foldWhile folder None

  let catch f x =
    try f x |> Some
    with _ -> None

  let ignore ma = ma |> Option.map ignore

  let toSeq ma = ma |> matchWith Seq.singleton (konst Seq.empty)
  
  let toList ma = ma |> matchWith List.singleton (konst List.empty)
  
  let toArray ma = ma |> matchWith Array.singleton (konst Array.empty)

  let ifPredicate predicate value =
    if predicate value then Some value
    else None

  let ofTryOperation (success, value) =
    if success then Some value
    else None

[<RequireQualifiedAccess>]
module Maybe =
  let result x = Some x
  
  let zero = result ()
  
  let returnFrom x = x

  let bind f ma = Option.bind f ma

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
module DefaultMaybeBuilder =
  let maybe = Maybe.Builder()

[<RequireQualifiedAccess>]
module OrElse =
  let result x = Some x
  
  let zero = None
  
  let returnFrom x = x

  let bind f ma =
    match ma with
    | Some x -> Some x
    | None -> f()

  let map f = bind (f >> result)
  
  let combine mb = bind (konst mb)

  type Builder() =
    member _.Zero() = zero

    member _.Return x = Some x

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
module DefaultOrElseBuilder =
  let orElse = OrElse.Builder()
