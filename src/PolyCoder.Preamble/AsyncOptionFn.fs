namespace PolyCoder

type AsyncOptionFn<'a, 'b> = 'a -> AsyncOption<'b>

[<RequireQualifiedAccess>]
module AsyncOptionFn =
  let some a = fun _ -> AsyncOption.some a

  let none<'a, 'b> : AsyncOptionFn<'a, 'b> =
    konst AsyncOption.none
  
  let matchWith fSome fNone (ma: AsyncOptionFn<_, _>) =
    ma >> AsyncOption.matchWith fSome fNone
  
  let matchWithAsync fSome fNone (ma: AsyncOptionFn<_, _>) =
    ma >> AsyncOption.matchWithAsync fSome fNone

  let bind (f: AsyncOptionFn<'b, 'c>) (ma: AsyncOptionFn<'a, 'b>) : AsyncOptionFn<'a, 'c> =
    ma >> AsyncOption.bind f

  let map f = bind (f >> AsyncOption.some)

  let mapOption f = bind (f >> Async.result)
  
  let mapAsync f = bind (f >> Async.map Some)
  
  let mapTask f = bind (f >> AsyncOption.ofTask)

  let ignore ma = ma |> map ignore
