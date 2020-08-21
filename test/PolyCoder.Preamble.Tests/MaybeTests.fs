module PolyCoder.MaybeTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote
open System

[<SetUp>]
let Setup () =
  ()

// Maybe

[<Property>]
let ``Maybe.result value should return Some value`` (value: int) =
  test <@ Maybe.result value = Some value @>

[<Property>]
let ``Maybe.zero should return Some()`` () =
  test <@ Maybe.zero = Some () @>

[<Property>]
let ``Maybe.returnFrom (Some value) should return Some value`` (value: int) =
  test <@ Maybe.returnFrom (Some value) = Some value @>

[<Property>]
let ``Maybe.returnFrom None should return None`` () =
  test <@ Maybe.returnFrom None = None @>

[<Property>]
let ``Maybe.bind on None should return None`` (mb: int option) =
  test <@ None |> Maybe.bind (fun _ -> mb) = None @>

[<Property>]
let ``Maybe.bind on None should not call the function`` (mb: int option) =
  let mutable calledTimes = 0
  let f _ =
    calledTimes <- calledTimes + 1
    mb
  None |> Maybe.bind f |> ignore
  test <@ calledTimes = 0 @>

[<Property>]
let ``Maybe.bind on Some value should return the second value`` (a: int) (mb: int option) =
  test <@ Some a |> Maybe.bind (fun _ -> mb) = mb @>

[<Property>]
let ``Maybe.bind on Some value should call the function exactly once`` (a: int) (mb: int option) =
  let mutable calledTimes = 0
  let f _ =
    calledTimes <- calledTimes + 1
    mb
  Some a |> Maybe.bind f |> ignore
  test <@ calledTimes = 1 @>

[<Property>]
let ``Maybe.bind on Some value should call the function with the value`` (a: int) (mb: int option) =
  let mutable calledWith = None
  let f x =
    calledWith <- Some x
    mb
  Some a |> Maybe.bind f |> ignore
  test <@ calledWith = Some a @>

// maybe

[<Property>]
let ``Maybe.maybe { return value } should return Some value`` (value: int) =
  test <@ maybe { return value } = Some value @>

[<Property>]
let ``Maybe.maybe { return! Some value } should return Some value`` (value: int) =
  test <@ maybe { return! Some value } = Some value @>

[<Property>]
let ``Maybe.maybe { return! None } should return None`` () =
  test <@ maybe { return! None } = None @>

[<Property>]
let ``Maybe.maybe { let x = value in return x } should return Some value`` (value: int) =
  test <@ maybe { let x = value in return x } = Some value @>

[<Property>]
let ``Maybe.maybe { let! x = Some value in return x } should return Some value`` (value: int) =
  test <@ maybe { let! x = Some value in return x } = Some value @>

[<Property>]
let ``Maybe.maybe { let! x = None in return x } should return None`` () =
  test <@ maybe { let! x = None in return x } = None @>

[<Property>]
let ``Maybe.maybe { do () return value } should return Some value`` (value: int) =
  let ce = maybe {
    do () 
    return value
  }
  test <@ ce = Some value @>

[<Property>]
let ``Maybe.maybe { do! Some () in in return value } should return Some value`` (value: int) =
  let ce = maybe {
    do! Some () 
    return value
  }
  test <@ ce = Some value @>

[<Property>]
let ``Maybe.maybe { do! None in return value } should return None`` (value: int) =
  let ce = maybe {
    do! None 
    return value
  }
  test <@ ce = None @>

[<Property>]
let ``Maybe.maybe { use _d = disposable in return value } should return Some value`` (value: int) =
  let disposable = { new IDisposable with member _.Dispose() = () }
  let ce = maybe {
    use _d = disposable
    return value
  }
  test <@ ce = Some value @>

[<Property>]
let ``Maybe.maybe { use _d = disposable in return value } should call the Dispose method`` (value: int) =
  let mutable disposed = false
  let disposable = { new IDisposable with member _.Dispose() = disposed <- true }
  maybe {
    use _d = disposable
    return value
  } |> ignore
  test <@ disposed = true @>

[<Property>]
let ``Maybe.maybe { use! _d = Some disposable in return value } should return Some value`` (value: int) =
  let disposable = { new IDisposable with member _.Dispose() = () }
  let ce = maybe {
    use! _d = Some disposable
    return value
  }
  test <@ ce = Some value @>

[<Property>]
let ``Maybe.maybe { use! _d = Some disposable in return value } should call the Dispose method`` (value: int) =
  let mutable disposed = false
  let disposable = { new IDisposable with member _.Dispose() = disposed <- true }
  maybe {
    use! _d = Some disposable
    return value
  } |> ignore
  test <@ disposed = true @>

[<Property>]
let ``Maybe.maybe { use! _d = None in return value } should return None`` (value: int) =
  let ce = maybe {
    use! _d = None
    return value
  }
  test <@ ce = None @>

[<Property>]
let ``Maybe.maybe { if true then {...} } should return Some ()`` () =
  let mutable thenCalled = false
  let ce = maybe {
    if true then
      thenCalled <- true
  }
  test <@ ce = Some () @>
  test <@ thenCalled = true @>

[<Property>]
let ``Maybe.maybe { if false then {...} } should return Some ()`` () =
  let mutable thenCalled = false
  let ce = maybe {
    if false then
      thenCalled <- true
  }
  test <@ ce = Some () @>
  test <@ thenCalled = false @>

[<Property>]
let ``Maybe.maybe { if true then return a else return b } should return Some a`` (thenValue: int) (elseValue: int) =
  let ce = maybe {
    if true then
      return thenValue
    else
      return elseValue
  }
  test <@ ce = Some thenValue @>

[<Property>]
let ``Maybe.maybe { if false then return a else return b } should return Some b`` (thenValue: int) (elseValue: int) =
  let ce = maybe {
    if false then
      return thenValue
    else
      return elseValue
  }
  test <@ ce = Some elseValue @>

[<Property>]
let ``Maybe.maybe { match expr with | pattern_i -> cexpr_i } should return cexpr_i`` (a: int) (b: int) (c: int) =
  let ce = maybe {
    match 2 with
    | 1 -> return a
    | 2 -> return b
    | _ -> return c
  }
  test <@ ce = Some b @>

[<Property>]
let ``Maybe.maybe { for pattern in expr do cexpr } should return Some ()`` (source: int list) =
  let ce = maybe {
    for _item in source do ()
  }
  test <@ ce = Some () @>

[<Property>]
let ``Maybe.maybe { for pattern in expr do cexpr } should call body with every source item`` (source: int list) =
  let mutable items = []
  let _ce = maybe {
    for item in source do
      items <- items @ [item]
  }
  test <@ items = source @>

[<Property>]
let ``Maybe.maybe { while expr do cexpr } should return Some ()`` (PositiveInt count: PositiveInt) =
  let mutable c = count
  let condition() = c <- c - 1; c >= 0
  let ce = maybe {
    while condition() do ()
  }
  test <@ ce = Some () @>

[<Property>]
let ``Maybe.maybe { while expr do cexpr } should call body the given amount of times`` (PositiveInt count: PositiveInt) =
  let mutable c = count
  let mutable bodyCount = 0
  let condition() = c <- c - 1; c >= 0
  let body() = bodyCount <- bodyCount + 1
  let _ce = maybe {
    while condition() do body ()
  }
  test <@ bodyCount = count @>

[<Property>]
let ``Maybe.maybe { try cexpr with | pattern_i -> expr_i } when returning a monad should return the same monad`` (ma: int option) =
  let ce = maybe {
    try
      return! ma
    with
      _exn -> return 42
  }
  test <@ ce = ma @>

[<Property>]
let ``Maybe.maybe { try cexpr with | pattern_i -> expr_i } when throwing an exception should return Some value from the handler`` (value: int) =
  let ce = maybe {
    try
      return raise <| InvalidOperationException()
    with
      _exn -> return value
  }
  test <@ ce = Some value @>


