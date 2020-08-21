module PolyCoder.OrElseTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote
open System

[<SetUp>]
let Setup () =
  ()

// OrElse

[<Property>]
let ``OrElse.result value should return Some value`` (value: int) =
  test <@ OrElse.result value = Some value @>

[<Property>]
let ``OrElse.zero should return None`` () =
  test <@ OrElse.zero = None @>

[<Property>]
let ``OrElse.returnFrom (Some value) should return Some value`` (value: int) =
  test <@ OrElse.returnFrom (Some value) = Some value @>

[<Property>]
let ``OrElse.returnFrom None should return None`` () =
  test <@ OrElse.returnFrom None = None @>

[<Property>]
let ``OrElse.bind on None should return The next value`` (mb: int option) =
  test <@ None |> OrElse.bind (fun _ -> mb) = mb @>

[<Property>]
let ``OrElse.bind on None should call the function`` (mb: int option) =
  let mutable calledTimes = 0
  let f _ =
    calledTimes <- calledTimes + 1
    mb
  None |> OrElse.bind f |> ignore
  test <@ calledTimes = 1 @>

[<Property>]
let ``OrElse.bind on Some value should return the first value`` (a: int) (mb: int option) =
  test <@ Some a |> OrElse.bind (fun _ -> mb) = Some a @>

[<Property>]
let ``OrElse.bind on Some value should not call the function`` (a: int) (mb: int option) =
  let mutable calledTimes = 0
  let f () =
    calledTimes <- calledTimes + 1
    mb
  Some a |> OrElse.bind f |> ignore
  test <@ calledTimes = 0 @>

// orElse

[<Property>]
let ``OrElse.orElse { return value } should return Some value`` (value: int) =
  test <@ orElse { return value } = Some value @>

[<Property>]
let ``OrElse.orElse { return! Some value } should return Some value`` (value: int) =
  test <@ orElse { return! Some value } = Some value @>

[<Property>]
let ``OrElse.orElse { return! None } should return None`` () =
  test <@ orElse { return! None } = None @>

[<Property>]
let ``OrElse.orElse { let x = value in return x } should return Some value`` (value: int) =
  test <@ orElse { let x = value in return x } = Some value @>

[<Property>]
let ``OrElse.orElse { let! x = Some value in return x } should return Some value`` (value: int) =
  test <@ orElse { let! _ = Some value in return! None } = Some value @>

[<Property>]
let ``OrElse.orElse { let! x = None in return next } should return Some next`` (next: int) =
  test <@ orElse { let! _ = None in return next } = Some next @>

[<Property>]
let ``OrElse.orElse { do () return value } should return Some value`` (value: int) =
  let ce = orElse {
    do () 
    return value
  }
  test <@ ce = Some value @>

[<Property>]
let ``OrElse.orElse { do! Some value in in return! None } should return Some value`` (value: int) =
  let ce = orElse {
    do! Some value
    return! None
  }
  test <@ ce = Some value @>

[<Property>]
let ``OrElse.orElse { do! None in return value } should return Some value`` (value: int) =
  let ce = orElse {
    do! None 
    return value
  }
  test <@ ce = Some value @>

[<Property>]
let ``OrElse.orElse { use _d = disposable in return value } should return Some value`` (value: int) =
  let disposable = { new IDisposable with member _.Dispose() = () }
  let ce = orElse {
    use _d = disposable
    return value
  }
  test <@ ce = Some value @>

[<Property>]
let ``OrElse.orElse { use _d = disposable in return value } should call the Dispose method`` (value: int) =
  let mutable disposed = false
  let disposable = { new IDisposable with member _.Dispose() = disposed <- true }
  orElse {
    use _d = disposable
    return value
  } |> ignore
  test <@ disposed = true @>

[<Property>]
let ``OrElse.orElse { if true then {...} } should return None`` () =
  let mutable thenCalled = false
  let ce = orElse {
    if true then
      thenCalled <- true
  }
  test <@ ce = None @>
  test <@ thenCalled = true @>

[<Property>]
let ``OrElse.orElse { if false then {...} } should return None`` () =
  let mutable thenCalled = false
  let ce = orElse {
    if false then
      thenCalled <- true
  }
  test <@ ce = None @>
  test <@ thenCalled = false @>

[<Property>]
let ``OrElse.orElse { if true then return a else return b } should return Some a`` (thenValue: int) (elseValue: int) =
  let ce = orElse {
    if true then
      return thenValue
    else
      return elseValue
  }
  test <@ ce = Some thenValue @>

[<Property>]
let ``OrElse.orElse { if false then return a else return b } should return Some b`` (thenValue: int) (elseValue: int) =
  let ce = orElse {
    if false then
      return thenValue
    else
      return elseValue
  }
  test <@ ce = Some elseValue @>

[<Property>]
let ``OrElse.orElse { match expr with | pattern_i -> cexpr_i } should return cexpr_i`` (a: int) (b: int) (c: int) =
  let ce = orElse {
    match 2 with
    | 1 -> return a
    | 2 -> return b
    | _ -> return c
  }
  test <@ ce = Some b @>

[<Property>]
let ``OrElse.orElse { for pattern in expr do cexpr } should return None`` (source: int list) =
  let ce = orElse {
    for _item in source do ()
  }
  test <@ ce = None @>

[<Property>]
let ``OrElse.orElse { for pattern in expr do cexpr } should call body with every source item`` (source: int list) =
  let mutable items = []
  let _ce = orElse {
    for item in source do
      items <- items @ [item]
  }
  test <@ items = source @>

[<Property>]
let ``OrElse.orElse { while expr do cexpr } should return None`` (PositiveInt count: PositiveInt) =
  let mutable c = count
  let condition() = c <- c - 1; c >= 0
  let ce = orElse {
    while condition() do ()
  }
  test <@ ce = None @>

[<Property>]
let ``OrElse.orElse { while expr do cexpr } should call body the given amount of times`` (PositiveInt count: PositiveInt) =
  let mutable c = count
  let mutable bodyCount = 0
  let condition() = c <- c - 1; c >= 0
  let body() = bodyCount <- bodyCount + 1
  let _ce = orElse {
    while condition() do body ()
  }
  test <@ bodyCount = count @>

[<Property>]
let ``OrElse.orElse { try cexpr with | pattern_i -> expr_i } when returning a monad should return the same monad`` (ma: int option) =
  let ce = orElse {
    try
      return! ma
    with
      _exn -> return 42
  }
  test <@ ce = ma @>

[<Property>]
let ``OrElse.orElse { try cexpr with | pattern_i -> expr_i } when throwing an exception should return Some value from the handler`` (value: int) =
  let ce = orElse {
    try
      return raise <| InvalidOperationException()
    with
      _exn -> return value
  }
  test <@ ce = Some value @>


