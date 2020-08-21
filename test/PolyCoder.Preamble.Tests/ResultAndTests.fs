module PolyCoder.ResultAndTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote
open System

[<SetUp>]
let Setup () =
  ()

// resultAnd

[<Property>]
let ``ResultAnd.resultAnd { return value } should return Some value`` (value: int) =
  test <@ resultAnd { return value } = Ok value @>

[<Property>]
let ``ResultAnd.resultAnd { return! Some value } should return Some value`` (value: int) =
  test <@ resultAnd { return! Some value } = Some value @>

[<Property>]
let ``ResultAnd.resultAnd { return! None } should return None`` () =
  test <@ resultAnd { return! None } = None @>

[<Property>]
let ``ResultAnd.resultAnd { let x = value in return x } should return Some value`` (value: int) =
  test <@ resultAnd { let x = value in return x } = Ok value @>

[<Property>]
let ``ResultAnd.resultAnd { let! x = Some value in return x } should return Some value`` (value: int) =
  test <@ resultAnd { let! x = Ok value in return x } = Ok value @>

[<Property>]
let ``ResultAnd.resultAnd { let! x = None in return x } should return None`` (e: string) =
  test <@ resultAnd { let! x = Error e in return x } = Error e @>

[<Property>]
let ``ResultAnd.resultAnd { do () return value } should return Some value`` (value: int) =
  let ce = resultAnd {
    do () 
    return value
  }
  test <@ ce = Ok value @>

[<Property>]
let ``ResultAnd.resultAnd { do! Some () in in return value } should return Some value`` (value: int) =
  let ce = resultAnd {
    do! Ok () 
    return value
  }
  test <@ ce = Ok value @>

[<Property>]
let ``ResultAnd.resultAnd { do! None in return value } should return None`` (value: int) (e: string) =
  let ce = resultAnd {
    do! Error e 
    return value
  }
  test <@ ce = Error e @>

[<Property>]
let ``ResultAnd.resultAnd { use _d = disposable in return value } should return Some value`` (value: int) =
  let disposable = { new IDisposable with member _.Dispose() = () }
  let ce = resultAnd {
    use _d = disposable
    return value
  }
  test <@ ce = Ok value @>

[<Property>]
let ``ResultAnd.resultAnd { use _d = disposable in return value } should call the Dispose method`` (value: int) =
  let mutable disposed = false
  let disposable = { new IDisposable with member _.Dispose() = disposed <- true }
  resultAnd {
    use _d = disposable
    return value
  } |> ignore
  test <@ disposed = true @>

[<Property>]
let ``ResultAnd.resultAnd { use! _d = Some disposable in return value } should return Some value`` (value: int) =
  let disposable = { new IDisposable with member _.Dispose() = () }
  let ce = resultAnd {
    use! _d = Ok disposable
    return value
  }
  test <@ ce = Ok value @>

[<Property>]
let ``ResultAnd.resultAnd { use! _d = Some disposable in return value } should call the Dispose method`` (value: int) =
  let mutable disposed = false
  let disposable = { new IDisposable with member _.Dispose() = disposed <- true }
  resultAnd {
    use! _d = Ok disposable
    return value
  } |> ignore
  test <@ disposed = true @>

[<Property>]
let ``ResultAnd.resultAnd { use! _d = None in return value } should return None`` (value: int) (e: string) =
  let ce = resultAnd {
    use! _d = Error e
    return value
  }
  test <@ ce = Error e @>

[<Property>]
let ``ResultAnd.resultAnd { if true then {...} } should return Some ()`` () =
  let mutable thenCalled = false
  let ce = resultAnd {
    if true then
      thenCalled <- true
  }
  test <@ ce = Ok () @>
  test <@ thenCalled = true @>

[<Property>]
let ``ResultAnd.resultAnd { if false then {...} } should return Some ()`` () =
  let mutable thenCalled = false
  let ce = resultAnd {
    if false then
      thenCalled <- true
  }
  test <@ ce = Ok () @>
  test <@ thenCalled = false @>

[<Property>]
let ``ResultAnd.resultAnd { if true then return a else return b } should return Some a`` (thenValue: int) (elseValue: int) =
  let ce = resultAnd {
    if true then
      return thenValue
    else
      return elseValue
  }
  test <@ ce = Ok thenValue @>

[<Property>]
let ``ResultAnd.resultAnd { if false then return a else return b } should return Some b`` (thenValue: int) (elseValue: int) =
  let ce = resultAnd {
    if false then
      return thenValue
    else
      return elseValue
  }
  test <@ ce = Ok elseValue @>

[<Property>]
let ``ResultAnd.resultAnd { match expr with | pattern_i -> cexpr_i } should return cexpr_i`` (a: int) (b: int) (c: int) =
  let ce = resultAnd {
    match 2 with
    | 1 -> return a
    | 2 -> return b
    | _ -> return c
  }
  test <@ ce = Ok b @>

[<Property>]
let ``ResultAnd.resultAnd { for pattern in expr do cexpr } should return Some ()`` (source: int list) =
  let ce = resultAnd {
    for _item in source do ()
  }
  test <@ ce = Ok () @>

[<Property>]
let ``ResultAnd.resultAnd { for pattern in expr do cexpr } should call body with every source item`` (source: int list) =
  let mutable items = []
  let _ce = resultAnd {
    for item in source do
      items <- items @ [item]
  }
  test <@ items = source @>

[<Property>]
let ``ResultAnd.resultAnd { while expr do cexpr } should return Some ()`` (PositiveInt count: PositiveInt) =
  let mutable c = count
  let condition() = c <- c - 1; c >= 0
  let ce = resultAnd {
    while condition() do ()
  }
  test <@ ce = Ok () @>

[<Property>]
let ``ResultAnd.resultAnd { while expr do cexpr } should call body the given amount of times`` (PositiveInt count: PositiveInt) =
  let mutable c = count
  let mutable bodyCount = 0
  let condition() = c <- c - 1; c >= 0
  let body() = bodyCount <- bodyCount + 1
  let _ce = resultAnd {
    while condition() do body ()
  }
  test <@ bodyCount = count @>

[<Property>]
let ``ResultAnd.resultAnd { try cexpr with | pattern_i -> expr_i } when returning a monad should return the same monad`` (ma: Result<int, string>) =
  let ce = resultAnd {
    try
      return! ma
    with
      _exn -> return 42
  }
  test <@ ce = ma @>

[<Property>]
let ``ResultAnd.resultAnd { try cexpr with | pattern_i -> expr_i } when throwing an exception should return Some value from the handler`` (value: int) =
  let ce = resultAnd {
    try
      return raise <| InvalidOperationException()
    with
      _exn -> return value
  }
  test <@ ce = Ok value @>
