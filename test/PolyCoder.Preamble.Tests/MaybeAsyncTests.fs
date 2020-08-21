module PolyCoder.MaybeAsyncAsyncTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote
open System

[<SetUp>]
let Setup () =
  ()

// MaybeAsync

[<Property>]
let ``MaybeAsync.result value should return Some value`` (value: int) =
    async {
      let! actual = MaybeAsync.result value
      test <@ actual = Some value @>
    } |> Async.RunSynchronously


[<Property>]
let ``MaybeAsync.zero should return Some()`` () =
  async {
    let! actual = MaybeAsync.zero
    test <@ actual = Some () @>
  } |> Async.RunSynchronously

//[<Property>]
//let ``MaybeAsync.returnFrom (Some value) should return Some value`` (value: int) =
//  test <@ MaybeAsync.returnFrom (Some value) = Some value @>

//[<Property>]
//let ``MaybeAsync.returnFrom None should return None`` () =
//  test <@ MaybeAsync.returnFrom None = None @>

//[<Property>]
//let ``MaybeAsync.bind on None should return None`` (mb: int option) =
//  test <@ None |> MaybeAsync.bind (fun _ -> mb) = None @>

//[<Property>]
//let ``MaybeAsync.bind on None should not call the function`` (mb: int option) =
//  let mutable calledTimes = 0
//  let f _ =
//    calledTimes <- calledTimes + 1
//    mb
//  None |> MaybeAsync.bind f |> ignore
//  test <@ calledTimes = 0 @>

//[<Property>]
//let ``MaybeAsync.bind on Some value should return the second value`` (a: int) (mb: int option) =
//  test <@ Some a |> MaybeAsync.bind (fun _ -> mb) = mb @>

//[<Property>]
//let ``MaybeAsync.bind on Some value should call the function exactly once`` (a: int) (mb: int option) =
//  let mutable calledTimes = 0
//  let f _ =
//    calledTimes <- calledTimes + 1
//    mb
//  Some a |> MaybeAsync.bind f |> ignore
//  test <@ calledTimes = 1 @>

//[<Property>]
//let ``MaybeAsync.bind on Some value should call the function with the value`` (a: int) (mb: int option) =
//  let mutable calledWith = None
//  let f x =
//    calledWith <- Some x
//    mb
//  Some a |> MaybeAsync.bind f |> ignore
//  test <@ calledWith = Some a @>

// maybeAsync

[<Property>]
let ``MaybeAsync.maybeAsync { return value } should return Some value`` (value: int) =
  async {
    let! actual = maybeAsync { return value }
    test <@ actual = Some value @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { return! some value } should return Some value`` (value: int) =
  async {
    let! actual = maybeAsync { return! AsyncOption.some value }
    test <@ actual = Some value @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { return! none } should return None`` () =
  async {
    let! actual = maybeAsync { return! AsyncOption.none }
    test <@ actual = None @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { let x = value in return x } should return Some value`` (value: int) =
  async {
    let! actual = maybeAsync { let x = value in return x }
    test <@ actual = Some value @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { let! x = Some value in return x } should return Some value`` (value: int) =
  async {
    let! actual = maybeAsync { let! x = AsyncOption.some value in return x }
    test <@ actual = Some value @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { let! x = None in return x } should return None`` () =
  async {
    let! actual = maybeAsync { let! x = AsyncOption.none in return x }
    test <@ actual = None @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { do () return value } should return Some value`` (value: int) =
  async {
    let! actual = maybeAsync {
      do () 
      return value
    }
    test <@ actual = Some value @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { do! Some () in in return value } should return Some value`` (value: int) =
  async {
    let! actual = maybeAsync {
      do! AsyncOption.some () 
      return value
    }
    test <@ actual = Some value @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { do! None in return value } should return None`` (value: int) =
  async {
    let! actual = maybeAsync {
      do! AsyncOption.none
      return value
    }
    test <@ actual = None @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { use _d = disposable in return value } should return Some value`` (value: int) =
  let disposed = ref false
  let disposable = { new IDisposable with member _.Dispose() = disposed := true }
  async {
    let! actual = maybeAsync {
      use _d = disposable
      return value
    }
    test <@ actual = Some value @>
    test <@ !disposed = true @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { use! _d = Some disposable in return value } should return Some value`` (value: int) =
  let disposed = ref false
  let disposable = { new IDisposable with member _.Dispose() = disposed := true }
  async {
    let! actual = maybeAsync {
      use! _d = AsyncOption.some disposable
      return value
    }
    test <@ actual = Some value @>
    test <@ !disposed = true @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { use! _d = None in return value } should return None`` (value: int) =
  async {
    let! actual = maybeAsync {
      use! _d = AsyncOption.none
      return value
    }
    test <@ actual = None @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { if true then {...} } should return Some ()`` () =
  let thenCalled = ref false
  async {
    let! actual = maybeAsync {
      if true then
        thenCalled := true
    }
    test <@ actual = Some () @>
    test <@ !thenCalled = true @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { if false then {...} } should return Some ()`` () =
  let thenCalled = ref false
  async {
    let! actual = maybeAsync {
      if false then
        thenCalled := true
    }
    test <@ actual = Some () @>
    test <@ !thenCalled = false @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { if true then return a else return b } should return Some a`` (thenValue: int) (elseValue: int) =
  async {
    let! actual = maybeAsync {
      if true then
        return thenValue
      else
        return elseValue
    }
    test <@ actual = Some thenValue @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { if false then return a else return b } should return Some b`` (thenValue: int) (elseValue: int) =
  async {
    let! actual = maybeAsync {
      if false then
        return thenValue
      else
        return elseValue
    }
    test <@ actual = Some elseValue @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { match expr with | pattern_i -> cexpr_i } should return cexpr_i`` (a: int) (b: int) (c: int) =
  async {
    let! actual = maybeAsync {
      match 2 with
      | 1 -> return a
      | 2 -> return b
      | _ -> return c
    }
    test <@ actual = Some b @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { for pattern in expr do cexpr } should return Some ()`` (source: int list) =
  let items = ref []
  async {
    let! actual = maybeAsync {
      for item in source do
        items := !items @ [item]
    }
    test <@ !items = source @>
    test <@ actual = Some () @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { while expr do cexpr } should return Some ()`` (PositiveInt count: PositiveInt) =
  let counter = ref count
  let bodyCount = ref 0
  let condition() =
    counter := !counter - 1
    !counter >= 0
  async {
    let! actual = maybeAsync {
      while condition() do
        bodyCount := !bodyCount + 1
    }
    test <@ !counter = -1 @>
    test <@ !bodyCount = count @>
    test <@ actual = Some () @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { try cexpr with | pattern_i -> expr_i } when returning an option should return the same option`` (ma: int option) =
  async {
    let! actual = maybeAsync {
      try
        return! Async.result ma
      with
        _exn -> return 42
    }
    test <@ actual = ma @>
  } |> Async.RunSynchronously

[<Property>]
let ``MaybeAsync.maybeAsync { try cexpr with | pattern_i -> expr_i } when throwing an exception should return Some value from the handler`` (value: int) =
  async {
    let! actual = maybeAsync {
      try
        return raise <| InvalidOperationException()
      with
        _exn -> return! AsyncOption.some value
    }
    test <@ actual = Some value @>
  } |> Async.RunSynchronously


