module PolyCoder.PreambleTests

open System
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote

[<SetUp>]
let Setup () =
  ()

[<Property>]
let ``konst should always return a constant function`` (k: int) (any: int) =
  test <@ (konst k) any = k @>

[<Property>]
let ``konst2 should always return a constant function`` (k: int) (any1: int) (any2: string) =
  test <@ (konst2 k) any1 any2 = k @>

[<Property>]
let ``konst3 should always return a constant function`` (k: int) (any1: int) (any2: string) (any3: float) =
  test <@ (konst3 k) any1 any2 any3 = k @>

[<Property>]
let ``flip should always return a flipped function`` (a: int) (b: int) =
  let f x y = 10 * x + y  
  test <@ (flip f) a b = f b a @>

[<Property>]
let ``curry should always return a curried function`` (a: int) (b: int) =
  let f (x, y) = 10 * x + y  
  test <@ (curry f) a b = f(a, b) @>

[<Property>]
let ``uncurry should always return an uncurried function`` (a: int) (b: int) =
  let f x y = 10 * x + y
  test <@ (uncurry f) (a, b) = f a b @>

[<Property>]
let ``curry3 should always return a curried function`` (a: int) (b: int) (c: int) =
  let f (x, y, z) = 100 * x + 10 * y + z
  test <@ (curry3 f) a b c = f(a, b, c) @>

[<Property>]
let ``uncurry3 should always return an uncurried function`` (a: int) (b: int) (c: int) =
  let f x y z = 100 * x + 10 * y + z
  test <@ (uncurry3 f) (a, b, c) = f a b c @>

[<Property>]
let ``tee should always return the input value`` (input: int) =
  test <@ input |> tee ignore = input @>

[<Property>]
let ``tee should always call the given function with the given input`` (input: int) =
  let mutable calledWith = None
  let f x = calledWith <- Some x
  input |> tee f |> ignore
  test <@ calledWith = Some input @>

[<Property>]
let ``teeIgnore should always return the input value`` (input: int) (any: int) =
  test <@ input |> teeIgnore (konst any) = input @>

[<Property>]
let ``teeIgnore should always call the given function with the given input`` (input: int) (any: int) =
  let mutable calledWith = None
  let f x =
    calledWith <- Some x
    any
  input |> teeIgnore f |> ignore
  test <@ calledWith = Some input @>

[<Property>]
let ``fstArg should always return the first arg`` (arg1: int) (arg2: int) =
  test <@ fstArg arg1 arg2 = arg1 @>

[<Property>]
let ``sndArg should always return the first arg`` (arg1: int) (arg2: int) =
  test <@ sndArg arg1 arg2 = arg2 @>

[<Property>]
let ``asObj should always return the same value as obj`` (str: string) =
  test <@ obj.ReferenceEquals(asObj str, str) @>

[<Property>]
let ``ofObj should always return the same value as obj`` (str: string) =
  test <@ ofObj(asObj str) = str @>

[<Property>]
let ``asObj with value types should always return the same value as obj`` (value: int) =
  test <@ obj.Equals(asObj value, value) @>

[<Property>]
let ``refEq should return true iff both are the same reference`` (NonNull a: NonNull<string>) =
  test <@ refEq a a @>
  test <@ refEq (a + "extra1") (a + "extra2") |> not @>

[<Test>]
let ``dispose of null should do nothing``() =
  dispose null

[<Test>]
let ``dispose should call the Dispose method``() =
  let disposed = ref false
  let disposable =
    {
      new IDisposable with
        member _.Dispose() = disposed := true
    }
  dispose disposable
  test <@ !disposed @>

[<Property>]
let ``isNotNull should be the opposite of isNull`` (str: string) =
  test <@ isNotNull str = (isNull str |> not) @>

[<Property>]
let ``eq should be the same as (=)`` (a: byte) b =
  test <@ eq a b = (a = b) @>

[<Property>]
let ``lt should be the same as <)`` (a: byte) b =
  test <@ lt a b = (a < b) @>

[<Property>]
let ``gt should be the same as (>)`` (a: byte) b =
  test <@ gt a b = (a > b) @>

[<Property>]
let ``le should be the same as (<=)`` (a: byte) b =
  test <@ le a b = (a <= b) @>

[<Property>]
let ``ge should be the same as (>=)`` (a: byte) b =
  test <@ ge a b = (a >= b) @>

[<Property>]
let ``neq should be the same as (<>)`` (a: byte) b =
  test <@ neq a b = (a <> b) @>

[<Property>]
let ``nlt should be the same as (>=)`` (a: byte) b =
  test <@ nlt a b = (a >= b) @>

[<Property>]
let ``ngt should be the same as (<=)`` (a: byte) b =
  test <@ ngt a b = (a <= b) @>

[<Property>]
let ``nle should be the same as (>)`` (a: byte) b =
  test <@ nle a b = (a > b) @>

[<Property>]
let ``nge should be the same as (<)`` (a: byte) b =
  test <@ nge a b = (a < b) @>

[<Test>]
let ``notImpl should throw NotImplementedException`` () =
  Assert.Throws<NotImplementedException>(fun () -> notImpl ()) |> ignore

[<Test>]
let ``notImpl2 should throw NotImplementedException`` () =
  Assert.Throws<NotImplementedException>(fun () -> notImpl2 () ()) |> ignore

[<Test>]
let ``notImpl3 should throw NotImplementedException`` () =
  Assert.Throws<NotImplementedException>(fun () -> notImpl3 () () ()) |> ignore
  
