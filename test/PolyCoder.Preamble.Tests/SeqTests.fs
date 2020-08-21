module PolyCoder.SeqTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote

[<SetUp>]
let Setup () =
  ()

[<Property>]
let ``foldWith, when first returned Break, should always return the initial state``
    (initialState: string)
    (source: int list) =
  let folder (state: string) (value: int): FoldStep<string> = Break
  test <@ source |> Seq.foldWhile folder initialState = initialState @>

[<Property>]
let ``foldWith, when returning Break on a non-empty source, should call the folder function exactly once``
    (initialState: string)
    (NonEmptyArray source: NonEmptyArray<int>) =
  let mutable calledTimes = 0
  let folder (state: string) (value: int): FoldStep<string> =
    calledTimes <- calledTimes + 1
    Break
  source |> Seq.foldWhile folder initialState |> ignore
  test <@ calledTimes = 1 @>

[<Property>]
let ``foldWith, on an empty source, should always return the initial state``
    (initialState: string)
    anyStep =
  let folder (state: string) (value: int): FoldStep<string> = anyStep
  test <@ [] |> Seq.foldWhile folder initialState = initialState @>

[<Property>]
let ``foldWith, on an empty source, should not call the folder ever``
    (initialState: string)
    anyStep =
  let mutable calledTimes = 0
  let folder (state: string) (value: int): FoldStep<string> =
    calledTimes <- calledTimes + 1
    anyStep
  [] |> Seq.foldWhile folder initialState |> ignore
  test <@ calledTimes = 0 @>

[<Property>]
let ``foldWith, when first returned BreakWith a value, should always return that value``
    (initialState: string)
    (breakValue: string)
    (NonEmptyArray source: NonEmptyArray<int>) =
  let folder (state: string) (value: int): FoldStep<string> = BreakWith breakValue
  test <@ source |> Seq.foldWhile folder initialState = breakValue @>

[<Property>]
let ``foldWith, when returning BreakWith a value on a non-empty source, should call the folder function exactly once``
    (initialState: string)
    (breakValue: string)
    (NonEmptyArray source: NonEmptyArray<int>) =
  let mutable calledTimes = 0
  let folder (state: string) (value: int): FoldStep<string> =
    calledTimes <- calledTimes + 1
    BreakWith breakValue
  source |> Seq.foldWhile folder initialState |> ignore
  test <@ calledTimes = 1 @>

[<Property>]
let ``foldWith, when returning ContinueWith a concatenation, should always return the concatenation of all values``
    (NonNull initialState: NonNull<string>)
    (source: int list) =
  let folder (state: string) (value: int): FoldStep<string> = ContinueWith (sprintf "%s,%d" state value)
  test <@ source |> Seq.foldWhile folder initialState = String.concat "," (seq {yield initialState; yield! source |> Seq.map string}) @>

[<Property>]
let ``foldWith, when returning ContinueWith anything, should call the folder function for the entire source``
    (initialState: string)
    (source: int list) =
  let mutable calledTimes = 0
  let folder (state: string) (value: int): FoldStep<string> =
    calledTimes <- calledTimes + 1
    ContinueWith (sprintf "%s,%d" state value)
  source |> Seq.foldWhile folder initialState |> ignore
  test <@ calledTimes = List.length source @>

[<Property>]
let ``foldWith, when returning Continue, should always return the initial state``
    (initialState: string)
    (source: int list) =
  let folder (state: string) (value: int): FoldStep<string> = Continue
  test <@ source |> Seq.foldWhile folder initialState = initialState @>

[<Property>]
let ``foldWith, when returning Continue, should call the folder function for the entire source``
    (initialState: string)
    (source: int list) =
  let mutable calledTimes = 0
  let folder (state: string) (value: int): FoldStep<string> =
    calledTimes <- calledTimes + 1
    Continue
  source |> Seq.foldWhile folder initialState |> ignore
  test <@ calledTimes = List.length source @>
