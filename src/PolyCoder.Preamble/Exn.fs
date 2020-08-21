namespace PolyCoder

open System
open System.Reflection
open System.Net

[<RequireQualifiedAccess>]
module Exn =
  let preserveStackTrace =
    lazy typeof<exn>.GetMethod(
      "InternalPreserveStackTrace",
      BindingFlags.Instance ||| BindingFlags.NonPublic)
    
  let inline reraise exn =
    (exn, null)
      |> preserveStackTrace.Value.Invoke
      |> ignore

    raise exn

  let findInner<'a when 'a :> exn> (exn: exn) : 'a option =
    let rec find (e: exn) =
      let fromSeq source = 
        source
          |> Seq.map find
          |> Seq.tryFind Option.isSome
          |> Option.flatten
      match e with
      | :? 'a as exn -> Some exn
      | :? AggregateException as aggExn -> fromSeq aggExn.InnerExceptions
      | _ -> find e.InnerException

    find exn

[<AutoOpen>]
module ExnUtils =
  let (|IsWebException|_|) (exn: exn) = Exn.findInner<WebException> exn
