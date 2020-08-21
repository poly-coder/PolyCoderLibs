namespace PolyCoder.Convertion

open System.Text

type Converter<'a, 'b> = 'a -> Async<'b>
type SyncConverter<'a, 'b> = 'a -> 'b

type DualConverter<'a, 'b> = Converter<'a, 'b> * Converter<'b, 'a>
type DualSyncConverter<'a, 'b> = SyncConverter<'a, 'b> * SyncConverter<'b, 'a>

module SyncConverter =
  let identity<'a> : SyncConverter<'a, 'a> = id

module Converter =
  let identity<'a> : Converter<'a, 'a> = async.Return

  let ofSync (cv: SyncConverter<_, _>) : Converter<_, _> = cv >> identity

module DualSyncConverter =
  let inline create i o : DualSyncConverter<_, _> = i, o

  let inline convert ((i, _): DualSyncConverter<_,_>) = i
  let inline deconvert ((_, o): DualSyncConverter<_,_>) = o

  let reverse (cv: DualSyncConverter<_, _>) =
      create
          (convert cv)
          (deconvert cv)

  let stringToEncoding (encoding: Encoding) =
      create
          (fun str -> encoding.GetBytes(str: string))
          (fun bytes -> encoding.GetString(bytes: byte[]))

  let stringToUtf8 = stringToEncoding Encoding.UTF8
  let stringToUtf32 = stringToEncoding Encoding.UTF32
  let stringToUnicode = stringToEncoding Encoding.Unicode

module DualConverter =
  let create i o : DualConverter<_, _> = i, o

  let reverse ((i, o): DualConverter<_, _>) =
      create o i

  let convert ((i, _): DualConverter<_,_>) = i
  let deconvert ((_, o): DualConverter<_,_>) = o

  let ofSync (cv: DualSyncConverter<_, _>) =
      create
          (cv |> DualSyncConverter.convert |> Converter.ofSync)
          (cv |> DualSyncConverter.deconvert |> Converter.ofSync)

  let stringToEncoding encoding =
      DualSyncConverter.stringToEncoding encoding
          |> ofSync

  let stringToUtf8 = DualSyncConverter.stringToUtf8 |> ofSync
  let stringToUtf32 = DualSyncConverter.stringToUtf32 |> ofSync
  let stringToUnicode = DualSyncConverter.stringToUnicode |> ofSync
