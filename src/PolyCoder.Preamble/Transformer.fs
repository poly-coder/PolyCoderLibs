namespace PolyCoder

type UnsafeSyncTransformer<'a, 'b> = 'a -> 'b
type UnsafeTransformer<'a, 'b> = UnsafeSyncTransformer<'a, Async<'b>>
type SyncTransformer<'a, 'b> = UnsafeSyncTransformer<'a, Result<'b, exn>>
type Transformer<'a, 'b> = UnsafeSyncTransformer<'a, AsyncResult<'b, exn>>

module UnsafeTransformer =
  let ofSync (transformer: UnsafeSyncTransformer<'a, 'b>) : UnsafeTransformer<'a, 'b> =
    transformer >> async.Return

module SyncTransformer =
  let ofUnsafe (transformer: UnsafeSyncTransformer<'a, 'b>) : SyncTransformer<'a, 'b> =
    Result.catch transformer

  let toUnsafe (transformer: SyncTransformer<'a, 'b>) : UnsafeSyncTransformer<'a, 'b> =
    transformer >> Result.unsafeGet

module Transformer =
  let ofSync (transformer: SyncTransformer<'a, 'b>) : Transformer<'a, 'b> =
    transformer >> async.Return

  let ofUnsafeSync transformer =
    transformer |> SyncTransformer.ofUnsafe |> ofSync

  let ofUnsafe (transformer: UnsafeTransformer<'a, 'b>) : Transformer<'a, 'b> =
    AsyncResult.catch transformer

  let toUnsafe(transformer: Transformer<'a, 'b>) : UnsafeTransformer<'a, 'b> =
    transformer >> AsyncResult.unsafeGet
