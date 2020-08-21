namespace PolyCoder

open System.Collections.Generic

type FoldStep<'a> =
  | Continue
  | ContinueWith of 'a
  | Break
  | BreakWith of 'a

[<RequireQualifiedAccess>]
module Seq =
  let bind f = Seq.collect f
  
  let ignore ma = Seq.map ignore ma

  let getEnumerator (source: _ seq) = source.GetEnumerator()
  
  let getCurrent (enumerator: IEnumerator<_>) = enumerator.Current
  
  let moveNext (enumerator: IEnumerator<_>) =
    enumerator.MoveNext()

  let scanWhile folder initialState source = seq {
    use enumerator = getEnumerator source

    let rec loop state = seq {
      if moveNext enumerator then
        match getCurrent enumerator |> folder state with
          | Continue ->
            yield! loop state // Keep looping
          | ContinueWith state ->
            yield state
            yield! loop state // Keep looping
          | Break ->
            do () // Stop looping
          | BreakWith state ->
            yield state
            do () // Stop looping
    }

    yield initialState
    yield! loop initialState
  }

  let scanWhileNone folder =
    let folder' state value =
      match folder state value with
        | None -> Continue
        | Some x -> BreakWith x
    scanWhile folder'

  let scanWhileSome folder =
    let folder' state value =
      match folder state value with
        | Some x -> ContinueWith x
        | None -> Break
    scanWhile folder'

  let scanOnSome folder =
    let folder' state value =
      match folder state value with
        | Some x -> ContinueWith x
        | None -> Continue
    scanWhile folder'
    

  let foldWhile folder initialState source =
    scanWhile folder initialState source
      |> Seq.last

  let foldWhileNone folder initialState source =
    scanWhileNone folder initialState source
      |> Seq.last

  let foldWhileSome folder initialState source =
    scanWhileSome folder initialState source
      |> Seq.last

  let foldOnSome folder initialState source =
    scanOnSome folder initialState source
      |> Seq.last

  let windowedAll windowSize (source: _ seq) =
    let e = source.GetEnumerator()
    let array = ResizeArray(windowSize: int)
    let rec loop() = seq {
      if e.MoveNext() then
        array.Add(e.Current)
        if array.Count >= windowSize then
          yield array.ToArray()
          array.Clear()
        yield! loop()
      else
        if array.Count > 0 then yield array.ToArray()
        e.Dispose()
    }
    loop()

  let dfsKeyedWith (comparer: IEqualityComparer<'key>) getKey getChildren (source: seq<'item>) =
    let foundSet = HashSet<_>(comparer)
    let rec loop item = seq {
      let key = getKey item
      if foundSet.Add key then
        yield item
        yield! getChildren item |> Seq.collect loop
    }
    source |> Seq.collect loop

  let dfsKeyed getKey = dfsKeyedWith EqualityComparer<_>.Default getKey
  let dfsWith comparer getChildren = dfsKeyedWith comparer id getChildren
  let dfs getChildren = dfsWith EqualityComparer<_>.Default getChildren

  let dlsKeyedWith (comparer: IEqualityComparer<'key>) getKey getChildren (source: seq<'item>) =
    let foundSet = HashSet<_>(comparer)
    let rec loop item = seq {
      let key = getKey item
      if foundSet.Add key then
        yield! getChildren item |> Seq.collect loop
        yield item
    }
    source |> Seq.collect loop

  let dlsKeyed getKey = dlsKeyedWith EqualityComparer<_>.Default getKey
  let dlsWith comparer getChildren = dlsKeyedWith comparer id getChildren
  let dls getChildren = dlsWith EqualityComparer<_>.Default getChildren
