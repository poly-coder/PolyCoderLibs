namespace PolyCoder

type SeqFn<'a, 'b> = 'a -> seq<'b>

[<RequireQualifiedAccess>]
module SeqFn =
  let bind f ma : SeqFn<_, _> = ma >> Seq.bind f
  
  let map f ma : SeqFn<_, _> = ma >> Seq.map f

  let allPairs f1 f2 : SeqFn<_, _> = fun a -> Seq.allPairs (f1 a) (f2 a)
  
  let append f1 f2 : SeqFn<_, _> = fun a -> Seq.append (f1 a) (f2 a)
  
  let choose chooser ma : SeqFn<_, _> = ma >> Seq.choose chooser
  
  let chunkBySize chunkSize ma : SeqFn<_, _> = ma >> Seq.chunkBySize chunkSize
  
  let collect f ma : SeqFn<_, _> = ma >> Seq.collect f
  
  let concat sourcesFn : SeqFn<_, _> = fun a -> sourcesFn |> Seq.map (fun fn -> fn a) |> Seq.concat
  
  let countBy projection ma : SeqFn<_, _> = ma >> Seq.countBy projection
  
  let distinct ma : SeqFn<_, _> = ma >> Seq.distinct
  
  let distinctBy projection ma : SeqFn<_, _> = ma >> Seq.distinctBy projection
  
  let except itemsToExclude ma : SeqFn<_, _> = ma >> Seq.except itemsToExclude
  
  let exists predicate ma = ma >> Seq.exists predicate
  
  let filter predicate ma : SeqFn<_, _> = ma >> Seq.filter predicate
  
  let fold folder state ma = ma >> Seq.fold folder state
  
  let foldBack folder state ma = ma >> Seq.foldBack folder state
  
  let foldWhile folder state ma = ma >> Seq.foldWhile folder state
  
  let foldWhileNone folder state ma = ma >> Seq.foldWhileNone folder state
  
  let foldWhileSome folder state ma = ma >> Seq.foldWhileSome folder state
  
  let foldOnSome folder state ma = ma >> Seq.foldOnSome folder state
  
  let groupBy projection ma = ma >> Seq.groupBy projection


  let ignore ma = ma >> Seq.ignore

  let indexed ma = ma >> Seq.indexed

  let iter fn ma = ma >> Seq.iter fn

  let iteri fn ma = ma >> Seq.iteri fn

  let scan folder state ma = ma >> Seq.scan folder state

  let scanBack folder state ma = ma >> Seq.scanBack folder state

  let scanWhile folder state ma = ma >> Seq.scanWhile folder state

  let scanWhileNone folder state ma = ma >> Seq.scanWhileNone folder state

  let scanWhileSome folder state ma = ma >> Seq.scanWhileSome folder state

  let scanOnSome folder state ma = ma >> Seq.scanOnSome folder state
