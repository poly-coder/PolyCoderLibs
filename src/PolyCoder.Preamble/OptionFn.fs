namespace PolyCoder

type OptionFn<'a, 'b> = 'a -> Option<'b>

[<RequireQualifiedAccess>]
module OptionFn =
  let bind f ma : OptionFn<_, _> = ma >> Option.bind f
  
  let map f ma : OptionFn<_, _> = ma >> Option.map f
  
  let matchWith fSome fNone ma = ma >> Option.matchWith fSome fNone
