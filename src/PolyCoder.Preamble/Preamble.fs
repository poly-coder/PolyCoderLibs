[<AutoOpen>]
module PolyCoder.Preamble

let konst k _ = k
let konst2 k _ _ = k
let konst3 k _ _ _ = k

let flip fn x y = fn y x

let curry fn x y = fn(x, y)
let uncurry fn (x, y) = fn x y

let curry3 fn x y z = fn(x, y, z)
let uncurry3 fn (x, y, z) = fn x y z

let tee fn x = fn x; x
let teeIgnore fn = tee (fn >> ignore)

let fstArg x _ = x
let sndArg _ y = y

let asObj a = a :> obj
let ofObj<'a> (o: obj) = o :?> 'a

let refEq a b = obj.ReferenceEquals(a, b)

let dispose (e: #System.IDisposable) =
  match e with
  | null -> ()
  | e -> e.Dispose()

let isNotNull x = isNull x |> not

let eq x y = x = y
let lt x y = x < y
let gt x y = x > y
let le x y = x <= y
let ge x y = x >= y

let neq x y = x <> y
let nlt x y = x >= y
let ngt x y = x <= y
let nle x y = x > y
let nge x y = x < y

let notImpl _ = raise <| System.NotImplementedException()
let notImpl2 _ = notImpl
let notImpl3 _ = notImpl2
