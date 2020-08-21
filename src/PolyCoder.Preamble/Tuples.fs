namespace PolyCoder

[<RequireQualifiedAccess>]
module Tuple2 =
    let map fn1 fn2 (a, b) = fn1 a, fn2 b


[<RequireQualifiedAccess>]
module Tuple3 =
    let map fn1 fn2 fn3 (a, b, c) = fn1 a, fn2 b, fn3 c

