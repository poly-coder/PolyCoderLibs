namespace PolyCoder

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Map =
  let toDict map =
    map
    |> Map.toSeq
    |> dict

  let ofDict (dict: #IDictionary<_, _>) =
    dict
    |> Seq.map (fun pair -> pair.Key, pair.Value)
    |> Map.ofSeq

