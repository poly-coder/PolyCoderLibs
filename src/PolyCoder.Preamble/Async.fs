namespace PolyCoder

open System.Threading.Tasks

[<RequireQualifiedAccessAttribute>]
module Async =
  let zero = async.Zero()
  
  let result a = async.Return a
  
  let delay f = async.Delay f
  
  let raise exn = delay <| fun () -> raise exn
  
  let failwith msg = delay <| fun () -> failwith msg
  
  let failwithf msg = delay <| fun () -> failwithf msg
  
  let invalidArg name msg = delay <| fun () -> invalidArg name msg
  
  let nullArg msg = delay <| fun () -> nullArg msg
  
  let invalidOp msg = delay <| fun () -> invalidOp msg
  
  let notImpl() = delay notImpl

  let bind f ma = async.Bind(ma, f)
  
  let map f = bind (f >> result)

  let ofTask ma = Async.AwaitTask ma
  
  let ofTaskVoid ma = Async.AwaitTask (ma: Task)
  
  let toTask ma = Async.StartAsTask ma
  
  let toTaskVoid ma = toTask ma |> fun t -> t :> Task

  let inline startAsTask ma = ma |> Async.StartAsTask
  
  let inline startAsTaskVoid ma = ma |> startAsTask |> fun t -> t :> Task

  let bindTask f = ofTask >> bind f
  
  let bindTaskVoid f = ofTaskVoid >> bind f
  
  let mapTask f = ofTask >> map f
  
  let mapTaskVoid f = ofTaskVoid >> map f

  let ignoreExn ma = async {
      try do! ma with _ -> ()
  }  

  let whenAllSerial (source: Async<'a> seq) = async {
    let e = source.GetEnumerator()
    let rec loop l = async {
        if not (e.MoveNext()) then
            return List.rev l |> List.toSeq
        else
            let! value = e.Current
            return! loop (value :: l)
    }
    return! loop []
  }

  let whenAll source =
    source
      |> Seq.map (Async.StartAsTask)
      |> Task.WhenAll
      |> ofTask
      |> map Array.toSeq

  let toPromise (asyncExpr: Async<_>) =
      Async.StartAsTask(asyncExpr)
      |> Async.AwaitTask
