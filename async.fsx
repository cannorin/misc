#load "paket.core.fsx"
#load "import.fsx"

frameworks ["net45"]

import "FSharp.Control.Reactive"

#load "main.group.fsx"

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Reactive
open FSharp.Control.Reactive.Builders

let loop name (ct: CancellationToken) =
  let rec f i =
    ct.ThrowIfCancellationRequested();
    Thread.Sleep 1000;
    printfn "loop \"%s\" [%i]" name i;
    if i = 10 then 42 else f (i+1)
  in f 0

let withTimeoutAsync (f: CancellationToken -> 'a) (i: int) =
  async {
    use ctst = new CancellationTokenSource() in
    let taskt = Task.Delay (i, ctst.Token) in
    use ctsf = new CancellationTokenSource() in
    let taskf = Task.Run (fun () -> f ctsf.Token) in
    let! completed = Async.AwaitTask <| Task.WhenAny(taskt, taskf) in
    if completed = (taskf :> Task) then
      do ctst.Cancel();
      let! result = Async.AwaitTask taskf in
      return Some result  
    else
      do ctsf.Cancel();
      return None
  }

let compute i ct =
  loop (sprintf "compute %i" i) ct

let workAsync i =
  async {
    let! result = withTimeoutAsync (compute i) (i * 1000) in
    return match result with
      | Some x -> printfn "The ultimate answer is: %i" x; true
      | None ->   printfn "What is the ultimate question ?"; false
  }

let obs =
  observe {
    yield 4;
    yield 8;
    yield 12
  }

let bot () =
  let obsc = obs |> Observable.publish in
  use worker = obsc |> Observable.flatmapTask (workAsync >> Async.Catch >> Async.StartAsTask)
                    |> Observable.subscribe (fun r -> printfn "%A" r)
  in
  use d = Observable.connect obsc in
  Thread.Sleep (14 * 1000)

bot()
