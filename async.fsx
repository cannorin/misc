#load "paket.core.fsx"
#load "import.fsx"

frameworks ["net45"]

import "FSharp.Control.Reactive"

#load "main.group.fsx"

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Reactive

let loop name (ct: CancellationToken) =
  ct.ThrowIfCancellationRequested();
  for i in 1..10 do
    Thread.Sleep 1000;
    printfn "loop \"%s\" [%i]" name i;
  done
  42

let loopRef name (ct: CancellationToken ref) =
  (!ct).ThrowIfCancellationRequested();
  for i in 1..10 do
    Thread.Sleep 1000;
    printfn "loopRef \"%s\" [%i]" name i;
  done
  42

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

let withTimeoutRefAsync (f: CancellationToken ref -> 'a) (i: int) =
  async {
    use ctst = new CancellationTokenSource() in
    let taskt = Task.Delay (i, ctst.Token) in
    use ctsf = new CancellationTokenSource() in
    let ctf = ref ctsf.Token in
    let taskf = Task.Run (fun () -> f ctf) in
    let! completed = Async.AwaitTask <| Task.WhenAny(taskt, taskf) in
    if completed = (taskf :> Task) then
      do ctst.Cancel();
      let! result = Async.AwaitTask taskf in
      return Some result  
    else
      do ctsf.Cancel();
      do ctf := ctsf.Token;
      return None
  }

let workAsync () =
  async {
    let! result = withTimeoutAsync (loop "called from the inside of F# async") (5 * 1000) in
    match result with
      | Some x -> printfn "The ultimate answer is: %i" x
      | None ->   printfn "What is the ultimate question ?"
  }


let workRefAsync () =
  async {
    let! result = withTimeoutRefAsync (loopRef "called from the inside of F# async") (6 * 1000) in
    match result with
      | Some x -> printfn "The ultimate ref answer is: %i" x
      | None ->   printfn "What is the ultimate ref question ?"
  }

let outside = withTimeoutAsync (loop "called from the outside of F# async") (3 * 1000) |> Async.Ignore in
let inside = workAsync () in
let outsideRef = withTimeoutRefAsync (loopRef "called from the outside of F# async") (4 * 1000) |> Async.Ignore in
let insideRef = workRefAsync () in
[inside; outside; insideRef; outsideRef] |> Async.Parallel |> Async.Ignore |> Async.Start;
Thread.Sleep (12 * 1000)
