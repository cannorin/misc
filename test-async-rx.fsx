(*
The X11 License
test-async-rx.fsx - example of using async and rx in F#
Copyright(c) 2018 cannorin
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

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
