(*
The X11 License
prelude.fs - my prelude
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

[<AutoOpen>]
module Prelude

open System
open System.Text.RegularExpressions

exception UndefinedException of msg: string with
  override this.Message = this.msg

let inline to_s x = x.ToString()

let inline (?|) opt df = defaultArg opt df

let inline undefined (x: 'a) : 'b = to_s x |> UndefinedException |> raise

let private ccl (fc: ConsoleColor) =
  Console.ForegroundColor <- fc;
  { new IDisposable with
      member x.Dispose() = Console.ResetColor() }

let cprintf color format =
  Printf.kprintf (fun s -> use c = ccl color in printf "%s" s) format

let cprintfn color format =
  Printf.kprintf (fun s -> use c = ccl color in printfn "%s" s) format

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

let (|DefaultValue|) dv x =
  match x with
    | Some v -> v
    | None -> dv

let inline (!!) (x: Lazy<'a>) = x.Value

module Lazy =
  begin
    let inline run (x: Lazy<'a>) = x.Value
    
    let inline bind (f: 'a -> Lazy<'b>) (x: Lazy<'a>) =
      lazy (!!x |> f) |> run

    let inline map (f: 'a -> 'b) (x: Lazy<'a>) =
      lazy (!!x |> f)

    let inline flatten (x: Lazy<Lazy<'a>>) = !!(!!x)
  end

[<Struct>]
type LazyBuilder =
  member inline this.Bind(m, f) = Lazy.bind f m
  member inline this.Return x = lazy x
  member inline this.ReturnFrom lx = lx
  member inline this.Zero () = lazy ()
  member inline this.Combine(a, b) = a
  member inline this.Delay f = lazy (!!f())

let doLazy = LazyBuilder ()

module List = 
  begin
    let rec takeToEnd n xs =
      if List.length xs > n then
        (xs |> List.take n) :: takeToEnd n (xs |> List.skip n)
      else
        [xs]
  end

module Result =
  begin
    let inline toOption res =
      match res with
        | Ok x -> Some x
        | Error _ -> None
    
    let inline toChoice res =
      match res with
        | Ok x -> Choice1Of2 x
        | Error e -> Choice2Of2 e

    let inline ofChoice cic =
      match cic with
        | Choice1Of2 x -> Ok x
        | Choice2Of2 e -> Error e

    let inline defaultWith f res =
      match res with
        | Ok x -> x
        | Error e -> f e

    let inline defaultValue y res =
      match res with
        | Ok x -> x
        | Error _ -> y
  end

module PerformanceMeasurement =
  begin
    let inline time repeats task =
      let times = 
        seq {
          for _ in 1 .. repeats do
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() in
            do task () |> ignore;
            do stopWatch.Stop();
            yield stopWatch.Elapsed.TotalMilliseconds
        }
      in
      printfn "%A: %gms" task (Seq.average times);
      task ()
  end

module Async =
  begin
    open System.Threading
    open System.Threading.Tasks
    open Microsoft.FSharp.Control

    let inline run x = Async.RunSynchronously x

    let withTimeout (timeout : TimeSpan) a =
      async {
        try
          let! child = Async.StartChild(a, int timeout.TotalMilliseconds) in
          let! result = child in
          return Some result
        with
          | :? TimeoutException -> return None
      }
end

