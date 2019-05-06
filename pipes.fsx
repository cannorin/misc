(*
The MIT License
Pipes - compositional pipelines for F#
Copyright(c) 2019 cannorin
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


module Pipes

let inline private undefined () = failwith "undefined"

/// Use ``await_`` and ``yield_`` to build Pipes and ``>->`` to connect Pipes.
/// 
/// ``cat_`` and ``>->`` obey the Category laws:
///
///    // Useless use of cat
///    cat_ >-> f = f
///    
///    // Redirecting output to cat does nothing
///    f >-> cat_ = f
///    
///    // The pipe operator is associative
///    (f >-> g) >-> h = f >-> (g >-> h)
type Pipe<'In, 'Out, 'Result> =
  | Yield of 'Out *   Pipe<'In, 'Out, 'Result>
  | Await of ('In ->  Pipe<'In, 'Out, 'Result>)
  | Delay of (unit -> Pipe<'In, 'Out, 'Result>)
  | Done of 'Result
  with
    static member inline Return x = Done x
    static member ( >>= ) (m, f) =
      let rec (>>=) m f =
        match m with
          | Yield (o, Done x) -> Yield (o, f x)
          | Yield (o, m') -> Yield (o, m' >>= f)
          | Await g       -> Await (fun i -> g i >>= f)
          | Delay g       -> Delay (fun () -> g () >>= f)
          | Done x        -> f x
      m >>= f
    static member ( *> ) (s1, s2) =
      let rec ( *> ) s1 s2 =
        match s1 with
          | Yield (o, Done _) -> Yield (o, s2)
          | Yield (o, s1') -> Yield (o, s1' *> s2)
          | Await g        -> Await (fun i -> g i *> s2)
          | Delay g        -> Delay (fun () -> g () *> s2)
          | Done _         -> s2
      s1 *> s2

/// uninhabited type. used to indicate that the pipeline have no input and/or output.
type Closed private () =
  static member absurd (_: Closed) : 'a = undefined ()

/// Use ``yield_`` to produce output and ``^~>`` / ``for_`` to substitute yields.
/// 
/// ``yield_`` and ``^~>`` obey the Category laws:
///
///    // Substituting 'yield_' with 'f' gives 'f'
///    yield_ ^~> f = f
///    
///    // Substituting every 'yield_' with another 'yield_' does nothing
///    f ^~> yield_ = f
///    
///    // 'yield_' substitution is associative
///    (f ^~> g) ^~> h = f ^~> (g ^~> h)
///
/// These are equivalent to the following "for loop laws":
///
///    // Looping over a single yield simplifies to function application
///    for_ (yield_ x) f = f x
///    
///    // Re-yielding every element of a stream returns the original stream
///    for_ s yield_ = s
///    
///    // Nested for loops can become a sequential for loops if the inner loop
///    // body ignores the outer loop variable
///    for_ s (\a -> for_ (f a) g) = for_ (for_ s f) g = for_ s (f ^~> g)
type Producer<'Out, 'Result> = Pipe<Closed, 'Out, 'Result>

/// Use ``await_`` to request input and ``@~>`` to substitute awaits.
///
/// ``await_`` and ``@~>`` obey the Category laws:
///
///    // Substituting every 'await_' with another 'await_' does nothing
///    await_ @~> f = f
///    
///    // Substituting 'await_' with 'f' gives 'f'
///    f @~> await_ = f
///    
///    // 'await_' substitution is associative
///    (f @~> g) @~> h = f @~> (g @~> h)
type Consumer<'In, 'Result>  = Pipe<'In, Closed, 'Result>

type Effect<'Result>         = Pipe<Closed, Closed, 'Result>

module Builder =
  module HelperFunctions =
    let rec undelay pipe =
      match pipe with
        | Delay f -> f () |> undelay
        | x -> x
    let inline tryWith m f () = try undelay m with exn -> f exn
    let inline tryFinally m f () = try undelay m finally f ()
    let rec whileLoopImpl (cond, m) =
      if not <| cond () then Done ()
      else Delay (fun () -> undelay m *> whileLoopImpl  (cond, m))
    let inline whileLoop cond m () = whileLoopImpl (cond, m)
    let rec mapInput f pipe =
      match pipe with
        | Yield (o, s) -> Yield (o, mapInput f s)
        | Await g      -> Await (fun i  -> g (f i) |> mapInput f)
        | Delay g      -> Delay (fun () -> g () |> mapInput f)
        | Done  x      -> Done x
    let rec mapOutput f pipe =
      match pipe with
        | Yield (o, s) -> Yield (f o, mapOutput f s)
        | Await g      -> Await (fun i  -> g i  |> mapOutput f)
        | Delay g      -> Delay (fun () -> g () |> mapOutput f)
        | Done  x      -> Done x
    let inline mapResult f (pipe: Pipe<_, _, _>) = pipe >>= (f >> Done)
    let rec weakenProducer (producer: Producer<_, _>) =
      match producer with
        | Yield (o, s) -> Yield (o, weakenProducer s)
        | Await _      -> undefined ()
        | Delay f      -> Delay (fun () -> f () |> weakenProducer)
        | Done  x      -> Done x
    let inline weakenConsumer (consumer: Consumer<_, _>) = mapOutput Closed.absurd consumer
    let inline weakenEffect   (effect: Effect<_>) = effect |> weakenProducer |> weakenConsumer
    let inline forList (exec: _ -> Pipe<_, _, unit>) (xs: _ list) () =
      match xs with
        | [] -> Done ()
        | h :: xs ->
          let mutable result = exec h
          for x in xs do
            result <- result *> exec x
          result *> Done ()
    let inline forArray (exec: _ -> Pipe<_, _, unit>) (xs: _ array) () =
      if xs.Length = 0 then Done ()
      else
        let mutable result = exec xs.[0]
        for i = 1 to xs.Length - 1 do
          result <- result *> exec xs.[i]
        result *> Done ()
    let rec forPipeImpl (exec: _ -> Pipe<_, _, unit>, xs: Pipe<_, _, _>) =
      match xs with
        | Yield (p, xs) -> exec p *> forPipeImpl (exec, xs)
        | Await f       -> Await (fun i  -> forPipeImpl (exec, f i))
        | Delay f       -> Delay (fun () -> forPipeImpl (exec, f()))
        | Done  x       -> Done x
    let inline forPipe (exec: _ -> Pipe<_, _, unit>) (xs: Pipe<_, _, _>) () =
      forPipeImpl (exec, xs)
    
  open HelperFunctions

  type PipeBuilder() =
    member inline this.Bind (m: Pipe<_, _, 'a>, f: 'a -> _) = m >>= f
    member inline this.Return x = Done x
    member inline this.ReturnFrom (x: Pipe<_, _, _>) = x
    member inline this.Delay (f: unit -> Pipe<_, _, _>) = Delay f
    member inline this.TryWith (m, f) = Delay (tryWith m f)
    member inline this.TryFinally (m, f) = Delay (tryFinally m f)
    member inline this.Combine (a: Pipe<_, _, _>, b) = a *> b
    member inline this.While (cond, m: Pipe<_, _, unit>) = Delay (whileLoop cond m)
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> disp.Dispose()
      )
    member inline this.Yield x = Yield (x, Done ())
    member inline this.YieldFrom (x: Pipe<_, _, _>) = x
    member inline this.Zero () = Done ()
    member inline this.For (xs: seq<'a>, exec: 'a -> Pipe<_, 'b, unit>) =
      this.Using(
        (xs :> seq<_>).GetEnumerator(),
        fun en ->
          this.While(
            en.MoveNext,
            this.Delay(fun () -> exec en.Current))
      )
    member inline this.For (xs: 'a list, exec: 'a -> Pipe<_, 'b, unit>)  = Delay (forList exec xs)
    member inline this.For (xs: 'a [],   exec: 'a -> Pipe<_, 'b, unit>)  = Delay (forArray exec xs)
    member inline this.For (xs: Pipe<_, _, _>, exec: _ -> Pipe<_, _, _>) = Delay (forPipe exec xs)
    
open Builder
open Builder.HelperFunctions

let pipe = PipeBuilder()

let empty_<'i, 'o> : Pipe<'i, 'o, unit> = Done ()

/// yields an output of type ``'a``.
///
/// you can use
///
///     yield x
///
/// instead of 
///
///     let! _ = yield_ x`
let inline yield_ (x: 'a) = Yield (x, Done ())

/// ``for_ p body`` loops over ``p`` replacing each ``yield b`` with ``do! body b``.
///
/// if ``'r`` is ``unit``, you can use
///
///     for x in p do body x
///
/// instead of
///
///     do! for_ p (fun x -> body x)
///
///                        .---->  b
///                       /        |
///       +-----+        /     +---v----+           +-------------+
///    x =>  p  => b  --'   x =>  body  => c  =  x => for_ p body => c
///       +--|--+              +---|----+           +------|------+
///          v                     v                       v
///          r                    unit                     r
let inline for_
  (p: Pipe<'x, 'b, 'r>)
  (body: 'b -> Pipe<'x, 'c, unit>) = Builder.HelperFunctions.forPipeImpl(body, p)

/// Compose loop bodies.
///
///          a          .------>  b                   a
///          |          |         |                   |
///       +--v--+       |      +--|--+           +----|----+
///    x =>  f  => b  --'   x =>  g  => c  =  x => f ^~> g => c
///       +--|--+              +--|--+           +----|----+
///          v                    v                   v
///          r                   unit                 r
let inline ( ^~> )
  (f: 'a -> Pipe<'x, 'b, 'r>)
  (g: 'b -> Pipe<'x, 'c, unit>)
  (a: 'a) = for_ (f a) g
let inline ( <~^ ) g f = f >^ g

/// waits for an input of type ``'a``.
///
///    let! a = await_<'a, _>
let await_<'a, 'b> : Pipe<'a, 'b, 'a> = Await Done

/// ``p @~> q`` loops over ``q`` replacing each ``let! x = await_`` with ``let! x = p``.
///
///       +-----+               +-----+           +----+----+
///    a =>  p  => y   .-->  b =>  q  => y  =  a => p @~> q => y
///       +--|--+     /         +--|--+           +----|----+
///          v       /             v                   v
///          b  ----'              c                   c
let rec ( @~> )
  (p: Pipe<'a, 'y, 'b>)
  (q: Pipe<'b, 'y, 'c>)
  :   Pipe<'a, 'y, 'c> =
  match q with
    | Yield (o, q) -> Yield (o, p @~> q)
    | Await f      -> p >>= fun b -> p @~> f b
    | Delay f      -> Delay (fun () -> p @~> f())
    | Done x       -> Done x
let inline ( <~@ ) q p = p @~> q

/// the identity Pipe, analogous to the Unix cat program.
let cat_<'a, 'r> : Pipe<'a, 'a, 'r> =
  let rec cat () = Await (fun x -> Yield (x, cat ()))
  cat ()

/// Pipe composition, analogous to the Unix pipe operator.
///
///       +-----+      +-----+           +---------+
///    a =>  f  => b  =>  g  => c  =  a => f >-> g => c
///       +--|--+      +--|--+           +----|----+
///          v            v                   v
///          r            r                   r
let rec ( >-> )
  (f: Pipe<'a, 'b, 'r>)
  (g: Pipe<'b, 'c, 'r>)
  :   Pipe<'a, 'c, 'r> =
  match f, g with
    | Delay fthunk,  Delay gthunk  -> Delay (fun () -> fthunk () >-> gthunk ())
    | Delay fthunk,  _             -> Delay (fun () -> fthunk () >-> g)
    | _,             Delay gthunk  -> Delay (fun () -> f >-> gthunk ())
    | _,             Done x        -> Done x
    | _,             Yield (c, g') -> Yield (c, f >-> g')
    | Yield (b, f'), Await gfun    -> f' >-> gfun b
    | Await ffun,    _             -> Await (fun a -> ffun a >-> g)
    | Done x,        _             -> Done x
let inline ( <-< ) g f = f >-> g
// >>> : fixing vscode sydntax highlighting here

type Pipe<'In, 'Out, 'Result> with
  static member inline get_Zero() = Done ()
  static member inline (+) (p1, p2) = p1 *> p2
  static member (<*>) (pf, px) =
    let rec go = function
      | Yield (o, p) -> Yield (o, go p)
      | Await f      -> Await (fun x  -> f x  |> go)
      | Delay f      -> Delay (fun () -> f () |> go)
      | Done  f      -> mapResult f px
    go pf

[<Struct>]
type PipeStatus<'In, 'Out, 'Result> =
  | Awaiting of next:  ('In -> Pipe<'In, 'Out, 'Result>)
  | Exited   of result:'Result

module Pipe =
  /// replaces every ``let! x = await_`` with ``let x = c``.
  let rec    inputConst c pipe : Producer<_, _> =
    match pipe with
      | Yield (o, p) -> Yield (o, p |> inputConst c)
      | Await f      -> f c |> inputConst c
      | Delay f      -> Delay (fun () -> f () |> inputConst c)
      | Done  x      -> Done x
  /// removes every ``do! await_``.
  let inline skipUnitAwait pipe : Producer<_, _> = inputConst () pipe
  /// removes every ``yield x``.
  let rec    ignoreOutput pipe : Consumer<_, _> =
    match pipe with
      | Yield (_, p) -> ignoreOutput p
      | Await f      -> Await (fun x ->  f x  |> ignoreOutput)
      | Delay f      -> Delay (fun () -> f () |> ignoreOutput)
      | Done  x      -> Done x
  
  let inline weakenProducer (producer: Producer<_, _>) = weakenProducer producer
  let inline weakenConsumer (consumer: Consumer<_, _>) = weakenConsumer consumer
  let inline weakenEffect   (effect: Effect<_>)        = weakenEffect   effect

  let inline mapInput f pipe = mapInput f pipe
  let inline mapOutput f pipe = mapOutput f pipe
  let inline mapResult f pipe = mapResult f pipe

  /// Consume the first value from a Producer.
  ///
  /// ``next`` either fails with a ``Error`` if the Producer terminates or succeeds with an ``Ok`` providing the next value and the remainder of the Producer.
  let rec next (producer: Producer<_, _>) =
    match producer with
      | Yield (o, p) -> Ok (o, p)
      | Await _      -> undefined ()
      | Delay f      -> f () |> next
      | Done  r      -> Error r

  /// pulls as many ``yield_``s as possible until it reaches ``await_`` or ``return_``.
  let pull pipe =
    let inline toArrRev count (list: _ list) =
      let arr = Array.zeroCreate count
      let mutable i = 0
      for item in list do
        i <- i + 1
        arr.[count - i] <- item
      arr
    let rec pull count outputsRev pipe =
      match pipe with
        | Yield (o, p) -> pull (count+1) (o :: outputsRev) p
        | Delay f -> f() |> pull count outputsRev
        | Await f ->
          toArrRev count outputsRev, Awaiting f
        | Done x ->
          toArrRev count outputsRev, Exited x
    pull 0 [] pipe

  let inline ofSeq   (xs: seq<_>) = pipe { for x in xs do yield x }
  let inline ofArray (xs: _[])    = forArray yield_ xs ()
  let rec    ofList  (xs: _ list) =
    match xs with
      | x :: xs -> Yield (x, ofList xs)
      | [] -> Done ()
  let rec    toSeq   (p: Producer<_, _>) =
    seq {
      match p with
        | Yield (o, p) -> yield o; yield! toSeq p
        | Delay f -> yield! toSeq <| f()
        | Await _ -> undefined ()
        | Done  _ -> ()
    }
  let inline toArray (p: Producer<_, _>) = pull p |> fst
  let inline toList  (p: Producer<_, _>) = toArray p |> List.ofArray
  
  let rec    runEffect   (effect: Effect<_>) =
    match effect with
      | Delay f -> runEffect <| f()
      | Done  x -> x
      | _ -> undefined ()
  let inline runProducer (producer: Producer<_, _>) =
    match pull producer with
      | xs, Exited result -> xs, result
      | _                 -> undefined ()

open Pipe

let nats<'a> : Pipe<'a, int, unit> =
  pipe {
    let mutable i = 0
    while true do
      yield i
      i <- i + 1
  }

let inline sum () =
  let rec sum x = pipe {
    let! y = await_
    let  x = x + y
    yield  x
    yield! sum x
  }
  sum LanguagePrimitives.GenericZero

let rec take n = pipe {
  if n = 0 then return ()
  else
    let! x = await_
    yield x
    yield! take (n-1)
}
  
let test n = nats >-> sum () >-> take n |> toArray |> printfn "%A"
let test2 n =
  let rec sumN n p =
    if n = 0 then p else sumN (n-1) (p >-> sum ())
  nats |> sumN n >-> take 10 |> toArray |> printfn "%A"
  
// test 5
// test2 5

let xs : Producer<_, _> = pipe { for i = 1 to 100 do yield i * i }
let mutable ys = xs
for i = 1 to 1000 do ys <- ys *> xs
ys |> Pipe.toArray

