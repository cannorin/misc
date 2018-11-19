module Coroutine

[<Struct>]
type Coroutine<'a> = Coroutine of (unit -> CoroutineState<'a>)
and  CoroutineState<'a> =
  | Run of Coroutine<'a>
  | Done of 'a

module Coroutine =
  let rec bind (f: 'a -> Coroutine<'b>) (Coroutine g: Coroutine<'a>) : Coroutine<'b> =
    Coroutine (fun () ->
      match g () with
      | Run  h -> Run (bind f h)
      | Done x -> let (Coroutine h) = f x in h ()
    )
  let inline return' (x: 'a) : Coroutine<'a> = Coroutine (fun () -> Done x)
  let inline extend (f: Coroutine<'a> -> 'b) (m: Coroutine<'a>) : Coroutine<'b> = return' (f m)

  // evaluate until the next yield
  let inline next (c: Coroutine<'a>) : Coroutine<'a> = bind return' c
  // evaluate until the final yield
  let rec run (Coroutine c: Coroutine<'a>) : 'a =
    match c () with
    | Done x -> x
    | Run  h -> run h

// for F#+
type Coroutine<'a> with
  // Monad operations
  static member inline (>>=) (x, f) = Coroutine.bind f x
  static member inline Return x     = Coroutine.return' x
  // Comonad operations
  static member inline Extract c    = Coroutine.run c
  static member inline (=>>) (x, f) = Coroutine.extend f x
  static member inline Duplicate (m: Coroutine<_>)  = Coroutine.return' m

[<Struct>]
type CoroutineBuilder =
  member inline __.Bind(m, f) = Coroutine.bind f m
  member inline __.Yield x    = Coroutine.return' x
  member inline __.YieldFrom m = m
  member inline __.Combine (m, n) = Coroutine.bind (fun _ -> n) m
  member inline __.Delay f = Coroutine.bind f (Coroutine.return' ())

let coroutine = CoroutineBuilder ()

module private Example =
  let c1 = coroutine {
    yield 0
    do printfn "pause"
    yield 1
    do printfn "pause"
    yield 2
    do printfn "pause"
    yield 3
  }