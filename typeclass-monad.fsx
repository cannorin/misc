module MonadTest

module Monad =

  type Monad () = 
    static member inline _bind (binder: ^a -> ^Mb, container: ^Ma, this: ^T) : ^Mb
      when ^Ma: (static member Bind: ^Ma -> (^a -> ^Mb) -> ^Mb) =
      (^Ma: (static member Bind: ^Ma -> (^a -> ^Mb) -> ^Mb) container,binder)
    static member inline _return (value: ^a, this: ^T) : ^Ma
      when ^Ma: (static member Return: ^a -> ^Ma) =
      (^Ma: (static member Return: ^a -> ^Ma) value)
  
  let inline bind (f: ^a -> ^Mb) (x: ^Ma) =
    Monad._bind (f, x, Monad())
  let inline return' x =
    Monad._return (x, Monad())
  
  type MonadBuilder() =
    member inline self.Bind (x, f) = bind f x
    member inline self.Return x = return' x
  
  let monad = MonadBuilder()

open Monad

type Maybe<'a> = Yes of 'a | No

type Maybe<'a> with
  static member inline Bind (container: Maybe<'a>, binder: 'a -> Maybe<'b>) : Maybe<'b> =
    match container with Yes x -> binder x | No -> No
  static member inline Return (value: 'a) : Maybe<'a> =
    Yes value

// instantiate an existing type w/ wrapper type 
// type extension doesn't work for types from foreign modules
type _option<'a> = OptionM of Option<'a> with
  static member inline Bind (OptionM c, b: 'a -> _option<'b>) =
    Option.bind (b >> function OptionM o -> o) c |> OptionM
  static member inline Return x = Some x |> OptionM

monad {
  let! a = Yes 1
  let! b = Yes 2
  return a + b
} |> printfn "%A"

monad {
  let! a = Some 1 |> OptionM
  let! b = Some 2 |> OptionM
  return a + b
} |> printfn "%A"
