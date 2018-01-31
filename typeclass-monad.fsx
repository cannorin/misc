(*
The X11 License
typeclass-monad.fsx - example implementation of Monad type class
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
