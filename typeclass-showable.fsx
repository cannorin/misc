(*
The X11 License
typeclass-showable.fsx - example implementation of Showable type class
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

module A =
  type Showable () =
    static member inline Show (s: ^S, x: ^T) : string when (^S or ^T) : (static member Show: ^T -> string) =
      ((^S or ^T): (static member Show: ^T -> string) x)
  let inline show (x: ^T) = Showable.Show (Showable(), x)

  type Showable with
    // instantiate an existing type
    static member inline Show (s: string) = s

  type Color = Red | Green | Blue | Other of int * int * int

  type Showable with
    static member inline Show (cl: Color) =
      match cl with
        | Red ->   "255, 0, 0"
        | Green -> "0, 255, 0"
        | Blue ->  "0, 0, 255"
        | Other (r, g, b) -> sprintf "%i, %i, %i" r g b
  
  let work () =
    "hello, world!!!!!" |> show |> printfn "%s"
    Other (100, 85, 50) |> show |> printfn "%s"

module B =
  open A
  type Human = Otaku of string | NotOtaku

  type Showable () = 
    inherit A.Showable () // <- pain 1: explicit inheritance needed
  let inline show (x: ^T) = Showable.Show (Showable(), x) // <- pain 2: re-definition needed

  type Str = Str of string
  
  type Showable with
    // instantiate an existing type
    static member inline Show (i: int) = sprintf "The ultimate answer is %i." i

    // pain 3: wrapper needed to override existing type class instances
    static member inline Show (Str s) = s.ToCharArray() |> Array.rev |> System.String.Concat

    static member inline Show (hm: Human) =
      match hm with
        | Otaku genre -> sprintf "I'm a %s otaku." genre
        | NotOtaku    -> sprintf "I'm not an otaku, really!"
  
  // alternative way of instantiation
  type Kirito = Kirito_Kun_Tasukete with
    static member inline Show (k: Kirito) = "Kirite kun tasuketo..."

  let work () =
    Str "hello again!!" |> show |> printfn "%s" // inherited from module A
    Other (255, 0, 255) |> show |> printfn "%s" // inherited from module A
    Otaku "programming" |> show |> printfn "%s"
    (8 + 8 + 8 + 9 + 9) |> show |> printfn "%s"
    Kirito_Kun_Tasukete |> show |> printfn "%s" 

A.work()
B.work()
