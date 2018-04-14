(*
The X11 License
bigint.fs - bitint reimplementation in F#, for study
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

module Study.BigInteger
open System.Text
open System.Collections.Generic

// .NET built-in implementation
module DotNet = begin
  open System.Numerics
  let inline fact x =
    let rec f x acc =
      if x <= 1 then acc else f (x - 1) (acc * bigint x)
     in
    f x (bigint 1)
end

[<Literal>]
let I = 1000000000000000000L

type BigIntType = Zero | Pos | Neg

type BigInt = struct
  val Type : BigIntType
  val Value : int64 list
  new (t, v) = { Type = t; Value = v }
  override this.ToString() =
    let sb = StringBuilder () in
    let rest =
      match (this.Value |> List.rev) with
        | h :: t ->
          h.ToString() |> sb.Append |> ignore; t
        | [] -> []
    in
    for x in rest do
      let s = x.ToString() in
      let s = String.init (18 - s.Length) (fun _ -> "0") + s in
      s |> sb.Append |> ignore
    done
    let s = sb.ToString() in
    match this.Type with
      | Zero -> "0"
      | Neg -> "-" + s
      | _ -> s
end

let inline bigint x =
  if x = 0 then new BigInt (Zero, [])
  else
    let (pm, a) = if x >= 0 then (Pos, int64 x) else (Neg, int64 -x) in
    if a >= I then
      new BigInt (pm, [a % I; a / I])
    else
      new BigInt (pm, [a])

let BZero = bigint 0

module My = begin
  let rec add x y  =
    match (x, y) with
      | (xs, []) -> xs
      | ([], ys) -> ys
      | (x0 :: [], y0 :: []) ->
        let s = x0 + y0 in
        if s >= I then
          (s % I) :: [s / I]
        else
          [s]
      | (x0 :: x1 :: xr, y0 :: yr)
      | (y0 :: yr, x0 :: x1 :: xr) ->
        let s = x0 + y0 in
        if s >= I then
          (s % I) :: add (x1 + s / I :: xr) yr
        else
          s :: add (x1 :: xr) yr

  let rec sub x y =
    match (x, y) with
      | (xs, []) -> (true, xs)
      | ([], ys) -> (false, ys)
      | (x :: [], y :: []) when x = y -> (true, [])
      | (x :: [], y :: []) ->
        let pm = x >= y in
        let s = if pm then x - y else y - x in
        if s >= I then (pm, s % I :: [s / I]) else (pm, [s])
      | (x0 :: x1 :: xr, y0 :: yr) ->
        let pm = x >= y in
        let s = if pm then x0 - y0 else y0 - x0 in
        let (s, x1) = 
          if s >= I then
            let a = s / I in
            if pm then (s % I, x1 + a) else (s % I, x1 - a)
          else (s, x1)
        in
        let (pm, v) = sub (x1 :: xr) yr in
        (pm, s :: v)
      | (x0 :: [], y0 :: y1 :: yr) ->
        if x0 >= y0 then
          (false, x0 - y0 :: y1 :: yr)
        else
          let s = y0 - x0 in
          if s >= I then
            (false, s % I :: y1 + s / I :: yr)
          else
            (false, s :: y1 :: yr)

  let rec mul xb yl =
    match xb with
      | [] -> []
      | x0 :: xr ->
        let rec f y =
          let p = x0 * (y%10L) in
          let pl = if p >= I then p % I :: [p / I] else [p] in
          if y >= 10L then
            let inline rep f i x =
              let xr = ref x in
              for _ in 1 .. i do
                xr := f (!xr)
              done; !xr
            in
            let pll = let a = f (y/10L) in add a a in
            rep (add pll) 4 pll |> add pl
          else pl
        in
        add (f yl) (0L :: mul xr yl)

  let inline (+) (x : BigInt) (y : BigInt) =
    match (x.Type, y.Type) with
      | (Zero, Zero) -> new BigInt (Zero, [])
      | (Zero, _) -> y
      | (_, Zero) -> x
      | (Pos, Pos) -> new BigInt (Pos, add x.Value y.Value)
      | (Neg, Neg) -> new BigInt (Neg, add x.Value y.Value)
      | (pmx, pmy) ->
        let (pm, v) = sub x.Value y.Value in
        if List.isEmpty v then new BigInt (Zero, [])
        else if pm then new BigInt (pmx, v)
        else new BigInt (pmy, v)
  
  let inline (*) (x : BigInt) y =
    let (pmx, y) = 
      match (x.Type, y >= 0L) with
        | (Pos, false) -> (Neg, -y)
        | (Neg, false) -> (Pos, -y)
        | (t, _) -> (t, y)
    in
    new BigInt (pmx, mul x.Value y)

  let inline fact x =
    let rec f x acc =
      if x = 0L then acc else f (x - 1L) (acc * x)
    in
    f x (bigint 1)
end


let inline time f x =
  let times = 
    seq {
      for _ in 1 .. 50 do
        let stopWatch = System.Diagnostics.Stopwatch.StartNew() in
        do f x |> ignore;
        do stopWatch.Stop();
        yield stopWatch.Elapsed.TotalMilliseconds
    }
  in
  printfn "%A: %gms" f (Seq.average times);
  f x
in

// .NET
let dnFact1000 = (time DotNet.fact 1000).ToString() in

// my own
let myFact1000 = (time My.fact 1000L).ToString() in
5

myFact1000 = dnFact1000 |> printfn "myFact1000 = dnFact1000: %A";
printfn "";
printfn "1000! = %s" myFact1000
