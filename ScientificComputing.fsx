(*
The MIT License
ScientificComputing.fsx - SI units of measure
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

open System
open LanguagePrimitives

module MathExtra =
  type Pow2 = Pow2 with
    static member inline ($) (Pow2, x:sbyte<_>)   = x * x
    static member inline ($) (Pow2, x:int<_>)     = x * x
    static member inline ($) (Pow2, x:int16<_>)   = x * x
    static member inline ($) (Pow2, x:int64<_>)   = x * x
    static member inline ($) (Pow2, x:float32<_>) = x * x
    static member inline ($) (Pow2, x:float<_>)   = x * x
    static member inline ($) (Pow2, x:decimal<_>) = x * x

  let inline pow2 x = Pow2 $ x

  let rec sqrtDecimal (x: decimal, guess: decimal option) =
    let guess = guess |> Option.defaultValue (x/2m)
    let result = x / guess
    let avg = (guess + result) / 2m
    if avg = guess then avg else sqrtDecimal (x, Some avg)

  type Sqrt = Sqrt with
    static member inline ($) (Sqrt, x:^x) = sqrt x
    static member inline ($) (Sqrt, x:decimal<'m>) =
      let x = decimal x
      sqrtDecimal (x, None) * DecimalWithMeasure<'m ^ (1/2)> 1.0m

  let inline sqrt x = Sqrt $ x

  module ScalarArithmeticOperators =
    type Scalar = Scalar with
      static member inline (+) (Scalar, x:sbyte<_>)   = fun y -> x + sbyte y * SByteWithMeasure<_> 1y
      static member inline (+) (Scalar, x:int<_>)     = fun y -> x + int y * Int32WithMeasure<_> 1
      static member inline (+) (Scalar, x:int16<_>)   = fun y -> x + int16 y * Int16WithMeasure<_> 1s
      static member inline (+) (Scalar, x:int64<_>)   = fun y -> x + int64 y * Int64WithMeasure<_> 1L
      static member inline (+) (Scalar, x:float32<_>) = fun y -> x + float32 y * Float32WithMeasure<_> 1.0f
      static member inline (+) (Scalar, x:float<_>)   = fun y -> x + float y * FloatWithMeasure<_> 1.0
      static member inline (+) (Scalar, x:decimal<_>) = fun y -> x + decimal y * DecimalWithMeasure<_> 1m
      static member inline (+) (Scalar, x:^X)         = fun y -> (^X: (member Add: ^Y -> ^X) x,y)
      static member inline (+) (Scalar, (x1, x2))     = fun y -> ((Scalar + x1) y, (Scalar + x2) y)
      static member inline (*) (Scalar, x:sbyte<_>)   = fun y -> x * sbyte y
      static member inline (*) (Scalar, x:int<_>)     = fun y -> x * int y
      static member inline (*) (Scalar, x:int16<_>)   = fun y -> x * int16 y
      static member inline (*) (Scalar, x:int64<_>)   = fun y -> x * int64 y
      static member inline (*) (Scalar, x:float32<_>) = fun y -> x * float32 y
      static member inline (*) (Scalar, x:float<_>)   = fun y -> x * float y
      static member inline (*) (Scalar, x:decimal<_>) = fun y -> x * decimal y
      static member inline (*) (Scalar, x:^X)         = fun y -> (^X: (member Multiply: ^Y -> ^X) x,y)
      static member inline (*) (Scalar, (x1, x2))     = fun y -> ((Scalar * x1) y, (Scalar * x2) y)
      static member inline (/@>) (Scalar, x:sbyte<_>)   = fun y -> x / sbyte y
      static member inline (/@>) (Scalar, x:int<_>)     = fun y -> x / int y
      static member inline (/@>) (Scalar, x:int16<_>)   = fun y -> x / int16 y
      static member inline (/@>) (Scalar, x:int64<_>)   = fun y -> x / int64 y
      static member inline (/@>) (Scalar, x:float32<_>) = fun y -> x / float32 y
      static member inline (/@>) (Scalar, x:float<_>)   = fun y -> x / float y
      static member inline (/@>) (Scalar, x:decimal<_>) = fun y -> x / decimal y
      static member inline (/@>) (Scalar, x:^X)         = fun y -> (^X: (member Divide: ^Y -> ^X) x,y)
      static member inline (/@>) (Scalar, (x1, x2))     = fun y -> ((Scalar /@> x1) y, (Scalar /@> x2) y)
      static member inline (/@<) (Scalar, x:sbyte<_>)   = fun y -> sbyte y / x
      static member inline (/@<) (Scalar, x:int<_>)     = fun y -> int y / x
      static member inline (/@<) (Scalar, x:int16<_>)   = fun y -> int16 y / x
      static member inline (/@<) (Scalar, x:int64<_>)   = fun y -> int64 y / x
      static member inline (/@<) (Scalar, x:float32<_>) = fun y -> float32 y / x
      static member inline (/@<) (Scalar, x:float<_>)   = fun y -> float y / x
      static member inline (/@<) (Scalar, x:decimal<_>) = fun y -> decimal y / x
      static member inline (/@<) (Scalar, x:^X)         = fun y -> (^X: (member DivideByThis: ^Y -> ^X) x,y)
      static member inline (/@<) (Scalar, (x1, x2))     = fun y -> ((Scalar /@< x1) y, (Scalar /@< x2) y)

    let inline ( +. ) x y = (Scalar + x) y
    let inline ( .+ ) x y = (Scalar + y) x
    let inline ( -. ) x y = (Scalar + x) -y
    let inline ( .- ) x y = (Scalar + y) -x
    let inline ( *. ) x y = (Scalar * x) y
    let inline ( .* ) x y = (Scalar * y) x
    let inline ( /. ) x y = (Scalar /@> x) y
    let inline ( ./ ) x y = (Scalar /@< y) x

open MathExtra

module UnitsOfMeasure =
  open MathExtra.ScalarArithmeticOperators

  type conversion< [<Measure>] 'measure, 'value, 'converter > = Conversion of 'value with
    static member inline (*) (x: sbyte<'n>, v: conversion<'m, 'v, ^Conv>)   = (^Conv: (static member Apply: _ * _ -> sbyte<'n 'm>) x,v)
    static member inline (*) (x: int<'n>, v: conversion<'m, 'v, ^Conv>)     = (^Conv: (static member Apply: _ * _ -> int<'n 'm>) x,v)
    static member inline (*) (x: int16<'n>, v: conversion<'m, 'v, ^Conv>)   = (^Conv: (static member Apply: _ * _ -> int16<'n 'm>) x,v)
    static member inline (*) (x: int64<'n>, v: conversion<'m, 'v, ^Conv>)   = (^Conv: (static member Apply: _ * _ -> int64<'n 'm>) x,v)
    static member inline (*) (x: float<'n>, v: conversion<'m, 'v, ^Conv>)   = (^Conv: (static member Apply: _ * _ -> float<'n 'm>) x,v)
    static member inline (*) (x: float32<'n>, v: conversion<'m, 'v, ^Conv>) = (^Conv: (static member Apply: _ * _ -> float32<'n 'm>) x,v)
    static member inline (*) (x: decimal<'n>, v: conversion<'m, 'v, ^Conv>) = (^Conv: (static member Apply: _ * _ -> decimal<'n 'm>) x,v)
    static member inline (*) (v: conversion<'m, 'v, ^Conv>, x: sbyte<'n>)   = (^Conv: (static member Apply: _ * _ -> sbyte<'n 'm>) x,v)
    static member inline (*) (v: conversion<'m, 'v, ^Conv>, x: int<'n>)     = (^Conv: (static member Apply: _ * _ -> int<'n 'm>) x,v)
    static member inline (*) (v: conversion<'m, 'v, ^Conv>, x: int16<'n>)   = (^Conv: (static member Apply: _ * _ -> int16<'n 'm>) x,v)
    static member inline (*) (v: conversion<'m, 'v, ^Conv>, x: int64<'n>)   = (^Conv: (static member Apply: _ * _ -> int64<'n 'm>) x,v)
    static member inline (*) (v: conversion<'m, 'v, ^Conv>, x: float<'n>)   = (^Conv: (static member Apply: _ * _ -> float<'n 'm>) x,v)
    static member inline (*) (v: conversion<'m, 'v, ^Conv>, x: float32<'n>) = (^Conv: (static member Apply: _ * _ -> float32<'n 'm>) x,v)
    static member inline (*) (v: conversion<'m, 'v, ^Conv>, x: decimal<'n>) = (^Conv: (static member Apply: _ * _ -> decimal<'n 'm>) x,v)
    static member inline (*) ((x: ^x, y: ^y), v: ^conv)                     = (x*v, y*v)
    static member inline (*) (v: ^conv, (x: ^x, y: ^y))                     = (x*v, y*v)
    static member inline (/) (x: sbyte<'n>, v: conversion<'m, 'v, ^Conv>)   = (^Conv: (static member Elim: _ * _ -> sbyte<'n/'m>) x,v)
    static member inline (/) (x: int<'n>, v: conversion<'m, 'v, ^Conv>)     = (^Conv: (static member Elim: _ * _ -> int<'n/'m>) x,v)
    static member inline (/) (x: int16<'n>, v: conversion<'m, 'v, ^Conv>)   = (^Conv: (static member Elim: _ * _ -> int16<'n/'m>) x,v)
    static member inline (/) (x: int64<'n>, v: conversion<'m, 'v, ^Conv>)   = (^Conv: (static member Elim: _ * _ -> int64<'n/'m>) x,v)
    static member inline (/) (x: float<'n>, v: conversion<'m, 'v, ^Conv>)   = (^Conv: (static member Elim: _ * _ -> float<'n/'m>) x,v)
    static member inline (/) (x: float32<'n>, v: conversion<'m, 'v, ^Conv>) = (^Conv: (static member Elim: _ * _ -> float32<'n/'m>) x,v)
    static member inline (/) (x: decimal<'n>, v: conversion<'m, 'v, ^Conv>) = (^Conv: (static member Elim: _ * _ -> decimal<'n/'m>) x,v) 
    static member inline (/) ((x: ^x, y: ^y), v: ^conv)                     = (x/v, y/v)
    static member inline (/) (v: ^conv, (x: ^x, y: ^y))                     = (x/v, y/v)
    static member inline (*) (m: conversion<'m, 'v, ^Conv>, n: conversion<'n, 'v, ^Conv>) =
      (^Conv: (static member Multiply: _ * _ -> conversion<'m 'n, 'v, ^Conv>) m,n)
    static member inline (/) (m: conversion<'m, 'v, ^Conv>, n: conversion<'n, 'v, ^Conv>) =
      (^Conv: (static member Divide: _ * _ -> conversion<'m/'n, 'v, ^Conv>) m,n)

  type MeasureConverter = MeasureConverter
  type measure<[<Measure>] 'measure> = conversion<'measure, unit, MeasureConverter>
  let inline measure< [<Measure>] 'm > : measure<'m> = Conversion ()
  type MeasureConverter with
    static member inline Apply (x: sbyte<'n>, _: measure<'m>)   = x * SByteWithMeasure<'m> 1y
    static member inline Apply (x: int<'n>, _: measure<'m>)     = x * Int32WithMeasure<'m> 1
    static member inline Apply (x: int16<'n>, _: measure<'m>)   = x * Int16WithMeasure<'m> 1s
    static member inline Apply (x: int64<'n>, _: measure<'m>)   = x * Int64WithMeasure<'m> 1L
    static member inline Apply (x: float<'n>, _: measure<'m>)   = x * FloatWithMeasure<'m> 1.0
    static member inline Apply (x: float32<'n>, _: measure<'m>) = x * Float32WithMeasure<'m> 1.0f
    static member inline Apply (x: decimal<'n>, _: measure<'m>) = x * DecimalWithMeasure<'m> 1.0m
    static member inline Elim (x: sbyte<'n>, _: measure<'m>)   = x / SByteWithMeasure<'m> 1y
    static member inline Elim (x: int<'n>, _: measure<'m>)     = x / Int32WithMeasure<'m> 1
    static member inline Elim (x: int16<'n>, _: measure<'m>)   = x / Int16WithMeasure<'m> 1s
    static member inline Elim (x: int64<'n>, _: measure<'m>)   = x / Int64WithMeasure<'m> 1L
    static member inline Elim (x: float<'n>, _: measure<'m>)   = x / FloatWithMeasure<'m> 1.0
    static member inline Elim (x: float32<'n>, _: measure<'m>) = x / Float32WithMeasure<'m> 1.0f
    static member inline Elim (x: decimal<'n>, _: measure<'m>) = x / DecimalWithMeasure<'m> 1.0m
    static member inline Multiply (_: measure<'m>, _: measure<'n>) = measure<'m 'n>
    static member inline Divide   (_: measure<'m>, _: measure<'n>) = measure<'m/'n>

  type MetricConverter = MetricConverter
  type metric< [<Measure>] 'measure > = conversion<'measure, int, MetricConverter>
  let inline metric< [<Measure>] 'm > m : metric<'m> = Conversion m
  type MetricConverter with
    /// x * 10^n
    static member inline Apply (x, Conversion m : metric<'m>) =
      measure<'m> * (
        if m = 0 then x
        else if m > 0 then x *. pown 10 m
        else x /. pown 10 (-m)
      )
    /// x * 10^n
    static member inline Elim (x, Conversion m : metric<'m>) =
      measure<1/'m> * (
        if m = 0 then x
        else if m < 0 then x *. pown 10 (-m)
        else x /. pown 10 m
      )
    static member inline Multiply (Conversion m: metric<'m>, Conversion n: metric<'n>) = metric<'m 'n> (m+n) 
    static member inline Divide   (Conversion m: metric<'m>, Conversion n: metric<'n>) = metric<'m/'n> (m-n)

  type CustomConverter = CustomConverter
  [<Struct>]
  type ConversionType = CTAdd of a:float | CTMul of m:float
  type converter< [<Measure>] 'measure > = conversion<'measure, ConversionType, CustomConverter>
  let inline converter<[<Measure>] 'measure> x : converter<'measure> = Conversion x
  type CustomConverter with
    static member inline Apply (x, Conversion m: converter<'m>) =
      measure<'m> * (
        match m with
          | CTAdd y -> x +. y
          | CTMul y -> x *. y
      )
    static member inline Elim  (x, Conversion m: converter<'m>) =
      measure<'m> * (
        match m with
          | CTAdd y -> x -. y
          | CTMul y -> x /. y
      )
    static member inline Multiply (Conversion m: converter<'m>, Conversion n: converter<'n>) =
      match m,n with
        | CTAdd x, CTAdd y -> converter<'m 'n> (CTAdd(x+y))
        | CTMul x, CTMul y -> converter<'m 'n> (CTMul(x*y))
        | _, _ -> failwith "converter type mismatch"
    static member inline Divide   (Conversion m: converter<'m>, Conversion n: converter<'n>) =
      match m,n with
        | CTAdd x, CTAdd y -> converter<'m/'n> (CTAdd(x+y))
        | CTMul x, CTMul y -> converter<'m/'n> (CTMul(x*y))
        | _, _ -> failwith "converter type mismatch"

  [<AutoOpen>]
  module MetricPrefixes =
    [<Measure>] type _deca
    let deca =  metric<_deca> -1
    [<Measure>] type _deci = 1 / _deca
    let deci =  metric<_deci> 1
    [<Measure>] type _hecto = _deca ^ 2
    let hecto = metric<_hecto> -2
    [<Measure>] type _centi = 1 / _deca^2 
    let centi = metric<_centi> 2
    [<Measure>] type _kilo = _deca ^ 3
    let kilo =  metric<_kilo> -3
    [<Measure>] type _milli = 1 / _deca^3
    let milli = metric<_milli> 3
    [<Measure>] type _mega = _deca ^ 6
    let mega =  metric<_mega> -6
    [<Measure>] type _micro = 1 / _deca^6
    let micro = metric<_micro> 6
    [<Measure>] type _giga = _deca ^ 9
    let giga =  metric<_giga> -9
    [<Measure>] type _nano = 1 / _deca^9
    let nano =  metric<_nano> 9
    [<Measure>] type _tera = _deca^12
    let tera =  metric<_tera> -12
    [<Measure>] type _pico = 1 / _deca^12
    let pico =  metric<_pico> 12

  module SIUnits =
    [<AutoOpen>]
    module SIBaseUnits =
      [<Measure>] type m
      [<Measure>] type g
      [<Measure>] type s
      [<Measure>] type A
      [<Measure>] type K
      [<Measure>] type cd
      [<Measure>] type mol

    [<AutoOpen>]
    module SIBaseUnitsWithCommonPrefix =
      [<Measure>] type km = _kilo * m
      [<Measure>] type cm = _centi * m
      [<Measure>] type mm = _milli * m
      [<Measure>] type kg = _kilo * g
      [<Measure>] type mg = _milli * g
      [<Measure>] type ms = _milli * s
      [<Measure>] type mA = _milli * A
      [<Measure>] type kA = _kilo * A
      [<Measure>] type mmol = _milli * mol

    [<AutoOpen>]
    module SIDerivedUnits =
      [<Measure>] type rad = 1
      [<Measure>] type sr = 1
      [<Measure>] type Hz = 1/s
      [<Measure>] type N = kg * m * s^-2
      [<Measure>] type Pa = N / m^2
      [<Measure>] type hPa = _hecto Pa
      [<Measure>] type J = N*m
      [<Measure>] type W = J/s
      [<Measure>] type C = s*A
      [<Measure>] type V = W/A
      [<Measure>] type F = C/V
      [<Measure>] type Ohm = V/A
      [<Measure>] type S = A/V
      [<Measure>] type Wb = V*s
      [<Measure>] type T = Wb / m^2 
      [<Measure>] type H = Wb/A
      [<Measure>] type Celsius
      [<Measure>] type lm = cd*sr
      [<Measure>] type lx = lm / m^2
      [<Measure>] type Bq = 1/s
      [<Measure>] type Gy = J/kg
      [<Measure>] type Sv
      [<Measure>] type kat = s^-1 * mol

    [<AutoOpen>]
    module NonSIUnits =
      [<Measure>] type L = _deci^3 * m^3
      [<Measure>] type mL = _milli*L
      [<Measure>] type ha = _hecto * m^2
      [<Measure>] type AU
      [<Measure>] type t = _mega * g
      [<Measure>] type min
      [<Measure>] type hour
      [<Measure>] type day
      [<Measure>] type year

    module SIConvertion =
      let KelvinCelsius = converter<K/Celsius>(CTAdd 273.15)
      let inline GraySievert mdfr = converter<Sv/Gy>(CTMul mdfr)
      let MetreAU = converter<AU/m>(CTMul (1.495978707 * pown 10.0 -11))
      let second = converter<s>(CTMul 1.0)
      let minute = converter<min>(CTMul (1.0/60.0))
      let hour   = converter<hour>(CTMul (1.0/3600.0))
      let day    = converter<day>(CTMul (1.0/86400.0))
      let year   = converter<year>(CTMul (1.0/31536000.0))
