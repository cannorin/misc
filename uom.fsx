(*
The MIT License
uom.fsx - SI units of measure
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

module SIUnits =
  [<AutoOpen>]
  module SIPrefixes =
    [<Measure>] type _deca
    let deca =  0.1<_deca>
    [<Measure>] type _deci = 1 / _deca
    let deci =  10.0<_deci>
    [<Measure>] type _hecto = _deca ^ 2
    let hecto = 0.01<_hecto>
    [<Measure>] type _centi = 1 / _deca^2 
    let centi = 100.0<_centi>
    [<Measure>] type _kilo = _deca ^ 3
    let kilo =  0.001<_kilo>
    [<Measure>] type _milli = 1 / _deca^3
    let milli = 1000.0<_milli>
    [<Measure>] type _mega = _deca ^ 6
    let mega =  0.000001<_mega>
    [<Measure>] type _micro = 1 / _deca^6
    let micro = 1000000.0<_micro>
    [<Measure>] type _giga = _deca ^ 9
    let giga =  0.000000001<_giga>
    [<Measure>] type _nano = 1 / _deca^9
    let nano =  10000000000.0<_nano>

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
    [<Measure>] type Celsius = K
    let toKelvin (x: float<Celsius>) = x * 1.0<K/Celsius> + 273.15<K>
    let toCelsius (x: float<K>) = x * 1.0<Celsius/K> - 273.15<Celsius>
    [<Measure>] type lm = cd*sr
    [<Measure>] type lx = lm / m^2
    [<Measure>] type Bq = 1/s
    [<Measure>] type Gy = J/kg
    [<Measure>] type Sv = Gy
    let toSievert mdfr (x: float<Gy>) = x * 1.0<Sv/Gy> * mdfr
    [<Measure>] type kat = s^-1 * mol

  [<AutoOpen>]
  module NonSIUnits =
    [<Measure>] type L = _deci^3 * m^3
    [<Measure>] type mL = _milli*L
    [<Measure>] type ha = _hecto * m^2
    [<Measure>] type AU = m
    let toMetre (x: float<AU>) = x * 1.495978707<m/AU> * pown 10.0 11
    let toAU (x: float<m>) = x / 1.495978707<AU/m> * pown 10.0 -11
    [<Measure>] type t = _mega * g
    [<Measure>] type min = s
    [<Measure>] type hour = s
    [<Measure>] type day = s
    [<Measure>] type year = s
    let second = 1.0<s>
    let minute = second / 60.0<s/min>
    let hour   = minute / 60.0<min/hour>
    let day    = hour / 24.0<hour/day>
    let year   = day / 365.0<day/year>

module private UomExamples =
  open SIUnits

  let ``Metric prefix conversion`` () =
    let a_1L = 1.0<L>
    let a_1000mL = milli * a_1L
    printfn "%AL = %AmL" a_1L a_1000mL
    printfn ""

  let ``1AU in km`` () =
    let printKm (f: float<km>) =
      printfn "1AU = %.1fkm" f
    // printKm 1.0<AU> (* compilation error *)
    // printKm (1.0<AU> |> toMetre) (* compilation error *)
    (1.0<AU> |> toMetre) * kilo |> printKm
    printfn ""

  let ``Comparing millilitre values`` () =
    let a_1mL = 1.0<_milli L>
    let b_1mL = 1.0<cm^3>
    let c_1mL = 0.001<L>

    let compareML (a: float<mL>) (b: float<mL>) =
      printfn "%AmL %s %AmL" a (if a = b then "=" else "<>") b

    compareML a_1mL b_1mL
    // compareML a_1mL c_1mL (* compilation error *)
    printfn ""

  let ``Equations of motion`` () =
    let f = 5.0<N>
    let m = 4.0<g>

    let printAcc (a: float<m/s^2>) =
      printfn "%AN = %Ag * %Am/s^2" f m a

    printAcc (f / (kilo * m))
    // printAcc (f / m) (* compilation error *)
    printfn ""

  let ``Kilchhoff's circuit laws`` () =
    let v = 4.0<V>
    let i = 0.5<A>
    let r = 5.0<Ohm>
    let q = 0.3<C>
    let c = 0.2<F>
    let kilchhoff = v - (r * i) - (q / c)
    printfn "v - r*i - q/c = %.1f" kilchhoff
    printfn ""

  let ``various units conversion`` () =
    40.0<N> - 400.0<g> * kilo * 9.8<m/s^2>                                  // accelerating object
            - 4.0<C> * 3.6<km/hour> / kilo * (hour/second) * 1.0<T>         // lorentz force
            - 1.013<hPa> / hecto * 0.5<m> * 0.5<m>                          // air pressure
            - 5.0<cm> * 6.0<cm> * 15.0<cm> / milli * 1.0<kg/L> * 9.8<m/s^2> // water weight
            |> printfn "%.1fN"

