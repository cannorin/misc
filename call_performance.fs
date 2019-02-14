let hoge1 a b c d e =
  (a + b) * (c + d) / (e + 1)

let hoge2 (a, b, c, d, e) =
  (a + b) * (c + d) / (e + 1)

let inline hoge3 a b c d e =
  (a + b) * (c + d) / (e + LanguagePrimitives.GenericOne<_>)

let inline hoge4 (a, b, c, d, e) =
  (a + b) * (c + d) / (e + LanguagePrimitives.GenericOne<_>)

let itercount = 100000000

let iter1 () =
  for i = 0 to itercount do
    hoge1 i i i i i |> ignore

let iter2 () =
  for i = 0 to itercount do
    hoge2 (i, i, i, i, i) |> ignore

let iter3 piyo =
  for i = 0 to itercount do
    piyo i i i i i |> ignore

let iter4 piyo =
  for i = 0 to itercount do
    piyo (i, i, i, i, i) |> ignore

let iter5 () =
  for i = 0 to itercount do
    hoge3 i i i i i |> ignore

let iter6 () =
  for i = 0 to itercount do
    hoge4 (i, i, i, i, i) |> ignore

let inline iter7 piyo =
  for i = 0 to itercount do
    piyo i i i i i |> ignore

let inline iter8 piyo =
  for i = 0 to itercount do
    piyo (i, i, i, i, i) |> ignore

let iter9 piyo =
  let f = OptimizedClosures.FSharpFunc<_,_,_,_,_,_>.Adapt(piyo)
  for i = 0 to itercount do
    f.Invoke(i, i, i, i, i) |> ignore

let inline iter10 piyo =
  let f = OptimizedClosures.FSharpFunc<_,_,_,_,_,_>.Adapt(piyo)
  for i = 0 to itercount do
    f.Invoke(i, i, i, i, i) |> ignore

let watch title =
  let watch = System.Diagnostics.Stopwatch()
  watch.Start()
  { new System.IDisposable with
      member this.Dispose() =
        watch.Stop()
        printfn "%s: elapsed = %d [ms]" title watch.ElapsedMilliseconds }
  
[<EntryPoint>]
let main argv = 
  do
    use __ = watch "iter1"
    iter1()

  do
    use __ = watch "iter2"
    iter2()

  do
    use __ = watch "iter3-normal"
    iter3 hoge1

  do
    use __ = watch "iter3-inline"
    iter3 hoge3

  do
    use __ = watch "iter4-normal"
    iter4 hoge2

  do
    use __ = watch "iter4-inline"
    iter4 hoge4

  do
    use __ = watch "iter5"
    iter5()

  do
    use __ = watch "iter6"
    iter6()

  do
    use __ = watch "iter7-normal"
    iter7 hoge1

  do
    use __ = watch "iter7-inline"
    iter7 hoge3

  do
    use __ = watch "iter8-normal"
    iter8 hoge2

  do
    use __ = watch "iter8-inline"
    iter8 hoge4

  do
    use __ = watch "iter9-normal"
    iter9 hoge1

  do
    use __ = watch "iter9-inline"
    iter9 hoge3
  
  do
    use __ = watch "iter10-normal"
    iter10 hoge1

  do
    use __ = watch "iter10-inline"
    iter10 hoge3

  0
