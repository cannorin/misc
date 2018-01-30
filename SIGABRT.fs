type Fooable<'a> =
  abstract member Foo: unit -> 'a

type Barrable<'Ma> =
  abstract member Bar: (unit -> 'Mb) -> 'Mb when 'Mb :> Fooable<'Mb>

[<Struct>]
type AB<'a> = A of 'a | B with
  interface Fooable<AB<'a>> with
    member __.Foo () = B

  interface Barrable<AB<'a>> with
    member __.Bar (f: unit -> 'Mb) =
      Unchecked.defaultof<'Mb>.Foo()

(B :> Barrable<_>).Bar (fun () -> B) |> printfn "%A"

let doBar<'Ma when 'Ma :> Barrable<'Ma>> foof (bar: 'Ma) =
    bar.Bar foof

B |> doBar (fun () -> B) |> printfn "%A"

