module Tests

open System
open Xunit
open Hello

[<Fact>]
let ``Hello should return Hello`` () =
  assert (hello () = "Hello!")
