## import.fsx - load NuGet packages with ease

```fsharp
#load "paket.core.fsx"
#load "import.fsx"

verbose true
declarative true

frameworks ["net40"; "net45"]

import "FSharp.Data"
import "Newtonsoft.Json ~> 8.0" // 8.0 <= x < 9.0

#load "main.group.fsx"

open FSharp.Data
open Newtonsoft.Json.Linq


let json = """ { "name":"Tomas", "age":4 } """

type Simple = JsonProvider<""" { "name":"John", "age":94 } """>

let simple = Simple.Parse(json)
simple.Name |> printfn "Name: %s"
simple.Age  |> printfn "Age:  %i"

let jo = JObject.Parse(json)
jo.GetValue("name") |> printfn "Name: %O"
jo.GetValue("age")  |> printfn "Age:  %O"
```

See example.fsx for details.

MIT (X11) License.

