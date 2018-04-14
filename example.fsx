// mandartory, in this order
#load "paket.core.fsx"
#load "import.fsx"

// optional: set true to see logs.
// * logs will be printed to stderr.
verbose true

// optional: set true to reset paket.dependencies and paket.lock each time.
// * recommended when you want to change the package versions
//   and you have to remove the previous ones.
// * recommended when you add packages and meet broken dependencies;
//   with this, everything will be reset and the listed packages will be reinstalled.
// * otherwise, set false to speed up the execution.
declarative true

// optional: you can avoid the "netstandard package hell" with this 
frameworks ["net40"; "net45"]

// list nuget packages here
import "FSharp.Data"
import "Newtonsoft.Json ~> 8.0" // 8.0 <= x < 9.0

// mandatory: .paket/load/net45/main.group.fsx references all the libraries
// if it fails to load, add the following line:
//   #I ".paket/load/net45"
#load "main.group.fsx"

let json = """ { "name":"Tomas", "age":4 } """

// example for FSharp.Data
open FSharp.Data

type Simple = JsonProvider<""" { "name":"John", "age":94 } """>

let simple = Simple.Parse(json)
simple.Name |> printfn "Name: %s"
simple.Age  |> printfn "Age:  %i"


// example for Newtonsoft.Json
open Newtonsoft.Json.Linq

let jo = JObject.Parse(json)
jo.GetValue("name") |> printfn "Name: %O"
jo.GetValue("age")  |> printfn "Age:  %O"

