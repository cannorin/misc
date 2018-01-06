(*
The X11 License
import.fsx - load NuGet packages with ease
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

[<AutoOpen>]
module Import

#I ".paket/lib/net45"
#I ".paket/lib/net40"
#I ".paket/load/net45"
#r "Paket.Core.dll"

open System
open System.IO
open Paket

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let private init () = 
  Dependencies.Init();
  let dep = Dependencies.Locate(__SOURCE_DIRECTORY__) in
  dep.Install (false, false, false, false, true, SemVerUpdateMode.NoRestriction, false, true, ["net45"], ["fsx"], None)

if not (File.Exists "paket.dependencies") then
  init ()

let mutable private vbs = false 
let verbose flag =
  vbs <- flag

let declarative flag =
  if flag then
    if vbs then eprintfn "* Warning: declarative true";
    File.Delete "paket.dependencies";
    File.Delete "paket.lock";
    init ()

let frameworks fws =
  let a = fws |> String.concat ", " |> sprintf "framework: %s" in
  let dep = Dependencies.Locate(__SOURCE_DIRECTORY__) in
  let df = dep.GetDependenciesFile() in
  let lines = df.Lines |> Array.tryFind (fun x -> x.StartsWith "framework:")
                       |> Option.map (fun x -> df.Lines |> Array.filter ((<>) x))
                       |> Option.defaultValue df.Lines
  in
  File.WriteAllLines(df.FileName, Array.append [| a |] lines)

let import (package: string) =
  let dep = Dependencies.Locate(__SOURCE_DIRECTORY__) in
  let l = dep.GetDependenciesFile().Lines in
  if l |> Array.tryFind ((=) (sprintf "nuget %s" package)) |> Option.isNone then
    if vbs then eprintfn "- Installing %s ..." package;
    try
      dep.Add package;
      dep.Install (false, false, false, false, true, SemVerUpdateMode.NoRestriction, false, true, ["net45"], ["fsx"], None)
    with
      | e -> eprintfn "* Error: %s" e.Message
