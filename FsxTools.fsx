#if COMPILED
[<AutoOpen>]
module FsxTools
#endif
#load "prelude.fs"
#load "paket-files/fsprojects/FSharp.TypeProviders.StarterPack/src/ProvidedTypes.fsi"
#load "paket-files/fsprojects/FSharp.TypeProviders.StarterPack/src/ProvidedTypes.fs"

open System
open System.IO
open System.Text
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

let inline private konst x = fun _ -> x

let private toIdentifier s =
  s |> String.toCharArray
    |> Array.mapi (fun i -> function
        | c when Char.IsLetter c -> c.ToString()
        | c when Char.IsDigit c ->
          if i = 0 then sprintf "_%c" c else sprintf "%c" c
        | _ -> "_" )
    |> String.concat ""

let private thisAsm = Assembly.GetExecutingAssembly()
let private ns = "FsxTools"

[<TypeProvider>]
type FSP (cfg) as this =
  inherit TypeProviderForNamespaces(cfg)

  let fileSystem = ProvidedTypeDefinition(thisAsm, ns, "FileSystem", None)
  
  let rec instantiate tyName rootDir showHidden useOrig =
    let toIdentifier s = if useOrig then s else toIdentifier s
    let ty = ProvidedTypeDefinition(thisAsm, ns, tyName, None)
    ProvidedConstructor([], invokeCode=konst <@@ rootDir @@>) |> ty.AddMember

    for file in Directory.GetFiles(rootDir) |> Seq.map FileInfo do
      let fileTy = ProvidedTypeDefinition(toIdentifier file.Name + "_static", None)
      ty.AddMember fileTy
      [
        ProvidedField.Literal("CompileTimeName", typeof<string>, box file.Name);
        ProvidedField.Literal("CompileTimeFullName", typeof<string>, box file.FullName);
      ] |> fileTy.AddMembers
      let n = file.Name
      let fn = file.FullName
      [ 
        ProvidedProperty("Name", typeof<string>, getterCode=konst <@@ n @@>);
        ProvidedProperty("FullName", typeof<string>, getterCode=konst <@@ fn @@>);
        ProvidedProperty("Text", typeof<string>, getterCode=konst <@@ File.ReadAllText fn @@>);
        ProvidedProperty("Lines", typeof<string[]>, getterCode=konst <@@ File.ReadAllLines fn @@>);
        ProvidedProperty("Info", typeof<FileInfo>, getterCode=konst <@@ FileInfo(fn) @@>);
      ] |> fileTy.AddMembers
      [
        ProvidedMethod("GetReadStream", [], typeof<FileStream>, invokeCode=konst <@@ File.OpenRead fn @@>);
        ProvidedMethod("GetWriteStream", [], typeof<FileStream>, invokeCode=konst <@@ File.OpenWrite fn @@>);
      ] |> fileTy.AddMembers
      try
        let text = File.ReadAllText file.FullName
        ProvidedField.Literal("CompileTimeText", typeof<string>, box text) |> fileTy.AddMember
      with _ -> ()
      ProvidedProperty(toIdentifier file.Name, fileTy, getterCode=konst <@@ fn @@>) |> ty.AddMember

    for subdir in Directory.GetDirectories(rootDir) |> Seq.map DirectoryInfo do
      ty.AddMembersDelayed(fun () ->
          let dirTy = instantiate (toIdentifier subdir.Name + "_static") subdir.FullName showHidden useOrig
          let fn = subdir.FullName
          let dirProp = ProvidedProperty(toIdentifier subdir.Name, dirTy, getterCode=konst <@@ fn @@>)
          [ dirTy :> MemberInfo; dirProp :> MemberInfo ]
        )
    [
      ProvidedField.Literal("CompileTimeName", typeof<string>, box (DirectoryInfo(rootDir).Name));
      ProvidedField.Literal("CompileTimeFullName", typeof<string>, box rootDir);
    ] |> ty.AddMembers
    let n = (DirectoryInfo(rootDir)).Name
    [
      ProvidedProperty("Name", typeof<string>, getterCode=konst <@@ n @@>);
      ProvidedProperty("FullName", typeof<string>, getterCode=konst <@@ rootDir @@>);
      ProvidedProperty("Info", typeof<DirectoryInfo>, getterCode=konst <@@ DirectoryInfo(rootDir) @@>)
    ] |> ty.AddMembers
    ty

  do fileSystem.DefineStaticParameters(
      parameters = [
        ProvidedStaticParameter("rootDir", typeof<string>, box (Directory.GetCurrentDirectory()));
        ProvidedStaticParameter("showHidden", typeof<bool>, box false);
        ProvidedStaticParameter("useOriginalNames", typeof<bool>, box false);
      ],
      instantiationFunction = (fun tyName ->
        function
          | [| :? string as rootDir; :? bool as showHidden; :? bool as useOrig |] ->
            instantiate tyName rootDir showHidden useOrig
          | args -> failwithf "unexpected parameter values: '%A'" args
        )
      )
  do this.AddNamespace(ns, [fileSystem])

[<assembly:TypeProviderAssembly>]
do ()

