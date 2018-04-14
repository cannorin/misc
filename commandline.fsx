(*
The X11 License
commandline.fsx - lightweight command line option parser & command line application framework
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

module Commandline
#load "scanf.fsx"
#load "prelude.fs"
open Text.Scanf

let inline private tupleMap f g (x, y) = (f x, g y)

exception OptionParseFailed of msg: string with
  override this.Message = this.msg
exception CommandExecutionFailed of msg: string with
  override this.Message = this.msg

type ValueFormat<'p,'st,'rd,'rl,'t,'a> = {
    format: PrintfFormat<'p,'st,'rd,'rl,'t>
    handler: 't -> 'a
  }
  with 
    member this.construct () =
      let parser s = 
        s |> tryKscanf this.format this.handler
          |> function Ok x -> Some x | _ -> None
      in
      parser

let valueFormat (fmt: PrintfFormat<_,_,_,_,'t>) : ValueFormat<_,_,_,_,'t,'t> =
  { format = fmt; handler = id }

let (-->) (cof: ValueFormat<_,_,_,_,'s,'t>) (mapper: 't -> 'u) =
  { format = cof.format;  handler = cof.handler >> mapper }

type Args = string list

let inline private argMap (xf: ^T) (argv: Args) : ('a * Args) =
  (^T: (member mapArgs: Args -> ('a * Args)) (xf,argv))
let inline private getName (xf: ^T) : string =
  (^T: (member Name: string) xf)

module Options =
  type CommandOptionNoArgProvided<'a> =
    | UseDefault of 'a
    | JustFail

  type CommandOptionKind<'a> =
    | Flag of (bool -> 'a)
    | TakingValueWith of CommandOptionNoArgProvided<'a> * (string -> 'a option) list

  type CommandOption<'a> = {
      names: string list;
      description: string;    
      kind: CommandOptionKind<'a>
    }

  let inline private defaultCO () = { names=[]; description=""; kind=TakingValueWith (JustFail, []) }
  let inline private defaultCF () = { names=[]; description=""; kind=Flag id }

  type CommandOptionBuilder<'a>(dc: unit -> CommandOption<'a>) =
    member __.For (_, _) = failwith "Not supported"
    member __.Yield _ = dc ()
    [<CustomOperation("names")>]
    member __.Names (co, x) = { co with names = x }
    [<CustomOperation("description")>]
    member __.Description (co, x) = { co with description = x }
    [<CustomOperation("takes")>]
    member __.Takes (co: CommandOption<'a>, x: ValueFormat<_,_,_,_,_,'a>) =
      { co with 
          kind =
            match co.kind with
              | TakingValueWith (d, xs) -> TakingValueWith (d, List.append xs [x.construct()])
              | _                       -> TakingValueWith (JustFail, [x.construct()]);
      }
    [<CustomOperation("defaultValue")>]
    member __.DefaultValue (co: CommandOption<'a>, value: 'a) =
      { co with
          kind =
            match co.kind with
              | TakingValueWith (_, xs) -> TakingValueWith (UseDefault value, xs)
              | x -> x
      }

  let commandOption<'a> = CommandOptionBuilder<'a> defaultCO
  let commandFlag = CommandOptionBuilder defaultCF

  type private RefinedToken =
    | RFlag of string
    | RFlagDisable of string
    | RFlagAndValue of string * string
    | RMaybeCombinedFlag of string
    | RMaybeCombinedFlagAndValue of string * string
    | RValue of string
    | RIgnoreAfter
    with
      override this.ToString() =
        match this with
          | RFlag s -> sprintf "--%s" s
          | RFlagDisable s -> sprintf "-%s-" s
          | RFlagAndValue (s, v) -> sprintf "--%s=%s" s v
          | RMaybeCombinedFlag s -> sprintf "-%s" s
          | RMaybeCombinedFlagAndValue (s, v) -> sprintf "-%s=%s" s v
          | RValue s -> s
          | RIgnoreAfter -> "--"

  let private optionForms = [
      tryKscanf "--" (fun () -> RIgnoreAfter)
      tryKscanf "-%c" (RFlag << to_s);
      tryKscanf "-%c=%s" (tupleMap to_s id >> RFlagAndValue);
      tryKscanf "--%s=%s" RFlagAndValue;
      tryKscanf "/%s=%s" RFlagAndValue;
      tryKscanf "--%s" RFlag;
      tryKscanf "-%c+" (RFlag << to_s);
      tryKscanf "-%c-" (RFlagDisable << to_s);
      tryKscanf "-%s=%s" RMaybeCombinedFlagAndValue;
      tryKscanf "-%s" RMaybeCombinedFlag;
      tryKscanf "/%s" RFlag;
      tryKscanf "%s" RValue
    ]

  let rec private tokenize argv =
    seq {
      if List.isEmpty argv then
        yield! Seq.empty
      else
        let (h, t) = (List.head argv, List.tail argv) in
        let r = optionForms |> List.map (fun f -> f h)
                            |> List.choose (function Ok x -> Some x | _ -> None)
                            |> List.head in
        yield r;
        yield!
          match r with
            | RIgnoreAfter -> t |> Seq.map RValue
            | _ -> tokenize t
    }

  /// specify how to treat options like ```-abcd```
  type SingleHyphenStyle = 
    /// treat ```-abcd``` as ```--abcd```
    | SingleLong
    /// treat ```-abcd``` as ```-a bcd```
    | SingleShort
    /// treat ```-abcd``` as ```-a -b -c -d```
    | MergedShort

  let getRemainingOptions argv =
    argv |> tokenize |> List.ofSeq
         |> List.choose (function RIgnoreAfter | RValue _ -> None | x -> Some x)
         |> List.map to_s

  let parseWithStyle shs (opt: CommandOption<'a>) argv =
    let inline isSingle s = String.length s = 1 in
    let inline matches x = opt.names |> List.contains x in
    let shortNames = opt.names |> List.filter isSingle in

    let tokens = tokenize argv |> List.ofSeq in
    let rec find ts =
      match opt.kind with
        | Flag f ->
          let inline f x = f x |> Some in
          match ts with
            | RFlag x :: rest when matches x -> (f true, rest)
            | RFlagDisable x :: rest when matches x -> (f false, rest)
            | RFlagAndValue (x, _) :: _ when matches x ->
              sprintf "'%s' is a flag and does not take an argument" x |> OptionParseFailed |> raise
            | RMaybeCombinedFlag xs :: rest & x :: _ ->
              match shs with
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains in
                  (f true, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | SingleLong  when matches xs -> (f true, rest)
                | SingleShort when shortNames |> List.exists xs.StartsWith ->
                  sprintf "'%c' is a flag and does not take an argument" (xs.[0]) |> OptionParseFailed |> raise
                | _ -> find rest |> tupleMap id (fun rest' -> x :: rest')
            | RMaybeCombinedFlagAndValue (xs, v) :: rest & x :: _ ->
              match shs with
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains in
                  if shortNames |> List.exists xs.EndsWith then
                    sprintf "'%s' is a flag and does not take an argument" c |> OptionParseFailed |> raise
                  else 
                    (f true, RMaybeCombinedFlagAndValue(xs.Replace(c, ""), v) :: rest)
                | SingleLong when matches xs ->
                    sprintf "'%s' is a flag and does not take an argument" xs |> OptionParseFailed |> raise
                | SingleShort -> sprintf "invalid option: '-%s=%s'" xs v |> OptionParseFailed |> raise
                | _ -> find rest |> tupleMap id (fun rest' -> x :: rest')
            | x :: rest -> find rest |> tupleMap id (fun rest' -> x :: rest')
            | [] -> (None, [])
        | TakingValueWith (na, fs) ->
          let inline tryReturn v name =
            match (fs |> List.map (fun f -> f v) |> List.choose id |> List.tryHead) with
              | Some x -> Some x
              | None -> 
                sprintf "the value '%s' is invalid for the option '%s'" v name |> OptionParseFailed |> raise
          in
          let inline tryDefault name =
            match na with
              | UseDefault x -> Some x
              | JustFail -> 
                sprintf "a value is missing for the option '%s'" name |> OptionParseFailed |> raise
          in
          match ts with
            | RFlag x :: RValue v :: rest
            | RFlagAndValue (x, v) :: rest when matches x -> (tryReturn v x, rest)
            | RFlag x :: rest when matches x -> (tryDefault x, rest)
            | RFlagDisable x :: _ when matches x ->
              sprintf "a value is missing for the option '%s'" x |> OptionParseFailed |> raise
            | RMaybeCombinedFlag xs :: RValue v :: rest & x :: _ :: _->
              match shs with
                | MergedShort when shortNames |> List.exists xs.EndsWith ->
                  let c = shortNames |> List.find xs.EndsWith in
                  (tryReturn v c, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains in
                  (tryDefault c, RMaybeCombinedFlag (xs.Replace(c, "")) :: RValue v :: rest)
                | SingleShort when shortNames |> List.exists xs.StartsWith ->
                  let c = shortNames |> List.find xs.StartsWith in
                  let v' = xs.Substring(1) in
                  (tryReturn v' c, RValue v :: rest)
                | SingleLong when matches xs ->
                  (tryReturn v xs, rest)
                | _ -> find rest |> tupleMap id (fun rest' -> x :: RValue v :: rest')
            | RMaybeCombinedFlagAndValue (xs, v) :: rest & x :: _ ->
              match shs with
                | MergedShort when shortNames |> List.exists xs.EndsWith ->
                  let c = shortNames |> List.find xs.EndsWith in
                  (tryReturn v c, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains in
                  (tryDefault c, RMaybeCombinedFlagAndValue (xs.Replace(c, ""), v) :: rest)
                | SingleLong when matches xs ->
                  (tryReturn v xs, rest)
                | SingleShort -> sprintf "invalid option: '-%s=%s'" xs v |> OptionParseFailed |> raise
                | _ -> find rest |> tupleMap id (fun rest' -> x :: rest')
            | x :: rest -> find rest |> tupleMap id (fun rest' -> x :: rest')
            | [] -> (None, [])
    in
    find tokens |> tupleMap id (List.map to_s)

  let parse opt argv =
    parseWithStyle MergedShort opt argv

  let parseMany opt argv =
    let rec p xs =
      seq  {
        yield!
          match (parse opt xs) with
            | (Some x, rest) ->
              seq { yield (x, rest); yield! p rest }
            | (None, rest) ->
              Seq.empty
      }
    in
    let x = p argv in
    (x |> Seq.map fst |> List.ofSeq, x |> Seq.map snd |> Seq.tryLast ?| argv)

  type CommandOption<'a> with
    member this.mapArgs argv =
      parse this argv
    member this.Name = this.names |> List.head
    member this.Description = this.description

module Commands =
  type Command<'a> =
    struct
      val func: Args -> ('a * Args)
      val name: string
      new(f) = { func = f; name = "(unnamed command)" }
      new(f, n) = { func = f; name = n }
      
      member this.mapArgs argv = this.func argv
      member this.Name = this.name
    end

  module CommandOptionUtils =
    let inline zeroOrMore co : Command<'a list> =
      let inline f argv =
        let rec p xs =
          seq  {
            yield!
              match (argMap co xs) with
                | (Some x, rest) ->
                  seq { yield (x, rest); yield! p rest }
                | (None, rest) ->
                  Seq.empty
          }
        in
        let x = p argv in
        (x |> Seq.map fst |> List.ofSeq, x |> Seq.map snd |> Seq.tryLast ?| [])
      in
      Command (f, getName co)

    let inline zeroOrExactlyOne co =
      let co' = zeroOrMore co in
      let name = getName co in
      Command ((fun argv -> 
          match (argMap co' argv) with
            | ([], _) -> (None, argv)
            | (x :: [], argv') -> (Some x, argv')
            | _ -> sprintf "the option '%s' should be provided only once" name |> CommandExecutionFailed |> raise
        ), name)

    let inline whenMissingUse v co =
      Command ((fun argv -> argMap co argv |> tupleMap (Option.defaultValue v) id), getName co)

  module Command =
    let inline run (argv: string seq) c = (List.ofSeq argv) |> argMap c |> fst
    
    let inline runAsEntryPoint (argv: string array) c =
      try
        run argv c
      with
        | CommandExecutionFailed msg
        | OptionParseFailed msg ->
          cprintfn System.ConsoleColor.Red "error: %s" msg; -1

    let inline bind f c : Command<'b> =
      Command (
          fun argv ->
            let (a, argv') = argv |> argMap c in
            argv' |> argMap (f a)
        )
    let inline map f c =
      Command (
          fun argv ->
            let (a, argv') = argv |> argMap c in
            (f a, argv')
        )
    let returnValue x = Command (fun argv -> (x, argv))
    let inline combine a b = a |> bind (fun () -> b)
    let inline delay f : Command<'a> =
      Command (fun argv -> argv |> argMap (f ()))
    let args = Command (fun argv -> (argv, argv))

  let inline (>>=) c f = Command.bind f c 

  type CommandBuilder() =
    member inline __.Bind (c, f) = Command.bind f c
    member __.Return x = Command.returnValue x
    member __.ReturnFrom x = x
    member inline __.Combine (a, b) = Command.combine a b
    member inline __.Delay f = Command.delay f
    member __.Zero () = Command.returnValue ()

  let command = CommandBuilder ()

  module CommandUtils =
    let failOnUnknownOptions =
      command {
        let! argv = Command.args in
        let uks = argv |> Options.getRemainingOptions in
        if uks |> List.isEmpty then
          return ()
        else
          sprintf "unknown option: '%s'" (List.head uks) |> CommandExecutionFailed |> raise
      }

#if EXAMPLE

open Options
open Commands

let sumCommand = 
  command {
    let! nums = 
      commandOption {
        names ["num"; "n"]
        description "a number to add."
        takes (valueFormat "%i")
      } |> CommandOptionUtils.zeroOrMore 
    in
    return List.sum nums
  }

[<EntryPoint>]
let main argv =
  command {
    let! name =
      commandOption {
        names ["name"]
        description "name of person."
        takes (valueFormat "%s")
      } |> CommandOptionUtils.zeroOrExactlyOne
        |> CommandOptionUtils.whenMissingUse "Guest"
    
    let! shout =
      commandFlag {
        names ["shout"; "s"]
        description "shouts!"
      } |> CommandOptionUtils.whenMissingUse false
    
    let! sum = sumCommand
    let! argv = Command.args
    //do! CommandUtils.failOnUnknownOptions

    do printfn "Name: %s, Sum: %i" name sum
    do printfn ""
    do printfn "Unused arguments: %A" argv
    return 0
  } |> Command.runAsEntryPoint argv

#endif
