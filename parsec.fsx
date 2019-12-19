#if COMPILED
module MiniParsec
#endif

let [<Literal>] EOS = '\uffff'

type StringSegment = {
  startIndex: int
  length: int
  underlying: string
} with
  member inline this.Value = this.underlying.Substring(this.startIndex, this.length)
  member inline this.Item index =
    if index < 0 || index >= this.length then failwith "Index was out of range (Item)."
    else this.underlying.[this.startIndex + index]
  member inline this.GetSafe index =
    if index < 0 || index >= this.length then EOS
    else this.underlying.[this.startIndex + index]
  member inline this.GetSlice (start, finish) =
    let start = defaultArg start 0
    let finish = defaultArg finish (this.length - 1)
    let inline check x = x >= 0 && x < this.length
    if check start && check finish then
      { underlying = this.underlying; startIndex = this.startIndex + start; length = finish - start + 1 }
    else failwith "Index was out of range (GetSlice)."

module StringSegment =
  let inline private normalize (str: string) =
    str.Replace("\r\n", "\n").Replace("\r", "\n")
  let inline ofString (str: string) =
    let str = normalize str
    { underlying = str; startIndex = 0; length = str.Length }
  let inline toString (seg: StringSegment) = seg.Value
  let inline isEmpty (seg: StringSegment) = seg.length <= 0
  let startsWith (s: string) (seg: StringSegment) =
    let rec check i =
      if i = s.Length then true
      else if seg.underlying.[i + seg.startIndex] <> s.[i] then false
      else check (i+1)
    check 0
  let indexOfItem (c: char) (seg: StringSegment) =
    let rec check i =
      if i = seg.length then -1
      else if seg.underlying.[i + seg.startIndex] = c then i
      else check (i+1)
    check 0
  let indexOfAny (cs: char seq) (seg: StringSegment) =
    let s = Set.ofSeq cs
    let rec check i =
      if i = seg.length then -1
      else if s |> Set.contains seg.underlying.[i + seg.startIndex] then i
      else check (i+1)
    check 0
  let indexOfSequence (s: string) (seg: StringSegment) =
    let rec check i j =
      if j = s.Length then i - j
      else if i + s.Length - j > seg.length then -1
      else if seg.underlying.[i + seg.startIndex] = s.[j] then check (i+1) (j+1)
      else check (i+1) 0
    check 0 0
  let inline subString start length (seg: StringSegment) = seg.[start..length-start+1]
  let inline skip length (seg: StringSegment) =
    { underlying = seg.underlying; startIndex = seg.startIndex + length; length = seg.length - length }
  let inline getSafe i (seg: StringSegment) = seg.GetSafe i

type StringSegment with
  member inline this.StartsWith (s: string) = StringSegment.startsWith s this
  member inline this.IndexOf (c: char) = StringSegment.indexOfItem c this
  member inline this.IndexOf (s: string) = StringSegment.indexOfSequence s this
  member inline this.IndexOfAny (cs: char seq) = StringSegment.indexOfAny cs this

type SourcePos = int

type Message = Lazy<string>

type ParseResult<'Result, 'State> = 'Result * StringSegment * 'State
type ParseError<'State> = SourcePos * Message * 'State

type Parser<'Result, 'State> =
  ('State * StringSegment -> Result<ParseResult<'Result, 'State>, ParseError<'State>>)

let inline run       (p: Parser<'a, 's>) state input = p (state, input)
let inline runString (p: Parser<'a, 's>) state input = p (state, StringSegment.ofString input)

[<AutoOpen>]
module Primitives =
  let inline preturn (x: 'a) : Parser<'a, 's> = fun (state, s) -> Ok (x, s, state)
  let inline pzero (state, s: StringSegment) = Error (s.startIndex, lazy "pzero", state)

  let inline ( >>= ) (p: Parser<'a, 's>) (f: 'a -> Parser<'b, 's>) : Parser<'b, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (r, s, state) -> run (f r) state s

  let inline ( >>% ) (p: Parser<'a, 's>) (x: 'b) : Parser<'b, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (_, s, state) -> Ok (x, s, state)

  let inline ( >>. ) (p1: Parser<'a, 's>) (p2: Parser<'b, 's>) : Parser<'b, 's> =
    fun (state, s) ->
      match run p1 state s with
      | Error e -> Error e
      | Ok (_, s, state) ->
        match run p2 state s with
        | Error e -> Error e
        | Ok (r2, s, state) -> Ok (r2, s, state)

  let inline ( .>> ) (p1: Parser<'a, 's>) (p2: Parser<'b, 's>) : Parser<'a, 's> =
    fun (state, s) ->
      match run p1 state s with
      | Error e -> Error e
      | Ok (r1, s, state) ->
        match run p2 state s with
        | Error e -> Error e
        | Ok (_, s, state) -> Ok (r1, s, state)

  let inline ( .>>. ) (p1: Parser<'a, 's>) (p2: Parser<'b, 's>) : Parser<'a * 'b, 's> =
    fun (state, s) ->
      match run p1 state s with
      | Error e -> Error e
      | Ok (r1, s, state) ->
        match run p2 state s with
        | Error e -> Error e
        | Ok (r2, s, state) -> Ok ((r1, r2), s, state)

  let inline between pl pr p = pl >>. p .>> pr

  let inline ( |>> ) (p: Parser<'a, 's>) (f: 'a -> 'b) : Parser<'b, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (r, s, state) -> Ok (f r, s, state)

  let inline pipe2 (p1: Parser<'a, 's>) (p2: Parser<'b, 's>) (f: 'a -> 'b -> 'c) : Parser<'c, 's> =
    fun (state, s) ->
      match run p1 state s with
      | Error e -> Error e
      | Ok (r1, s, state) ->
        match run p2 state s with
        | Error e -> Error e
        | Ok (r2, s, state) -> Ok (f r1 r2, s, state)

  let inline pipe3 p1 p2 p3 f : Parser<'r, 's> =
    p1 >>= fun r1 ->
      p2 >>= fun r2 ->
        p3 >>= fun r3 -> f r1 r2 r3 |> preturn

  let inline pipe4 p1 p2 p3 p4 f : Parser<'r, 's> =
    p1 >>= fun r1 ->
      p2 >>= fun r2 ->
        p3 >>= fun r3 ->
          p4 >>= fun r4 -> f r1 r2 r3 r4 |> preturn

  let inline pipe5 p1 p2 p3 p4 p5 f : Parser<'r, 's> =
    p1 >>= fun r1 ->
      p2 >>= fun r2 ->
        p3 >>= fun r3 ->
          p4 >>= fun r4 ->
            p5 >>= fun r5 -> f r1 r2 r3 r4 r5 |> preturn

  let inline ( <|> ) (p1: Parser<'a, 's>) (p2: Parser<'a, 's>) : Parser<'a, 's> =
    fun (state, s) ->
      match run p1 state s with
      | Ok _ as x -> x
      | Error _ -> run p2 state s

  let inline ( <|>% ) (p: Parser<'a, 's>) (x: 'a) : Parser<'a, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok _ as x -> x
      | Error _ -> Ok (x, s, state)

  let inline choice (ps: Parser<'a, 's> list) : Parser<'a, 's> =
    fun (state, s) ->
      let rec go = function
        | [] -> Error (s.startIndex, lazy "No parsers given", state)
        | p :: [] -> run p state s
        | p :: ps ->
          match run p state s with
          | Ok _ as x -> x
          | Error _ -> go ps
      go ps

  let inline choiceL (ps: Parser<'a, 's> list) (label: string) : Parser<'a, 's> =
    fun (state, s) ->
      let rec go = function
        | [] -> Error (s.startIndex, lazy sprintf "Expected: %s" label, state)
        | p :: [] ->
          match run p state s with
          | Ok _ as x -> x
          | Error (pos, _, state) -> Error (pos, lazy sprintf "Expected: %s" label, state)
        | p :: ps ->
          match run p state s with
          | Ok _ as x -> x
          | Error _ -> go ps
      go ps

  let inline opt (p: Parser<'a, 's>) : Parser<'a option, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok (r, s, state) -> Ok (Some r, s, state)
      | Error _ -> Ok (None, s, state)

  let inline optional (p: Parser<'a, 's>) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok (_, s, state) -> Ok ((), s, state)
      | Error _ -> Ok ((), s, state)

  let inline notEmpty (p: Parser<'a, 's>) : Parser<'a, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok (_, s', _) as x when s.startIndex <> s'.startIndex || s.length <> s'.length -> x
      | Ok _ -> Error (s.startIndex, lazy "notEmpty failed", state)
      | Error _ as x -> x

  let inline followedBy (p: Parser<'a, 's>) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok _ -> Ok ((), s, state)
      | Error e -> Error e

  let inline followedByL (p: Parser<'a, 's>) (label: string) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok _ -> Ok ((), s, state)
      | Error (pos, _, state) -> Error (pos, lazy sprintf "Expected: %s" label, state)

  let inline notFollowedBy (p: Parser<'a, 's>) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok _ -> Error (s.startIndex, lazy "notFollowedBy failed", state)
      | Error _ -> Ok ((), s, state)

  let inline notFollowedByL (p: Parser<'a, 's>) (label: string) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok _ -> Error (s.startIndex, lazy sprintf "Expected: %s" label, state)
      | Error _ -> Ok ((), s, state)

  let inline lookAhead (p: Parser<'a, 's>) : Parser<'a, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok (r, _, _) -> Ok (r, s, state)
      | Error _ as x -> x

  let inline ( <?> ) (p: Parser<'a, 's>) (label: string) : Parser<'a, 's> =
    fun (state, s) ->
      match run p state s with
      | Ok _ as x -> x
      | Error (pos, _, state) -> Error (pos, lazy sprintf "Expected: %s" label, state)

  let inline fail (msg: string) : Parser<'a, 's> = fun (state, s) -> Error (s.startIndex, lazy msg, state)

  let inline tuple2 (p1: Parser<'a, 's>) (p2: Parser<'b, 's>) : Parser<'a * 'b, 's> = p1 .>>. p2
  let inline tuple3 p1 p2 p3 : Parser<'a * 'b * 'c, 's> =
    pipe3 p1 p2 p3 (fun r1 r2 r3 -> r1,r2,r3)
  let inline tuple4 p1 p2 p3 p4 : Parser<'a * 'b * 'c * 'd, 's> =
    pipe4 p1 p2 p3 p4 (fun r1 r2 r3 r4 -> r1,r2,r3,r4)
  let inline tuple5 p1 p2 p3 p4 p5 : Parser<'a * 'b * 'c * 'd * 'e, 's> =
    pipe5 p1 p2 p3 p4 p5 (fun r1 r2 r3 r4 r5 -> r1,r2,r3,r4,r5)

  let inline parray (len: int) (p: Parser<'a, 's>) : Parser<'a[], 's> =
    fun (state, s) ->
      let result = Array.zeroCreate len
      let rec go i (state, s) =
        if i = len then Ok (result, s, state)
        else
          match run p state s with
          | Error e -> Error e
          | Ok (r, s, state) ->
            result.[i] <- r
            go (i+1) (state, s)
      go 0 (state, s)

  let inline skipArray (len: int) (p: Parser<'a, 's>) : Parser<unit, 's> =
    let rec go i (state, s) =
      if i = len then Ok ((), s, state)
      else
        match run p state s with
        | Error e -> Error e
        | Ok (_, s, state) -> go (i+1) (state, s)
    go 0

  let inline many (p: Parser<'a, 's>) : Parser<'a list, 's> =
    let rec go acc (state, s) =
      match run p state s with
      | Error _ -> Ok (List.rev acc, s, state)
      | Ok (r, s, state) -> go (r :: acc) (state, s)
    go []

  let inline many1 (p: Parser<'a, 's>) : Parser<'a list, 's> =
    let rec go acc (state, s) =
      match run p state s with
      | Error e -> if List.isEmpty acc then Error e else Ok (List.rev acc, s, state)
      | Ok (r, s, state) -> go (r :: acc) (state, s)
    go []

  let inline skipMany (p: Parser<'a, 's>) : Parser<unit, 's> =
    let rec go (state, s) =
      match run p state s with
      | Error _ -> Ok ((), s, state)
      | Ok (_, s, state) -> go (state, s)
    go

  let inline skipMany1 (p: Parser<'a, 's>) : Parser<unit, 's> =
    let rec go first (state, s) =
      match run p state s with
      | Error e -> if first then Error e else Ok ((), s, state)
      | Ok (_, s, state) -> go false (state, s)
    go true

  let inline sepBy (p: Parser<'a, 's>) (sep: Parser<_, 's>) : Parser<'a list, 's> =
    fun (state, s) ->
      match run p state s with
      | Error _ -> Ok ([], s, state)
      | Ok (r, s, state) ->
        let p' = sep >>. p
        let rec go acc (state, s) =
          match run p' state s with
          | Error _ -> Ok (List.rev acc, s, state)
          | Ok (r, s, state) -> go (r :: acc) (state, s)
        go [r] (state, s)

  let inline sepBy1 (p: Parser<'a, 's>) (sep: Parser<_, 's>) : Parser<'a list, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (r, s, state) ->
        let p' = sep >>. p
        let rec go acc (state, s) =
          match run p' state s with
          | Error _ -> Ok (List.rev acc, s, state)
          | Ok (r, s, state) -> go (r :: acc) (state, s)
        go [r] (state, s)

  let inline skipSepBy (p: Parser<'a, 's>) (sep: Parser<_, 's>) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Error _ -> Ok ((), s, state)
      | Ok (_, s, state) ->
        let p' = sep >>. p
        let rec go (state, s) =
          match run p' state s with
          | Error _ -> Ok ((), s, state)
          | Ok (_, s, state) -> go (state, s)
        go (state, s)

  let inline skipSepBy1 (p: Parser<'a, 's>) (sep: Parser<_, 's>) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (_, s, state) ->
        let p' = sep >>. p
        let rec go (state, s) =
          match run p' state s with
          | Error _ -> Ok ((), s, state)
          | Ok (_, s, state) -> go (state, s)
        go (state, s)

  let inline sepEndBy (p: Parser<'a, 's>) (sep: Parser<_, 's>) : Parser<'a list, 's> =
    fun (state, s) ->
      match run p state s with
      | Error _ -> Ok ([], s, state)
      | Ok (r, s, state) ->
        let p' = sep >>. p
        let rec go acc (state, s) =
          match run p' state s with
          | Error _ ->
            match run sep state s with
            | Error _ -> Ok (List.rev acc, s, state)
            | Ok (_, s, state) -> Ok (List.rev acc, s, state)
          | Ok (r, s, state) -> go (r :: acc) (state, s)
        go [r] (state, s)

  let inline sepEndBy1 (p: Parser<'a, 's>) (sep: Parser<_, 's>) : Parser<'a list, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (r, s, state) ->
        let p' = sep >>. p
        let rec go acc (state, s) =
          match run p' state s with
          | Error _ ->
            match run sep state s with
            | Error _ -> Ok (List.rev acc, s, state)
            | Ok (_, s, state) -> Ok (List.rev acc, s, state)
          | Ok (r, s, state) -> go (r :: acc) (state, s)
        go [r] (state, s)

  let inline skipSepEndBy (p: Parser<'a, 's>) (sep: Parser<_, 's>) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Error _ -> Ok ((), s, state)
      | Ok (_, s, state) ->
        let p' = sep >>. p
        let rec go (state, s) =
          match run p' state s with
          | Error _ ->
            match run sep state s with
            | Error _ -> Ok ((), s, state)
            | Ok (_, s, state) -> Ok ((), s, state)
          | Ok (_, s, state) -> go (state, s)
        go (state, s)

  let inline skipSepEndBy1 (p: Parser<'a, 's>) (sep: Parser<_, 's>) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (_, s, state) ->
        let p' = sep >>. p
        let rec go (state, s) =
          match run p' state s with
          | Error _ ->
            match run sep state s with
            | Error _ -> Ok ((), s, state)
            | Ok (_, s, state) -> Ok ((), s, state)
          | Ok (_, s, state) -> go (state, s)
        go (state, s)

  let inline manyTill (p: Parser<'a, 's>) (till: Parser<_, 's>) : Parser<'a list, 's> =
    let rec go acc (state, s) =
      match run till state s with
      | Ok (_, s, state) -> Ok (List.rev acc, s, state)
      | Error _ ->
        match run p state s with
        | Error e -> Error e
        | Ok (r, s, state) -> go (r :: acc) (state, s)
    go []

  let inline many1Till (p: Parser<'a, 's>) (till: Parser<_, 's>) : Parser<'a list, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (r, s, state) ->
        let rec go acc (state, s) =
          match run till state s with
          | Ok (_, s, state) -> Ok (List.rev acc, s, state)
          | Error _ ->
            match run p state s with
            | Error e -> Error e
            | Ok (r, s, state) -> go (r :: acc) (state, s)
        go [r] (state, s)

  let inline skipManyTill (p: Parser<'a, 's>) (till: Parser<_, 's>) : Parser<unit, 's> =
    let rec go (state, s) =
      match run till state s with
      | Ok (_, s, state) -> Ok ((), s, state)
      | Error _ ->
        match run p state s with
        | Error e -> Error e
        | Ok (_, s, state) -> go (state, s)
    go

  let inline skipMany1Till (p: Parser<'a, 's>) (till: Parser<_, 's>) : Parser<unit, 's> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (_, s, state) ->
        let rec go (state, s) =
          match run till state s with
          | Ok (_, s, state) -> Ok ((), s, state)
          | Error _ ->
            match run p state s with
            | Error e -> Error e
            | Ok (_, s, state) -> go (state, s)
        go (state, s)

  [<Sealed>]
  type Inline =
    static member inline Many(stateFromFirstElement: 'a -> 'State,
                              foldState: 'State -> 'a -> 'State,
                              resultFromState: 'State -> 'r,
                              elementParser: Parser<'a, 's>,
                              ?firstElementParser: Parser<'a, 's>,
                              ?resultForEmpty: unit -> 'r) : Parser<'r, 's> =
      let p = elementParser
      let fp = defaultArg firstElementParser p
      fun (state, s) ->
        match run fp state s with
        | Error e ->
          match resultForEmpty with
          | Some _ -> Ok ((match resultForEmpty with Some f -> f () | None -> Unchecked.defaultof<_>), s, state)
          | None   -> Error e
        | Ok (r, s, state) ->
          let r = stateFromFirstElement r
          let rec go acc (state, s) =
            match run p state s with
            | Error _ -> Ok (acc |> resultFromState, s, state)
            | Ok (r, s, state) -> go (foldState acc r) (state, s)
          go r (state, s)

    static member inline SepBy(stateFromFirstElement: 'a -> 'State,
                               foldState: 'State -> 'b -> 'a -> 'State,
                               resultFromState: 'State -> 'r,
                               elementParser: Parser<'a, 's>,
                               separatorParser: Parser<'b, 's>,
                               ?firstElementParser: Parser<'a, 's>,
                               ?resultForEmpty: unit -> 'r,
                               ?separatorMayEndSequece: bool) : Parser<'r, 's> =
      let p = elementParser
      let fp = defaultArg firstElementParser p
      let sep = separatorParser
      fun (state, s) ->
        match run fp state s with
        | Error e ->
          match resultForEmpty with
          | Some _ -> Ok ((match resultForEmpty with Some f -> f () | None -> Unchecked.defaultof<_>), s, state)
          | None   -> Error e
        | Ok (r, s, state) ->
          let r = stateFromFirstElement r
          let rec go acc (state, s) =
            match run sep state s with
            | Error e -> Error e
            | Ok (rSep, s, state) ->
              match run p state s with
              | Error e ->
                if defaultArg separatorMayEndSequece false then
                  Ok (acc |> resultFromState, s, state)
                else Error e
              | Ok (r, s, state) -> go (foldState acc rSep r) (state, s)
          go r (state, s)

    static member inline ManyTill(stateFromFirstElement: 'a -> 'State,
                                  foldState: 'State -> 'a -> 'State,
                                  resultFromStateAndEnd: 'State -> 'e -> 'r,
                                  elementParser: Parser<'a, 's>,
                                  endParser: Parser<'e, 's>,
                                  ?firstElementParser: Parser<'a, 's>,
                                  ?resultForEmpty: 'e -> 'r) : Parser<'r, 's> =
      let p = elementParser
      let fp = defaultArg firstElementParser p
      let ep = endParser
      let rec go acc (state, s) =
        match run ep state s with
        | Ok (rEnd, s, state) -> Ok (resultFromStateAndEnd acc rEnd, s, state)
        | Error _ ->
          match run p state s with
          | Ok (r, s, state) -> go (foldState acc r) (state, s)
          | Error e -> Error e
      fun (state, s) ->
        match resultForEmpty with
        | None ->
          match run fp state s with
          | Error e -> Error e
          | Ok (r, s, state) ->
            let r = stateFromFirstElement r
            go r (state, s)
        | Some _ ->
          match run fp state s with
          | Error _ ->
            match run ep state s with
            | Ok (rEnd, s, state) ->
              Ok ((match resultForEmpty with Some f -> f rEnd | None -> Unchecked.defaultof<_>), s, state)
            | Error e -> Error e
          | Ok (r, s, state) ->
            let r = stateFromFirstElement r
            go r (state, s)

  let inline chainl1 (p: Parser<'a, 'u>) (op: Parser<'a -> 'a -> 'a, 'u>) : Parser<'a, 'u> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (r, s, state) ->
        let rec go acc (state, s) =
          match run op state s with
          | Error _ -> Ok (acc, s, state)
          | Ok (f, s, state) ->
            match run p state s with
            | Error e -> Error e
            | Ok (r, s, state) -> go (f acc r) (state, s)
        go r (state, s)

  let inline chainl (p: Parser<'a, 'u>) (op: Parser<'a -> 'a -> 'a, 'u>) (defVal: 'a) : Parser<'a, 'u> =
    fun (state, s) ->
      match run p state s with
      | Error _ -> Ok (defVal, s, state)
      | Ok (r, s, state) ->
        let rec go acc (state, s) =
          match run op state s with
          | Error _ -> Ok (acc, s, state)
          | Ok (f, s, state) ->
            match run p state s with
            | Error e -> Error e
            | Ok (r, s, state) -> go (f acc r) (state, s)
        go r (state, s)

  let inline chainr1 (p: Parser<'a, 'u>) (op: Parser<'a -> 'a -> 'a, 'u>) : Parser<'a, 'u> =
    fun (state, s) ->
      match run p state s with
      | Error e -> Error e
      | Ok (r, s, state) ->
        let rec go fAcc argAcc (state, s) =
          match run op state s with
          | Ok (f, s, state) ->
            match run p state s with
            | Error e -> Error e
            | Ok (r, s, state) -> go (f :: fAcc) (r :: argAcc) (state, s)
          | Error _ ->
            match argAcc with
            | [] -> failwith "impossible"
            | x :: xs ->
              let result = List.fold2 (fun state f x -> f x state) x fAcc xs
              Ok (result, s, state)
        go [] [r] (state, s)

  let inline chainr (p: Parser<'a, 'u>) (op: Parser<'a -> 'a -> 'a, 'u>) (defVal: 'a) : Parser<'a, 'u> =
    fun (state, s) ->
      match run p state s with
      | Error _ -> Ok (defVal, s, state)
      | Ok (r, s, state) ->
        let rec go fAcc argAcc (state, s) =
          match run op state s with
          | Ok (f, s, state) ->
            match run p state s with
            | Error e -> Error e
            | Ok (r, s, state) -> go (f :: fAcc) (r :: argAcc) (state, s)
          | Error _ ->
            match argAcc with
            | [] -> failwith "impossible"
            | x :: xs ->
              let result = List.fold2 (fun state f x -> f x state) x fAcc xs
              Ok (result, s, state)
        go [] [r] (state, s)

  let inline createParserForwardedToRef () : Parser<'a, 'u> * Parser<'a, 'u> ref =
    let dummy (_, _) = failwith "invalid definition with createParserForwardedToRef"
    let r = ref dummy
    (fun (state, s) -> !r (state, s)), r

  let inline pfix (f: Parser<'a, 's> -> Parser<'a, 's>) =
    let p, pr = createParserForwardedToRef ()
    pr := f p
    p

  let inline getUserState (state, s) = Ok (state, s, state)
  let inline setUserState state : Parser<unit, 'State> = fun (_, s) -> Ok ((), s, state)
  let inline updateUserState f : Parser<unit, 'State> = fun (state, s) -> Ok ((), s, f state)
  let inline userStateSatisfies (cond: 's -> bool) : Parser<unit, 's> =
    fun (state, s) ->
      if cond state then Ok ((), s, state)
      else Error (s.startIndex, lazy "userStateSatisfies failed", state)
  let inline getPosition (state, s) = Ok (s.startIndex, s, state)

  [<Sealed>]
  type ParserCombinator() =
    member inline __.Delay(f)   = fun state -> (f ()) state
    member inline __.Return(x)  = preturn x
    member inline __.Bind(p, f) = p >>= f
    member inline __.Zero()     = pzero
    member inline __.ReturnFrom(p) = p
    member inline __.TryWith(p, cf) =
      fun state -> try p state with e -> (cf e) state
    member inline __.TryFinally(p, ff) =
      fun state -> try p state finally ff ()

  let parse = ParserCombinator()

open Primitives

/// MiniParsec does not support fatal errors.
/// every MiniParsec's function backtracks by default.
module FParsecCompat =
  let inline attempt (p: Parser<_, _>) = p
  let inline ( >>=? ) p1 p2 = p1 >>= p2
  let inline ( >>?  ) p1 p2 = p1 >>. p2
  let inline ( .>>? ) p1 p2 = p1 .>> p2
  let inline ( .>>.? ) p1 p2 = p1 .>>. p2
  let inline ( <??> ) p msg = p <?> msg
  let inline failFatally s = fail s

[<AutoOpen>]
module CharParsers =
  open StringSegment

  let inline charReturn c v : Parser<'a, _> =
    fun (state, s) ->
      match getSafe 0 s with
      | EOS -> Error (s.startIndex, lazy sprintf "Expected '%c', got EOS." c, state)
      | head ->
        if head = c then Ok (v, s |> skip 1, state)
        else Error (s.startIndex, lazy sprintf "Expected '%c', got '%c'." c head, state)
  let inline pchar c = charReturn c c
  let inline skipChar c = charReturn c ()

  let inline anyChar (state, s) =
    match getSafe 0 s with
    | EOS -> Error (s.startIndex, lazy sprintf "Expected any char, got EOS.", state)
    | c   -> Ok (c, s |> skip 1, state)
  let inline skipAnyChar (state, s) =
    match getSafe 0 s with
    | EOS -> Error (s.startIndex, lazy sprintf "Expected any char, got EOS.", state)
    | _   -> Ok ((), s |> skip 1, state)

  type CharSet = System.Collections.Generic.HashSet<char>

  let inline satisfyL (cond: char -> bool) (label: Lazy<string>) : Parser<char, _> =
    fun (state, s) ->
      match getSafe 0 s with
      | EOS -> Error (s.startIndex, lazy sprintf "Expected %s, got EOS." label.Value, state)
      | c ->
        if cond c then Ok (c, s |> skip 1, state)
        else
          Error (s.startIndex, lazy sprintf "Expected %s, got %c." label.Value c, state)
  let inline skipSatisfyL cond label : Parser<unit, _> = satisfyL cond label >>% ()

  let inline satisfy cond : Parser<char, _> = satisfyL cond (lazy "a char with condition")
  let inline skipSatisfy cond : Parser<unit, _> = skipSatisfyL cond (lazy "a char with condition")

  let inline anyOf (chars: char seq) : Parser<char, _> =
    let set = CharSet(chars)
    satisfyL set.Contains (lazy sprintf "one of %A" (Seq.toList chars))
  let inline skipAnyOf (chars: char seq) : Parser<unit, _> =
    let set = CharSet(chars)
    skipSatisfyL set.Contains (lazy sprintf "one of %A" (Seq.toList chars))
  let inline noneOf (chars: char seq) : Parser<char, _> =
    let set = CharSet(chars)
    satisfyL (set.Contains >> not) (lazy sprintf "one of %A" (Seq.toList chars))
  let inline skipNoneOf (chars: char seq) : Parser<unit, _> =
    let set = CharSet(chars)
    skipSatisfyL (set.Contains >> not) (lazy sprintf "one of %A" (Seq.toList chars))

  open System

  let inline asciiLower (state, s) = satisfyL (fun c -> 'a' <= c && c <= 'z') (lazy "[a-z]") (state, s)
  let inline asciiUpper (state, s) = satisfyL (fun c -> 'A' <= c && c <= 'Z') (lazy "[A-Z]") (state, s)
  let inline asciiLetter (state, s) = satisfyL (fun c -> ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')) (lazy "[a-zA-Z]") (state, s)
  let inline lower (state, s) = satisfyL Char.IsLower (lazy "Lowercase Letter") (state, s)
  let inline upper (state, s) = satisfyL Char.IsUpper (lazy "Uppercase Letter") (state, s)
  let inline letter (state, s) = satisfyL Char.IsLetter (lazy "Letter") (state, s)
  let inline digit (state, s) = satisfyL Char.IsDigit (lazy "[0-9]") (state, s)
  let inline hex (state, s) = satisfyL (fun c -> Char.IsDigit c || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f')) (lazy "[0-9a-fA-F]") (state, s)
  let inline octal (state, s) = satisfyL (fun c -> '0' <= c && c <= '7') (lazy "[0-7]") (state, s)
  let inline tab (state, s) = pchar '\t' (state, s)

  let inline newlineReturn v : Parser<'a, _> =
    fun (state, s) ->
      match getSafe 0 s with
      | '\n' -> Ok (v, s |> skip 1, state)
      | _ -> Error (s.startIndex, lazy sprintf "Expected newline.", state)
  let inline newline (state, s) = newlineReturn '\n' (state, s)
  let inline skipNewline (state, s) = newlineReturn () (state, s)

  let inline spaces (state, s) =
    let rec go (s: StringSegment) =
      match getSafe 0 s with
      | '\n' | '\t' | ' ' -> go (s |> skip 1)
      | _ -> Ok ((), state, s)
    go s
  let inline spaces1 (state, s) =
    match getSafe 0 s with
    | '\n' | '\t' | ' ' ->
      let rec go (s: StringSegment) =
        match getSafe 0 s with
        | '\n' | '\t' | ' ' -> go (s |> skip 1)
        | _ -> Ok ((), state, s)
      go (s |> skip 1)
    | _ -> Error (s.startIndex, lazy "Expected one or more spaces", state)
  let inline eof (state, s) =
    match getSafe 0 s with
    | EOS -> Ok ((), state, s)
    | _ -> Error (s.startIndex, lazy "Expected EOF", state)

  let inline stringReturn (str: string) v : Parser<'a, 's> =
    fun (state, s) ->
      if s |> startsWith str then
        Ok (v, s |> skip str.Length, state)
      else
        Error (s.startIndex, lazy "Expected '%s'", state)
  let inline pstring str : Parser<string, 's> = stringReturn str str
  let inline skipString str : Parser<unit, 's> = stringReturn str ()

open CharParsers

module Extensions =
  module Convert =
    let inline private foldi folder state xs =
      Seq.fold (fun (i, state) x -> (i + 1, folder i state x)) (0, state) xs |> snd
    let inline hexsToInt (hexs: #seq<char>) =
      let len = Seq.length hexs - 1
      hexs |> foldi (fun i sum x ->
        let n =
          let n = int x - int '0'
          if n < 10 then n
          else if n < 23 then n - 7
          else n - 44
        sum + n * pown 16 (len - i)) 0
    let inline digitsToInt (digits: #seq<char>) =
      let len = Seq.length digits - 1
      digits |> foldi (fun i sum x ->
        sum + (int x - int '0') * pown 10 (len - i)) 0

  /// Variant of `<|>` but accept different types of parsers and returns `Choice<'a, 'b>`.
  let inline (<||>) a b = (a |>> Choice1Of2) <|> (b |>> Choice2Of2)

  /// short hand for `skipString s`
  let inline syn s = skipString s

  /// short hand for `skipChar c `
  let inline cyn c = skipChar c

  /// short hand for `x .>>? spaces`
  let inline ws x = x .>>? spaces

  /// Given a sequence of `(key, value)`, parses the string `key`
  /// and returns the corresponding `value`.
  let inline pdict (d: list<_*_>) =
    d |> List.map (fun (k, v) -> pstring k >>% v) |> choice

  /// Optimized version of `pdict d <?> descr`.
  let inline pdictL (d: list<_*_>) descr =
    d |> List.map (fun (k, v) -> pstring k >>% v) |> choiceL <| descr

  /// String with escaped characters. Should be used along with `between`.
  let inline escapedString (escapedChars: #seq<char>) =
    let controls =
      pdictL [
        "\\b", '\b'; "\\t", '\t'; "\\n", '\n';
        "\\v", '\u000B'; "\\f", '\u000C'; "\\r", '\r'; "\\\\", '\\'
      ] "control characters"
    let unicode16bit =
      syn "\\u" >>. parray 4 hex |>> (Convert.hexsToInt >> char)
    let unicode32bit =
      syn "\\U" >>. parray 8 hex |>> (Convert.hexsToInt >> char)
    let customEscapedChars =
      let d = escapedChars |> Seq.map (fun c -> sprintf "\\%c" c, c) |> Seq.toList
      pdict d
    
    let escape = choice [controls; unicode16bit; unicode32bit; customEscapedChars]
    let nonEscape = noneOf (sprintf "\\\b\t\n\u000B\u000C\r%s" (System.String.Concat escapedChars))
    let character = nonEscape <|> escape
    many character |>> System.String.Concat

  /// Defines a recursive rule.
  let inline recursive (definition: (Parser<'a, _> -> Parser<'a, _>)) =
    let p, pr = createParserForwardedToRef()
    pr := definition p
    p

/// ISO8601-compliant Date/Time Parser.
/// See https://tools.ietf.org/html/iso8601#section-5.6 for details.
module ISO8601DateTime =
  open System
  open Extensions
  open Extensions.Convert

  // date-fullyear   = 4DIGIT
  // date-month      = 2DIGIT
  // date-mday       = 2DIGIT
  // full-date       = date-fullyear "-" date-month "-" date-mday
  let private iso8601_full_date =
    parray 4 digit .>>. parray 2 (cyn '-' >>. parray 2 digit)
    <?> "ISO8601 Full Date"
    |>> function 
      | (year, [|month; day|]) ->
        digitsToInt year, digitsToInt month, digitsToInt day
      | _ -> failwith "impossible"

  // time-hour       = 2DIGIT  ; 00-23
  // time-minute     = 2DIGIT  ; 00-59
  // time-second     = 2DIGIT  ; 00-5
  // time-secfrac    = "." 1*DIGIT
  // partial-time    = time-hour ":" time-minute ":" time-second [time-secfrac]
  let private iso8601_partial_time =
    parray 2 digit .>>. parray 2 (cyn ':' >>. parray 2 digit)
    .>>. opt (cyn '.' >>. many1 digit)
    <?> "ISO8601 Partial Time"
    |>> function
      | ((hour, [|minute; second|]), secfrac) ->
        digitsToInt hour, digitsToInt minute, digitsToInt second,
        secfrac
        |> Option.map (fun xs ->
          digitsToInt <|
            // we only respect up to the top 3 bits (milliseconds)
            if List.length xs > 3 then List.take 3 xs else xs)
        |> Option.defaultValue 0
      | _ -> failwith "impossible"

  // time-numoffset  = ("+" / "-") time-hour ":" time-minute
  // time-offset     = "Z" / time-numoffset
  // NOTE: Per [ABNF] and ISO8601, the "T" and "Z" characters in this
  //  syntax may alternatively be lower case "t" or "z" respectively.
  let private iso8601_offset =
    let sign = (cyn '+' >>% true) <|> (cyn '-' >>% false)
    let numoffset =
      sign .>>. parray 2 digit .>> cyn ':' .>>. parray 2 digit
      |>> fun ((sign, minute), second) -> sign, digitsToInt minute, digitsToInt second
    ((anyOf "zZ" >>% ()) <||> numoffset) <?> "ISO8601 Time Offset"

  // full-time       = partial-time time-offset
  let private iso8601_full_time =
    iso8601_partial_time .>>. iso8601_offset
    <?> "ISO8601 Full Time"
    |>> fun ((h,m,s,f),o) -> h,m,s,f,o

  // date-time       = full-date "T" full-time
  // NOTE: Per [ABNF] and ISO8601, the "T" and "Z" characters in this
  //  syntax may alternatively be lower case "t" or "z" respectively.
  // NOTE: ISO 8601 defines date and time separated by "T".
  //  Applications using this syntax may choose, for the sake of
  //  readability, to specify a full-date and full-time separated by
  //  (say) a space character.
  let private iso8601_date_time =
    iso8601_full_date .>> (anyOf "tT " >>% ()) .>>. iso8601_full_time
    <?> "ISO8601 Date Time"
    |>> fun ((Y,M,D), (h,m,s,f,o)) -> Y,M,D,h,m,s,f,o
  
  /// ISO8601-compliant partial-time parser.
  let ppartialtime : Parser<_, unit> =
    iso8601_partial_time |>> fun (h,m,s,f) -> DateTime(0,0,0,h,m,s,f,DateTimeKind.Local)
  
  /// ISO8601-compliant full-time parser.
  let pfulltime : Parser<_, unit> =
    iso8601_full_time |>> function
      | (h,m,s,f,Choice1Of2()) ->
        DateTimeOffset(0,0,0,h,m,s,f,TimeSpan.Zero)
      | (h,m,s,f,Choice2Of2(sign, oh, om)) ->
        let inline sign x = if sign then x else -x
        DateTimeOffset(0,0,0,h,m,s,f,TimeSpan(sign oh, sign om, 0))

  /// ISO8601-compliant full-date parser.
  let pfulldate : Parser<_, unit> =
    iso8601_full_date |>> fun (y,m,d) -> DateTime(y,m,d,0,0,0,DateTimeKind.Local)

  /// ISO8601-compliant datetime parser.
  let pdatetime : Parser<_, unit> =
    iso8601_date_time |>> function
      | (Y,M,D,h,m,s,f,Choice1Of2()) ->
        DateTimeOffset(Y,M,D,h,m,s,f,TimeSpan.Zero)
      | (Y,M,D,h,m,s,f,Choice2Of2(sign, oh, om)) ->
        let inline sign x = if sign then x else -x
        DateTimeOffset(Y,M,D,h,m,s,f,TimeSpan(sign oh, sign om, 0))

let f x = runString ISO8601DateTime.pdatetime () x

let test() =
  match f "2019-12-10T14:57:13+09:00" with
  | Ok (date, _, _) -> printfn "success: %A" date
  | Error e -> printfn "error: %A" e
