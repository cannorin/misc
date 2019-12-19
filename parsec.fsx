#if COMPILED
module MiniParsec
#endif

type StringSegment = {
  startIndex: int
  length: int
  underlying: string
} with
  member inline this.Value = this.underlying.Substring(this.startIndex, this.length)
  member inline this.Item index =
    if index < 0 || index >= this.length then failwith "Index was out of range (Item)."
    else this.underlying.[this.startIndex + index]
  member this.GetSlice (start, finish) =
    let start = defaultArg start 0
    let finish = defaultArg finish (this.length - 1)
    let inline check x = x >= 0 && x < this.length
    if check start && check finish then
      { underlying = this.underlying; startIndex = this.startIndex + start; length = finish - start + 1 }
    else failwith "Index was out of range (GetSlice)."

module StringSegment =
  let inline ofString (str: string) = { underlying = str; startIndex = 0; length = str.Length }
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
  let inline take length (seg: StringSegment) = seg.[..length-1]
  let inline skip length (seg: StringSegment) =
    { underlying = seg.underlying; startIndex = seg.startIndex + length; length = seg.length - length }
  let takeWhile (cond: char -> bool) (seg: StringSegment) =
    let rec check i =
      if i = seg.length || not (cond seg.[i]) then i
      else check (i+1)
    take (check 0) seg
  let skipWhile (cond: char -> bool) (seg: StringSegment) =
    let rec check i =
      if i = seg.length || not (cond seg.[i]) then i
      else check (i+1)
    skip (check 0) seg
      
open StringSegment

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
let inline runString (p: Parser<'a, 's>) state input = p (state, ofString input)

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
    let mutable result = Array.zeroCreate len
    let rec go i (state, s) =
      if i = len then Ok (result, s, state)
      else
        match run p state s with
        | Error e -> Error e
        | Ok (r, s, state) ->
          result.[i] <- r
          go (i+1) (state, s)
    go 0

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
  let inline getPosition (state, s) = Ok (s.startIndex, s, state)

open Primitives

[<AutoOpen>]
module CharParsers =
  let inline pchar c : Parser<char, _> =
    fun (state, s) ->
      if s |> StringSegment.isEmpty then Error (s.startIndex, lazy "EOF", state)
      else
        let head = s.[0]
        if head = c then Ok (head, s |> skip 1, state)
        else Error (s.startIndex, lazy sprintf "Expected '%c', got '%c'" c head, state)

  let inline anyOf (chars: char seq) : Parser<char, _> =
    fun (state, s) ->
      if s |> StringSegment.isEmpty then Error (s.startIndex, lazy "EOF", state)
      else
        let head = s.[0]
        if chars |> Seq.contains head then
          Ok (head, s |> skip 1, state)
        else
          Error (s.startIndex, lazy sprintf "Expected one of %A, but got %c." (Seq.toList chars) head, state)

open CharParsers

let num: Parser<int, unit> = anyOf "0123456789" |>> fun c -> int c - int '0'

let p = chainl1 (num |>> fun x -> [x]) (pchar '+' >>% (@))
