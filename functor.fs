module FunctorTest
open System
open System.Collections.Generic

// 型変数を値変数として受け渡すためのダミー型
type typrm<'t> = struct end
let inline typrm<'t> : typrm<'t> = Unchecked.defaultof<typrm<'t>>

// 実装を使って比較
let inline private compareBy (_: typrm< ^OrderedType >) a b =
  (^OrderedType: (static member Compare: _ * _ -> int) a,b)

// F# の組み込みの比較機構を使うためのラッパー (任意の型に使える)
type DefaultComparison = DefaultComparison of unit with
  static member inline Compare (x: 't, y: 't) = compare x y

module Set =
  // Make も型なので入れ子にできないため外に出す
  type t<'elt, 'OrderedType> =
    | Empty
    | Node of t<'elt, 'OrderedType> * 'elt * t<'elt, 'OrderedType> * int

  type Make<'ord> = SetModule of unit with
    static member inline singleton x : t<_, 'ord> = Node (Empty, x, Empty, 1)
    
    // メンバ変数は型変数を取れない
    static member inline empty () : t<_, 'ord> = Empty

    static member inline height x = match x with Empty -> 0 | Node (_, _, _, h) -> h

    static member inline create l v r =
      let hl = Make<_>.height l
      let hr = Make<_>.height r
      Node (l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    static member inline bal (l: t<'elt, ^OrderedType>) (v: 'elt) (r: t<'elt, ^OrderedType>) =
      let inline height x = Make<_>.height x
      let inline create l v r = Make<_>.create l v r
      
      let hl = height l
      let hr = height r
      if hl > hr + 2 then
        match l with
          Empty -> invalidArg "l" "Set.bal"
        | Node(ll, lv, lr, _) ->
            if height ll >= height lr then
              create ll lv (create lr v r)
            else
              match lr with
                Empty -> invalidArg "l" "Set.bal"
              | Node(lrl, lrv, lrr, _)->
                create (create ll lv lrl) lrv (create lrr v r)
      else if hr > hl + 2 then
        match r with
          Empty -> invalidArg "r" "Set.bal"
        | Node(rl, rv, rr, _) ->
            if height rr >= height rl then
              create (create l v rl) rv rr
            else
              match rl with
                Empty -> invalidArg "r" "Set.bal"
              | Node(rll, rlv, rlr, _) ->
                  create (create l v rll) rlv (create rlr rv rr)
      else
        Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    static member inline add (x: 'elt) (tree: t<'elt, ^OrderedType>) =
      let inline bal x = Make<_>.bal x
      // インライン関数は直接再帰できないので中に再帰関数を作る
      let rec add x = function
        | Empty -> Node (Empty, x, Empty, 1)
        | Node (l, v, r, _) as t ->
          let c = compareBy typrm< ^OrderedType > x v
          if c = 0 then t else
          if c < 0 then bal (add x l) v r else bal l v (add x r)
      add x tree

    // 他の関数も同様に実装できる

// F# の組み込みの比較機構を使う Set モジュール (任意の型に使える)
type DefaultSet = Set.Make<DefaultComparison>

// open DefaultSet // ダメ(モジュールではないので)

let intSetDefault =
  DefaultSet.singleton 3
  |> DefaultSet.add 2
  |> DefaultSet.add 4
  |> DefaultSet.add 2

let strSetDefault =
  DefaultSet.singleton "hoge"
  |> DefaultSet.add "piyo"
  |> DefaultSet.add "hoge"

intSetDefault |> printfn "intSetDefault: %A"
strSetDefault |> printfn "strSetDefault: %A"

type A = A1 of int | A2 of string

// F# では自動で comparison が実装される
let aSetDefault =
  DefaultSet.singleton (A1 2)
  |> DefaultSet.add (A2 "foo")
  |> DefaultSet.add (A2 "2")
aSetDefault |> printfn "aSetDefault: %A"

// だがあえて自分で実装を与えてみる
type CustomAComparison = CustomAComparison with
  static member Compare (a, b) =
    match a, b with
      | A1 i1, A1 i2 -> compare i1 i2
      | A2 s1, A2 s2 -> compare s1 s2
      // 違うときは両方とも文字列にして比較 (A1 42 = A2 "42")
      | A1 i1, A2 s2 -> compare (string i1) s2
      | A2 s1, A1 i2 -> compare s1 (string i2)

type ASet = Set.Make<CustomAComparison>
let aSetCustom =
  ASet.singleton (A1 2)
  |> ASet.add (A2 "foo")
  |> ASet.add (A2 "2")
aSetCustom |> printfn "aSetCustom: %A"