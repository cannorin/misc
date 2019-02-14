// normal let-expression
let a = 1 in printfn "normal let:%d" a ;;

// continuation passing style
(fun a -> printfn "cps: %d" a) 1 ;;

// - with pipe operator
1 |> (fun a -> printfn "cps with pipe: %d" a) ;;

// name it 'bind'
let bind (x, f) = f x
bind (1, fun a -> printfn "bind: %d" a) ;;

// a bit complex case
let a = 1 in
  let b = 2 in
    printfn "complex-let: %d" (a+b)
;;

bind (1, fun a ->
  bind (2, fun b ->
    printfn "complex-bind: %d" (a+b)))
;;

// the simplest builder
type SimplestBuilder() =
  member this.Bind(x: 'a, f: 'a -> 'b) : 'b =
    f x
  member this.Return (x: 'a) : 'a = // I'll explain this later
    x 

let sb = SimplestBuilder()

// and its usage
sb.Bind (1, fun a ->
  sb.Bind (2, fun b ->
    printfn "simple builder: %d" (a+b);
    ()
  )
);;

// computation expression is just a syntax suger for the above!
sb {
  let! a = 1
  let! b = 2
  printfn "simple compexp: %d" (a+b)
  return ()
} ;;

// the simplest builder with logging
type LoggingBuilder() =
  member this.Bind(x, f) =
    printfn "  bind: %A" x
    f x
  member this.Return x = x
let logging = LoggingBuilder()

// observe the difference
logging {
  let! a = 1
  let b = 2 // this is the 'normal' let binding, and not converted to the Bind call!
  do printfn "logging compexp: %d" (a+b)
  return ()
} ;;

// you know you can insert 'background' operations between the let! bindings
// now consider the following type:
type Result = Success of int | Failure of errorMessage:string

// and the following function:
let safeHalf x =
  if x%2 = 0 then
    Success (x/2)
  else
    Failure (sprintf "%d is not divisible!" x)

// the error-checking becomes boilerplate..
let a = safeHalf 14 in
  match a with
  | Failure msg -> printfn "%s" msg
  | Success a' ->
    let b = safeHalf a' in
    match b with
    | Failure msg -> printfn "%s" msg
    | Success b' ->
      printfn "boilerplate result: %d" b'
;;

// then consider the following builder:
type ResultBuilder() =
  
  // `Bind` unwraps a container, then apply a function that creates a new container
  member this.Bind (x: Result, f: int -> Result) : Result = 
    match x with
    | Success x' -> f x'
    | Failure msg -> Failure msg

  // `Return` wraps a value to the container
  member this.Return (x: int) : Result =
    Success x

let result = ResultBuilder()

// and this solves the entire problem!
let divideTwice x =
  result {
    let! a = safeHalf x
    let! b = safeHalf a
    return b
  }

divideTwice 14 |> printfn "divideTwice 14 = %A"
divideTwice 16 |> printfn "divideTwice 16 = %A"

// the expanded form for reference:

let divideTwiceExpanded x =
  result.Bind (safeHalf x, fun a ->
    result.Bind (safeHalf a, fun b ->
      result.Return b // equivalent to `Success b`
    )
  )
divideTwiceExpanded 16 |> printfn "divideTwiceExpanded 16 = %A"
