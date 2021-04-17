(*2.2*)
(*Defines the language used for expressions*)
type expr = 
  | Const of int
  | Var of string
  | Plus of args
  | Minus of args
  | Mult of args
  | Div of args
and args = {arg1:expr; arg2:expr};;

(*2.3*)
(*Evalueates the expression without variables in the form defined above*)
let rec evaluate exp = match exp with
  | Const x -> x
  | Plus {arg1; arg2} -> evaluate arg1 + evaluate arg2
  | Minus  {arg1; arg2} -> evaluate arg1 - evaluate arg2
  | Mult  {arg1; arg2} -> evaluate arg1 * evaluate arg2
  | Div  {arg1; arg2} -> evaluate arg1 / evaluate arg2;;