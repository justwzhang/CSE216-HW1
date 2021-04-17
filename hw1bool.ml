(*Defines the language for Boolean logic*)
type bool_expr =
  | Lit of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

(*A recursive helper method for the last function below*)
let rec evaluate x a b boola boolb = match x with
  | Lit y -> if y=a then boola else boolb
  | Not y -> not(evaluate y a b boola boolb)
  | And (y, z)-> (evaluate y a b boola boolb) && (evaluate z a b boola boolb)
  | Or (y, z) -> (evaluate y a b boola boolb) || (evaluate z a b boola boolb);;

(*Returns a truth table in a list*)
let truth_table a b x = [(true, true, evaluate x a b true true);
                          (true, false, evaluate x a b true false);
                          (false, true, evaluate x a b false true);
                          (false, false, evaluate x a b false false)];;