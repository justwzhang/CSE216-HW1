(*Q1*)
(*Function that returns x to the power n*)
let rec pow x n = match n with 
  | 0 -> 1
  | 1 -> x
  | _ -> x * pow x (n-1);;

let rec float_pow x n = match n with 
  | 0 -> 1.0
  | 1 -> x
  | _ -> x *. float_pow x (n-1);;

(*Q2*)
(*Removes the consecutive duplicates from a list*)
let rec compress x = match x with
  | [] -> []
  | h::[] -> h::[]
  | h::x::t -> if h = x then compress (x::t) else h :: compress (x::t);;

(*Q3*)
(*If an item of the list satisfies the function condition then it removes it*)
let rec remove_if x f = match x with
  | [] -> []
  | h::t -> if f h = false then h:: remove_if t f else remove_if t f;;

(*Q4*)
(*Returns a list from index i to and not including j*)
let rec slice l i j = match l with
  | [] -> []
  | h::t -> if i>j then [] else if i>0 || j<=0 then slice t (i-1) (j-1) else h:: slice t (i-1) (j-1);;

(*Q5*)
(*Partitionsl a list into equivalence classes*)
let rec q5remover x l f = match l with
  | [] -> []
  | h::t -> if(f x h) then q5remover x t f else h::q5remover x t f;;

let rec q5finder x l f = match l with
  | [] -> []
  | h::t -> if(f x h) then h::q5finder x t f else q5finder x t f;;

let rec equivs f l = match l with
  | [] -> []
  | h::t -> (h :: (q5finder h t f))::equivs f (q5remover h t f);;

(*Q6*)
(*Finds a set of numbers that satisfies the Goldbach's conjecture*)
let rec check_prime x y = match y with
  | 1 ->  true
  | _ -> if(x mod y = 0) then false else check_prime x (y-1);;

let rec q6helper x y = if(check_prime (x-y) (x-y-1) && check_prime (y) (y-1)) 
  then (if y<(x-y) then (y, x-y)else (x-y, y))  
  else q6helper x (y+1);;

let goldbachpair x = if x mod 2 = 0 then q6helper x 2 else (0,0);;

(*Q7*)
(*Checks the equivalences of a function if they have the same effect on all the elements of a list*)
let rec equiv_on f g lst = match lst with
  | [] -> true
  | h::t -> (f h = g h) && equiv_on f g t;;

(*Q8*)
(*Returns a list that contains the elements returned from a function*)
let rec pairwisefilter cmp lst = match lst with
  | [] -> []
  | h::[] ->[h]
  | h::x::t -> cmp h x ::pairwisefilter cmp t;;

(*Q9*)
(*Turns a list of tuples into a polynomial*)
let rec polynomial l x = match l with
  | [] -> 0
  | h::t -> match h with
    | (w,y) ->  w * pow x y + polynomial t x;;

(*Q10*)
(*Returns the power set of a list*)
let rec q10helper f l = match l with
  | [] ->[]
  | h::t -> f h:: q10helper f t;;

let rec powerset l = match l with
  | []->[[]]
  | h::t -> powerset t @ q10helper (fun y -> h::y) (powerset t);;