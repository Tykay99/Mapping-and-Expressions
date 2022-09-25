(* 

Name: Ty Kay
Email: tkay@princeton.edu

*)

open Ast 
open ExpressionLibrary 

(*>* Problem 2.1 *>*)

(* evaluate : evaluates an expression for a particular value of x. 
 *  Example : evaluate (parse "x*x + 3") 2.0 = 7.0 *)

let rec evaluate (e:expression) (x:float) : float =
  match e with
  | Num n -> n
  | Var -> x
  | Binop (b,e1,e2) -> match b with
    | Add -> evaluate(e1)(x) +. evaluate(e2)(x)
    | Sub -> evaluate(e1)(x) -. evaluate(e2)(x)
    | Mul -> evaluate(e1)(x) *. evaluate(e2)(x)

(*>* Problem 2.2 *>*)

(* derivate : calculates derivative of an expression *)

let rec derivative (e:expression) : expression =
  match e with
  | Num n -> Num 0.0
  | Var -> Num 1.0
  | Binop (b,e1,e2) -> match b with
    | Mul -> Binop(Add,(Binop(Mul,derivative(e1),e2)),
    (Binop(Mul,derivative(e2),e1)))
    | _ -> Binop(b,derivative(e1),derivative(e2))

(*>* Problem 2.3 *>*)

(* find_zero : uses Newton's Method to calculate approximate zero of 
expression, within a bound of epsilon, in less than lim recursive calls *)

let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  if lim < 0 then None
  else let temp = evaluate e g in
  if temp < epsilon && (-.temp < epsilon) then Some g
  else let update = g -. temp /. evaluate (derivative(e)) g in
  find_zero e update epsilon (lim - 1)

