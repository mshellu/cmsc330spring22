open MicroCamlTypes
open Utils
open String 

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
(* environment -> expr -> value *)
let rec eval_expr env e = match e with 
| Value v -> v 
| ID i -> lookup env i (* lookup declares error if var not found *)
| Not n -> (match eval_expr env n with 
  | Bool b -> Bool (not b)
  | _ -> raise (TypeError ("Expected type bool")))
| Binop (op, e1, e2) -> (match op with 
  | Add -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Int (x + y)
    | _, _ -> raise (TypeError ("Expected type int")) )
  | Div -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> if y = 0 then raise DivByZeroError else Int (x / y)
    | _, _ -> raise (TypeError ("Expected type int")) )
  | Sub -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Int (x - y)
    | _, _ -> raise (TypeError ("Expected type int")) )
  | Mult -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Int (x * y)
    | _, _ -> raise (TypeError ("Expected type int")) )
  | Greater -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Bool (x > y)
    | _, _ -> raise (TypeError ("Expected type int")) )
  | Less -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Bool (x < y)
    | _, _ -> raise (TypeError ("Expected type int")) )
  | GreaterEqual -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Bool (x >= y)
    | _, _ -> raise (TypeError ("Expected type int")) )
  | LessEqual -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Bool (x <= y)
    | _, _ -> raise (TypeError ("Expected type int")) )
  | Concat -> (match (eval_expr env e1), (eval_expr env e2) with 
    | String x, String y -> String (x^y)
    | _, _ -> raise (TypeError ("Expected type string")) )
  | Equal -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Bool (x = y)
    | Bool x,  Bool y -> Bool (x = y)
    | String x, String y -> Bool (x = y)
    | _, _ -> raise (TypeError ("Cannot compare types")) )
  | NotEqual -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Int x, Int y -> Bool (x != y)
    | Bool x,  Bool y -> Bool (x != y)
    | String x, String y -> Bool (x != y)
    | _, _ -> raise (TypeError ("Cannot compare types")) )
  | Or -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Bool x, Bool y -> Bool (x || y)
    | _, _ -> raise (TypeError ("Expected type bool")) )
  | And -> (match (eval_expr env e1), (eval_expr env e2) with 
    | Bool x, Bool y -> Bool (x && y)
    | _, _ -> raise (TypeError ("Expected type bool")) ) )

| If (e1, e2, e3) -> (match eval_expr env e1 with 
  | Bool true -> eval_expr env e2 
  | Bool false -> eval_expr env e3
  | _ -> raise (TypeError ("Expected type bool")) )
| Let (var, b, e1, e2) -> if b = false then 
  let v = eval_expr env e1 in 
    (* non recursive *) 
    eval_expr (extend env var v) e2 (* update if already exists?? *)
  else (* recursive *)
    (* evaluate e1 in envir with extended temp variable *)
    let new_env = extend_tmp env var in 
    let v = eval_expr new_env e1 in 
      update new_env var v; eval_expr new_env e2
| Fun (var, e) -> Closure (env, var, e)
| FunctionCall (e1, e2) -> (match eval_expr env e1, eval_expr env e2 with 
  | Closure (a, x, e), v -> eval_expr (extend a x v) e (* need to pattern match v? *)
  | _, _ -> raise (TypeError ("Not a function")) )

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with 
| Def (var, e) -> 
  let new_env = extend_tmp env var in 
  let v = eval_expr new_env e in 
  update new_env var v; (new_env, Some v)

| Expr e -> (env, Some (eval_expr env e))
| NoOp -> ([], None)