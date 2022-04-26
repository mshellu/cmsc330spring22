open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
(* token list -> token list * expr *)
let rec parse_expr toks = 
  let (t, exp) = 
    match lookahead toks with 
    | Some Tok_Let -> parse_LetExpr toks 
    | Some Tok_If -> parse_IfExpr toks 
    | Some Tok_Fun -> parse_FunctionExpr toks 
    | _ -> parse_OrExpr toks 
  in (t, exp)

and parse_LetExpr toks = 
  match lookahead toks with 
  | Some Tok_Let -> let t1 = match_token toks Tok_Let in 
      let (t2, s1) = parse_Recursion t1 in 
      let (t3, s2) = 
        match lookahead t2 with 
        | Some Tok_ID id -> (match_token t2 (Tok_ID (id)), id) 
        | _ -> raise (InvalidInputException "parser_LetExpr")
        in 
      let t4 = match_token t3 Tok_Equal in 
      let (t5, s3) = parse_expr t4 in 
      let t6 = match_token t5 Tok_In in 
      let (t7, s4) = parse_expr t6 in 
      (t7, Let (s2, s1, s3, s4))
  | _ -> raise (InvalidInputException "parser_LetExpr")

and parse_Recursion toks = 
  match lookahead toks with 
  | Some Tok_Rec -> let t = match_token toks Tok_Rec in (t, true)
  | _ -> (toks, false)

and parse_IfExpr toks = 
  match lookahead toks with 
  | Some Tok_If -> let t1 = match_token toks Tok_If in 
      let (t2, s1) = parse_expr t1 in 
      let t3 = match_token t2 Tok_Then in 
      let (t4, s2) = parse_expr t3 in 
      let t5 = match_token t4 Tok_Else in 
      let (t6, s3) = parse_expr t5 in 
      (t6, If(s1, s2, s3))
  | _ -> raise (InvalidInputException "parser_IfExpr")

and parse_FunctionExpr toks = 
  match lookahead toks with 
  | Some Tok_Fun -> let t1 = match_token toks Tok_Fun in 
    let (t2, s1) = 
      match lookahead t1 with 
      | Some Tok_ID id -> (match_token t1 (Tok_ID (id)), id) 
      | _ -> raise (InvalidInputException "parser_LetExpr")
    in 
    let t3 = match_token t2 Tok_Arrow in 
    let (t4, s2) = parse_expr t3 in 
    (t4, Fun(s1, s2))
  | _ -> raise (InvalidInputException "parser_FunctionExpr")

and parse_OrExpr toks = 
  let (t1, s1) = parse_AndExpr toks in 
  match lookahead t1 with 
  | Some Tok_Or -> 
      let t2 = match_token t1 Tok_Or in
      let (t3, s2) = parse_OrExpr t2 in 
      (t3, Binop(Or, s1, s2))
  | _ -> (t1, s1)

and parse_AndExpr toks =
  let (t1, s1) = parse_EqualityExpr toks in 
  match lookahead t1 with 
  | Some Tok_And -> 
      let t2 = match_token t1 Tok_And in 
      let (t3, s2) = parse_AndExpr t2 in 
      (t3, Binop(And, s1, s2))
  | _ -> (t1, s1)

and parse_EqualityExpr toks = 
  let (t1, s1) = parse_RelationalExpr toks in 
  match lookahead t1 with 
  | Some Tok_Equal -> 
      let t2 = match_token t1 Tok_Equal in 
      let (t3, s2) = parse_EqualityExpr t2 in 
      (t3, Binop(Equal, s1, s2))
  | Some Tok_NotEqual -> 
      let t2 = match_token t1 Tok_NotEqual in 
      let (t3, s2) = parse_EqualityExpr t2 in 
      (t3, Binop(NotEqual, s1, s2))
  | _ -> (t1, s1) (* only parse_RelationalExpr *)

and parse_RelationalExpr toks = 
  let (t1, s1) = parse_AdditiveExpr toks in 
  match lookahead t1 with 
  | Some Tok_Less -> 
      let t2 = match_token t1 Tok_Less in 
      let (t3, s2) = parse_RelationalExpr t2 in 
      (t3, Binop(Less, s1, s2)) 
  | Some Tok_Greater -> 
      let t2 = match_token t1 Tok_Greater in 
      let (t3, s2) = parse_RelationalExpr t2 in 
      (t3, Binop(Greater, s1, s2)) 
  | Some Tok_LessEqual -> 
      let t2 = match_token t1 Tok_LessEqual in 
      let (t3, s2) = parse_RelationalExpr t2 in 
      (t3, Binop(LessEqual, s1, s2)) 
  | Some Tok_GreaterEqual -> 
      let t2 = match_token t1 Tok_GreaterEqual in 
      let (t3, s2) = parse_RelationalExpr t2 in 
      (t3, Binop(GreaterEqual, s1, s2)) 
  | _ -> (t1, s1)

and parse_AdditiveExpr toks = 
  let (t1, s1) = parse_MultiplicativeExpr toks in 
  match lookahead t1 with 
  | Some Tok_Add -> 
      let t2 = match_token t1 Tok_Add in 
      let (t3, s2) = parse_AdditiveExpr t2 in 
      (t3, Binop(Add, s1, s2))
  | Some Tok_Sub -> 
      let t2 = match_token t1 Tok_Sub in 
      let (t3, s2) = parse_AdditiveExpr t2 in 
      (t3, Binop(Sub, s1, s2))
  | _ -> (t1, s1)

and parse_MultiplicativeExpr toks = 
  let (t1, s1) = parse_ConcatExpr toks in 
  match lookahead t1 with 
  | Some Tok_Mult -> 
      let t2 = match_token t1 Tok_Mult in 
      let (t3, s2) = parse_MultiplicativeExpr t2 in 
      (t3, Binop (Mult, s1, s2))
  | Some Tok_Div -> 
      let t2 = match_token t1 Tok_Div in 
      let (t3, s2) = parse_MultiplicativeExpr t2 in 
      (t3, Binop (Div, s1, s2))
  | _ -> (t1, s1)

and parse_ConcatExpr toks = 
  let (t1, s1) = parse_UnaryExpr toks in 
  match lookahead t1 with 
  | Some Tok_Concat -> 
      let t2 = match_token t1 Tok_Concat in 
      let (t3, s2) = parse_ConcatExpr t2 in 
      (t3, Binop (Concat, s1, s2))
  | _ -> (t1, s1)

and parse_UnaryExpr toks = 
  match lookahead toks with 
  | Some Tok_Not -> 
      let t1 = match_token toks Tok_Not in 
      let (t2, s1) = parse_UnaryExpr t1 in 
      (t2, Not (s1))
  | _ -> parse_FunctionCallExpr toks

(*FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr*)
and parse_FunctionCallExpr toks = 
  let (t1, s1) = parse_PrimaryExpr toks in 
  match lookahead t1 with 
  | Some Tok_Int i -> let (t2, s2) = parse_PrimaryExpr t1 in (t2, FunctionCall(s1, s2))
  | Some Tok_Bool b -> let (t2, s2) = parse_PrimaryExpr t1 in (t2, FunctionCall(s1, s2))
  | Some Tok_String s -> let (t2, s2) = parse_PrimaryExpr t1 in (t2, FunctionCall(s1, s2))
  | Some Tok_ID id -> let (t2, s2) = parse_PrimaryExpr t1 in (t2, FunctionCall(s1, s2))
  | Some Tok_LParen -> let (t2, s2) = parse_PrimaryExpr t1 in (t2, FunctionCall(s1, s2))
  | _ -> (t1, s1)

and parse_PrimaryExpr toks = 
  match lookahead toks with 
  | Some Tok_Int i -> let t = match_token toks (Tok_Int i) in (t, Value(Int i))
  | Some Tok_Bool b -> let t = match_token toks (Tok_Bool b) in (t, Value(Bool b))
  | Some Tok_String s -> let t = match_token toks (Tok_String s) in (t, Value(String s))
  | Some Tok_ID id -> let t = match_token toks (Tok_ID id) in (t, ID(id))
  | Some Tok_LParen -> let t = match_token toks Tok_LParen in 
      let (t1, s) = parse_expr t in 
      let t2 = match_token t1 Tok_RParen in 
      (t2, s)
  | _ -> raise (InvalidInputException "parser_PrimaryExpr")

(* Part 3: Parsing mutop *)
(* token list -> token list * mutop *)
let rec parse_mutop toks = 
  match lookahead toks with 
  | Some Tok_Def -> parse_DefMuTop toks 
  | Some Tok_DoubleSemi -> let toks' = match_token toks Tok_DoubleSemi in (toks', NoOp)
  | _ -> parse_ExprMuTop toks 

(* -> var * expr *)
and parse_DefMuTop toks = 
  match lookahead toks with 
  | Some Tok_Def -> 
      let t1 = match_token toks Tok_Def in 
      let (t2, s1) = match lookahead t1 with 
      | Some Tok_ID id -> (match_token t1 (Tok_ID id), id)
      | _ -> raise (InvalidInputException "parser_DefMuTop, match ID")
      in 
      let t3 = match_token t2 Tok_Equal in 
      let (t4, s2) = parse_expr t3 in 
      let t5 = match_token t4 Tok_DoubleSemi in 
      (t5, Def (s1, s2))
  | _ -> raise (InvalidInputException "parser_DefMuTop")

(* -> expr *)
and parse_ExprMuTop toks = 
  let (t, s) = parse_expr toks in 
  let t' = match lookahead t with 
  | Some Tok_DoubleSemi -> match_token t Tok_DoubleSemi
  | _ -> raise (InvalidInputException "parser_ExprMuTop")
  in 
  (t', Expr(s))