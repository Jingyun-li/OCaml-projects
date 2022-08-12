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

let rec parse_expr toks = 
  match lookahead toks with
  | Some Tok_Let -> parse_LetExpr toks 
  | Some Tok_If -> parse_IfExpr toks 
  | Some Tok_Fun -> parse_FunctionExpr toks
  | _ -> parse_OrExpr toks
        
and parse_LetExpr toks =
  let t = match_token toks Tok_Let in
  let t', v = match lookahead t with
    | Some Tok_Rec -> match_token t Tok_Rec, true
    | _ -> t, false in
  let t'', d = match lookahead t' with 
    | Some Tok_ID d -> match_token t' (Tok_ID d), d
    | _ -> raise (InvalidInputException "parse_LetExpr") in
  let l = match_token t'' Tok_Equal in
  let (a, e') = parse_expr l in
  let l' = match_token a Tok_In in
  let (a', e'') = parse_expr l' in
  (a', Let(d, v, e', e''))

and parse_IfExpr toks =
  let t = match_token toks Tok_If in
  let (a, e) = parse_expr t in
  let t' = match_token a Tok_Then in
  let (a', e') = parse_expr t' in
  let t'' = match_token a' Tok_Else in
  let (a'', e'') = parse_expr t'' in
  (a'', If(e,e',e''))

and parse_FunctionExpr toks =
  let t = match_token toks Tok_Fun in
  let t', d = match lookahead t with 
    | Some Tok_ID d -> match_token t (Tok_ID d), d
    | _ -> raise (InvalidInputException "parse_FunctionExpr") in
  let t'' = match_token t' Tok_Arrow in
  let (a, e) = parse_expr t'' in
  (a, Fun(d, e))
  
and parse_OrExpr toks =
  let (t, a) = parse_AndExpr toks in
  match lookahead t with
  | Some Tok_Or ->
      let t' = match_token t Tok_Or in
      let (t'', b) = parse_OrExpr t' in
      (t'', Binop(Or, a, b))
  | _ -> (t, a)
  
and parse_AndExpr toks =
  let (t, a) = parse_EqualityExpr toks in
  match lookahead t with
  | Some Tok_And ->
      let t' = match_token t Tok_And in
      let (t'', b) = parse_AndExpr t' in
      (t'', Binop(And, a, b))
  | _ -> (t, a)
    

and parse_EqualityExpr toks =
  let (t, a) = parse_RelationalExpr toks in
  match lookahead t with
  | Some Tok_Equal ->
      let t' = match_token t Tok_Equal in
      let (t'', b) = parse_EqualityExpr t' in
      (t'', Binop(Equal, a, b))
  | Some Tok_NotEqual ->
      let t' = match_token t Tok_NotEqual in
      let (t'', b) = parse_EqualityExpr t' in
      (t'', Binop(NotEqual, a, b))
  | _ -> (t, a)
      
and parse_RelationalExpr toks =
  let (t, a) = parse_AdditiveExpr toks in 
  match lookahead t with
  | Some Tok_Greater ->
      let t' = match_token t Tok_Greater in
      let (t'', b) = parse_RelationalExpr t' in
      (t'', Binop(Greater, a, b))
  | Some Tok_GreaterEqual ->
      let t' = match_token t Tok_GreaterEqual in
      let (t'', b) = parse_RelationalExpr t' in
      (t'', Binop(GreaterEqual, a, b))
  | Some Tok_Less ->
      let t' = match_token t Tok_Less in
      let (t'', b) = parse_RelationalExpr t' in
      (t'', Binop(Less, a, b))
  | Some Tok_LessEqual ->
      let t' = match_token t Tok_LessEqual in
      let (t'', b) = parse_RelationalExpr t' in
      (t'', Binop(LessEqual, a, b)) 
  | _ -> (t, a)

and parse_AdditiveExpr toks =
  let (t, a) = parse_MultiplicativeExpr toks in 
  match lookahead t with 
  | Some Tok_Add ->
      let t' = match_token t Tok_Add in
      let (t'', b) = parse_AdditiveExpr t' in
      (t'', Binop(Add, a, b))
  | Some Tok_Sub ->
      let t' = match_token t Tok_Sub in
      let (t'', b) = parse_AdditiveExpr t' in
      (t'', Binop(Sub, a, b))
  | _ -> (t, a)

and parse_MultiplicativeExpr toks =
  let (t, a) = parse_ConcatExpr toks in
  match lookahead t with 
  | Some Tok_Mult ->
      let t' = match_token t Tok_Mult in
      let (t'', b) = parse_MultiplicativeExpr t' in
      (t'', Binop(Mult, a, b))
  | Some Tok_Div ->
      let t' = match_token t Tok_Div in
      let (t'', b) = parse_MultiplicativeExpr t' in
      (t'', Binop(Div, a, b))
  | _ -> (t, a)

and parse_ConcatExpr toks =
  let (t, a) = parse_UnaryExpr toks in 
  match lookahead t with
  | Some Tok_Concat -> let t' = match_token t Tok_Concat in
      let (t'', b) = parse_ConcatExpr t' in
      (t'', Binop(Concat, a, b))
  | _ -> (t, a) 

and parse_UnaryExpr toks =
  match lookahead toks with
  | Some Tok_Not -> let t = match_token toks Tok_Not in 
      let (t', u) = parse_UnaryExpr t in
      (t', Not (u))
  | _ -> parse_FunctionCallExpr toks

and parse_FunctionCallExpr toks = 
  let (t, a) = parse_PrimaryExpr toks in 
  try
    let (t', a') = parse_PrimaryExpr t in 
    (t', FunctionCall(a, a'))  
  with
    InvalidInputException _ -> (t, a) 

and parse_PrimaryExpr toks =
  match lookahead toks with
  | Some Tok_Int i -> (match_token toks (Tok_Int i), Value (Int (i)))
  | Some Tok_Bool b -> (match_token toks (Tok_Bool b), Value (Bool (b)))
  | Some Tok_String s -> let t = match_token toks (Tok_String s) in
      (t, Value (String (s)))
  | Some Tok_ID d -> let t = match_token toks (Tok_ID d) in
      (t, ID (d))
  | Some Tok_LParen -> let t = match_token toks Tok_LParen in 
      let (t', e) = parse_expr t in 
      let t'' = match_token t' Tok_RParen in
      (t'', e)
  | _ -> raise (InvalidInputException "parse_PrimaryExpr")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
  | Some Tok_Def -> let t = match_token toks Tok_Def in parse_DefMutop t
  | _ -> parse_ExprMutop toks

and parse_DefMutop toks = 
  let t, d = match lookahead toks with 
    | Some Tok_ID d -> match_token toks (Tok_ID d), d
    | _ -> raise (InvalidInputException "parse_DefMutop") in
  let t' = match_token t Tok_Equal in
  let (t'', e) = parse_expr t' in
  let t''' = match_token t'' Tok_DoubleSemi in
  (t''', Def(d, e))

and parse_ExprMutop toks = 
  let (t, e) = parse_expr toks in
  let t' = match_token t Tok_DoubleSemi in
  (t', Expr(e))