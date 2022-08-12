open TokenTypes
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let re_rparen = Str.regexp ")" 
let re_lparen = Str.regexp "("
let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greatereql = Str.regexp ">="
let re_lesseql = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not"
let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"
let re_add = Str.regexp "\\+"
let re_sub = Str.regexp "\\-"
let re_mult = Str.regexp "\\*"
let re_div = Str.regexp "\\/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "let"
let re_rec = Str.regexp "rec"
let re_in = Str.regexp "in"
let re_def = Str.regexp "def"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"
let re_posint = Str.regexp "[0-9]+"
let re_negint = Str.regexp "(-[0-9]+)"
let re_true = Str.regexp "true"
let re_false = Str.regexp "false"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_dbsemi = Str.regexp ";;"
let re_empty = Str.regexp " "

let tokenize input =
  let rec tok pos s =
    if pos >= String.length s then
      []

    else if (Str.string_match re_true s pos) then
      let token = Str.matched_string s in 
      (Tok_Bool true)::(tok (pos+(String.length token)) s)

    else if (Str.string_match re_false s pos) then
      let token = Str.matched_string s in 
      (Tok_Bool false)::(tok (pos+(String.length token)) s)
    
    else if (Str.string_match re_string s pos) then
      let token = Str.matched_string s in 
      (Tok_String (String.sub token 1 ((String.length token)-2)))::(tok (pos+(String.length token)) s)

    else if (Str.string_match re_negint s pos) then
      let token = Str.matched_string s in 
      (Tok_Int (int_of_string (String.sub token 1 ((String.length token)-2))))::
      (tok (pos+(String.length token)) s)

    else if (Str.string_match re_rparen s pos) then
      Tok_RParen::(tok (pos+1) s)

    else if (Str.string_match re_lparen s pos) then
      Tok_LParen::(tok (pos+1) s)

    else if (Str.string_match re_arrow s pos) then
      Tok_Arrow::(tok (pos+2) s)

    else if (Str.string_match re_equal s pos) then
      Tok_Equal::(tok (pos+1) s)

    else if (Str.string_match re_notequal s pos) then
      Tok_NotEqual::(tok (pos+2) s)

    else if (Str.string_match re_greatereql s pos) then
      Tok_GreaterEqual::(tok (pos+2) s)

    else if (Str.string_match re_lesseql s pos) then
      Tok_LessEqual::(tok (pos+2) s)

    else if (Str.string_match re_greater s pos) then
      Tok_Greater::(tok (pos+1) s)
    
    else if (Str.string_match re_less s pos) then
      Tok_Less::(tok (pos+1) s)
    
    else if (Str.string_match re_or s pos) then
      Tok_Or::(tok (pos+2) s)
    
    else if (Str.string_match re_and s pos) then
      Tok_And::(tok (pos+2) s)
    
    else if (Str.string_match re_not s pos) then
      Tok_Not::(tok (pos+3) s)
    
    else if (Str.string_match re_if s pos) then
      Tok_If::(tok (pos+2) s)
    
    else if (Str.string_match re_then s pos) then
      Tok_Then::(tok (pos+4) s)
    
    else if (Str.string_match re_else s pos) then
      Tok_Else::(tok (pos+4) s)
    
    else if (Str.string_match re_add s pos) then
      Tok_Add::(tok (pos+1) s)
    
    else if (Str.string_match re_sub s pos) then
      Tok_Sub::(tok (pos+1) s)
    
    else if (Str.string_match re_mult s pos) then
      Tok_Mult::(tok (pos+1) s)
    
    else if (Str.string_match re_div s pos) then
      Tok_Div::(tok (pos+1) s)
    
    else if (Str.string_match re_concat s pos) then
      Tok_Concat::(tok (pos+1) s)
    
    else if (Str.string_match re_let s pos) then
      Tok_Let::(tok (pos+3) s)

    else if (Str.string_match re_rec s pos) then
      Tok_Rec::(tok (pos+3) s)

    else if (Str.string_match re_in s pos) then
      Tok_In::(tok (pos+2) s)

    else if (Str.string_match re_def s pos) then
      Tok_Def::(tok (pos+3) s)

    else if (Str.string_match re_fun s pos) then
      Tok_Fun::(tok (pos+3) s)

    else if (Str.string_match re_dbsemi s pos) then
      Tok_DoubleSemi::(tok (pos+2) s)
    
    else if (Str.string_match re_empty s pos) then
      tok (pos+1) s

    else if (Str.string_match re_id s pos) then
      let token = Str.matched_string s in 
      (Tok_ID token)::(tok (pos+(String.length token)) s)

    else if (Str.string_match re_posint s pos) then
      let token = Str.matched_string s in 
      (Tok_Int (int_of_string token))::(tok (pos+(String.length token)) s)

    else raise (InvalidInputException "tokenize")
  
  in tok 0 input