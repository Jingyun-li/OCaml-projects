open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

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

let get_int e = match e with 
    | Int i -> i
    | Bool _ -> raise (TypeError "Wrong type")
    | String _ -> raise (TypeError "Wrong type")
    | Closure (_,_,_) -> raise (TypeError "Wrong type")

let get_bool e = match e with 
    | Int _ -> raise (TypeError "Wrong type")
    | Bool b -> b
    | String _ -> raise (TypeError "Wrong type")
    | Closure (_,_,_) -> raise (TypeError "Wrong type")

let get_string e = match e with 
    | Int _ -> raise (TypeError "Wrong type")
    | Bool _ -> raise (TypeError "Wrong type")
    | String s -> s
    | Closure (_,_,_) -> raise (TypeError "Wrong type") 
                       
let get_closure e = match e with 
    | Int _ -> raise (TypeError "Wrong type")
    | Bool _ -> raise (TypeError "Wrong type")
    | String _ -> raise (TypeError "Wrong type")
    | Closure (a,b,c) -> (a,b,c)

let rec eval_expr env e = 
    match e with
    | Value v -> v
    | ID x -> lookup env x
    | Not e1 -> 
        let n = get_bool (eval_expr env e1) in
        if n then Bool (false) else Bool (true) 
    | Binop (op, e1, e2) -> (match op with 
        | Greater -> 
            let n1 = get_int (eval_expr env e1) in 
            let n2 = get_int (eval_expr env e2) in 
            let n3 = n1 > n2 in 
            Bool n3
        | GreaterEqual -> 
            let n1 = get_int (eval_expr env e1) in 
            let n2 = get_int (eval_expr env e2) in 
            let n3 = n1 >= n2 in 
            Bool n3
        | Less -> 
            let n1 = get_int (eval_expr env e1) in 
            let n2 = get_int (eval_expr env e2) in 
            let n3 = n1 < n2 in 
            Bool n3
        | LessEqual -> 
            let n1 = get_int (eval_expr env e1) in 
            let n2 = get_int (eval_expr env e2) in 
            let n3 = n1 <= n2 in 
            Bool n3
        | Add -> 
            let n1 = get_int (eval_expr env e1) in 
            let n2 = get_int (eval_expr env e2) in 
            let n3 = n1 + n2 in
            Int n3
        | Sub -> 
            let n1 = get_int (eval_expr env e1) in 
            let n2 = get_int (eval_expr env e2) in 
            let n3 = n1 - n2 in
            Int n3
        | Mult -> 
            let n1 = get_int (eval_expr env e1) in 
            let n2 = get_int (eval_expr env e2) in 
            let n3 = n1 * n2 in
            Int n3
        | Div -> 
            let n1 = get_int (eval_expr env e1) in 
            let n2 = get_int (eval_expr env e2) in 
            if n2 != 0 then
            let n3 = n1 / n2 in
            Int n3 
            else raise (DivByZeroError)
        | Or -> 
            let n1 = get_bool (eval_expr env e1) in 
            let n2 = get_bool (eval_expr env e2) in 
            let n3 = n1 || n2 in
            Bool n3
        | And -> 
            let n1 = get_bool (eval_expr env e1) in 
            let n2 = get_bool (eval_expr env e2) in 
            let n3 = n1 && n2 in
            Bool n3 
        | Concat -> 
            let n1 = get_string (eval_expr env e1) in 
            let n2 = get_string (eval_expr env e2) in 
            let n3 = n1 ^ n2 in
            String n3 
        | Equal -> 
            let n1 = eval_expr env e1 in 
            (match n1 with 
            | Int n -> let n2 = get_int (eval_expr env e2) in
                let n3 = n = n2 in
                Bool n3
            | Bool n -> let n2 = get_bool (eval_expr env e2) in
                let n3 = n = n2 in
                Bool n3
            | String n -> let n2 = get_string (eval_expr env e2) in
                let n3 = n = n2 in
                Bool n3
            |_ -> raise (TypeError "Wrong type"))    
        | NotEqual -> 
            let n1 = eval_expr env e1 in 
            (match n1 with 
            | Int n -> let n2 = get_int (eval_expr env e2) in
                let n3 = n != n2 in
                Bool n3
            | Bool n -> let n2 = get_bool (eval_expr env e2) in
                let n3 = n != n2 in
                Bool n3
            | String n -> let n2 = get_string (eval_expr env e2) in
                let n3 = n != n2 in
                Bool n3
            |_ -> raise (TypeError "Wrong type") ))
    | If (e1, e2, e3) ->
        let b = get_bool (eval_expr env e1) in 
        if b then eval_expr env e2
        else eval_expr env e3
    | Let (x, r, e1, e2) -> 
        if r then
        let env2 = extend_tmp env x in 
        let v1 = eval_expr env2 e1 in 
        (*let (env1, v, e) = get_closure v1 in
        let _ = update env1 x v1 in 
        eval_expr env1 e2*)
        let (env1,v,e) = get_closure v1 in
        update env1 x v1; eval_expr env1 e2
        else
        let v1 = eval_expr env e1 in
        let env' = extend env x v1 in 
        eval_expr env' e2
    | Fun (v, e) -> Closure (env, v, e) 
    | FunctionCall (func, e1) -> 

        let (env', v, e) = get_closure(eval_expr env func) in 
        let v1 = eval_expr env e1 in 
        let env'' = extend env' v v1 in 
        eval_expr env'' e 
        
let eval_mutop env m = 
    match m with
    | Def (var, e) -> 
        let env' = (extend_tmp env var) in
        let v = eval_expr env' e in 
        update env' var v;
        (env', Some v)
    | Expr e ->
        let v = eval_expr env e in
        (env, Some v)
    | NoOp -> (env, None)
