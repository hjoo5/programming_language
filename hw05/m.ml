type program = exp
and exp = 
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

exception TypeError

type typ = TyInt | TyBool 
  | TyFun of typ * typ (* t1 -> t2 *) 
  | TyVar of tyvar
and tyvar = string
type typ_eqn = (typ * typ) list (* t1 = t2 *)

let rec string_of_type ty = 
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x

let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_string (string_of_type ty1 ^ " = " ^ string_of_type ty2 ^ "\n")) eqns;
  print_endline ""

(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TyInt -> TyInt
    | TyBool -> TyBool 
    | TyFun (t1,t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyVar x -> 
      try find x subst
      with _ -> typ

  (* add a binding (tv,ty) to the substitution and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  =fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ string_of_type ty)) subst
end

let tyvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn 
=fun tenv e ty ->
  match e with
  | TRUE -> [(ty, TyBool)]
  | FALSE -> [(ty, TyBool)]
  | CONST n -> [(ty, TyInt)]
  | VAR v -> let varV = (TEnv.find tenv v) in [(ty, varV)]
  | ADD (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO e -> [(ty, TyBool)] @ (gen_equations tenv e TyInt)
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) ->
    (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (v, e1, e2) ->
    let new_tyvar = fresh_tyvar () in
    let new_tenv = TEnv.extend (v, new_tyvar) tenv in
      (gen_equations tenv e1 new_tyvar) @ (gen_equations new_tenv e2 ty)
  | LETREC (v_f, v_x, e1, e2) ->
    let new_tyvar1 = fresh_tyvar () in
    let new_tyvar2 = fresh_tyvar () in
    let new_tenv = (TEnv.extend (v_f, TyFun (new_tyvar2, new_tyvar1)) tenv) in
    let new_tenv' = (TEnv.extend (v_x, new_tyvar2) new_tenv) in
      (gen_equations new_tenv' e1 new_tyvar1) @ (gen_equations new_tenv e2 ty)
  | PROC (v, e) ->
    let new_tyvar1 = fresh_tyvar () in
    let new_tyvar2 = fresh_tyvar () in
    let new_tenv = (TEnv.extend (v, new_tyvar1) tenv) in
      [(ty, TyFun (new_tyvar1, new_tyvar2))] @ (gen_equations new_tenv e new_tyvar2)
  | CALL (e1, e2) ->
    let new_tyvar = fresh_tyvar () in
      (gen_equations tenv e1 (TyFun (new_tyvar, ty))) @ (gen_equations tenv e2 new_tyvar)


let solve : typ_eqn -> Subst.t
=fun eqns -> unifyall eqns [] 

(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations TEnv.empty exp new_tv in
  let _ = print_endline "= Equations = ";
          print_typ_eqns eqns in
  try 
    let subst = solve eqns in
    let ty = Subst.apply new_tv subst in
     print_endline "= Substitution = ";
      Subst.print subst;
      print_endline "";
      print_endline ("Type of the given program: " ^ string_of_type ty);
      print_endline "";
      ty
  with TypeError -> (print_endline "The program does not have type. Rejected."); exit (1)
