type formula = TRUE | FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr




let rec eval : formula -> bool =
  fun e ->match e with
| TRUE -> true
| FALSE -> false
| NOT e1 -> 
  if e1 = TRUE then false else true
| ANDALSO (e1,e2) -> ((eval e1) && (eval e2))
| ORELSE (e1,e2) -> ((eval e1) || (eval e2))
| IMPLY (e1,e2) -> 
  if e1=TRUE then
    if e2=TRUE then true else false
  else 
    true
| LESS (e1,e2) -> ((eval_num e1) < (eval_num e2))
and eval_num : expr -> int =
  fun x -> match x with
  | NUM x -> x
  | PLUS (x1,x2) -> ((eval_num x1) + (eval_num x2))
  | MINUS (x1,x2) -> ((eval_num x1) - (eval_num x2))

;;


