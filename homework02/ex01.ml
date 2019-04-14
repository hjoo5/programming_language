type exp = X | INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp


let rec sub_cal exp env =
  match exp with
   | INT n -> (float_of_int n)
   | REAL n -> n
   | X -> sub_cal (List.assoc X env) env
   | ADD (ex1,ex2) -> (sub_cal ex1 env) +. (sub_cal ex2 env) 
   | SUB (ex1,ex2) -> (sub_cal ex1 env) -. (sub_cal ex2 env) 
   | MUL (ex1,ex2) -> (sub_cal ex1 env) *. (sub_cal ex2 env) 
   | DIV (ex1,ex2) -> (sub_cal ex1 env) /. (sub_cal ex2 env) 
   | SIGMA (ex1,ex2,ex3) ->  
      if (sub_cal ex1 [(X,ex1)]) > (sub_cal ex2 [(X,ex1)]) 
        then 0.0
      else
        if  (sub_cal ex1 [(X,ex1)] = (sub_cal ex2 [(X,ex1)]))
          then sub_cal ex3 [(X,ex1)]
          else (sub_cal ex3 [(X,ex1)]) +. (sub_cal (SIGMA (ADD (ex1,INT 1),ex2, ex3)) [(X,ex1)]) 
  | INTEGRAL (ex1,ex2,ex3) -> 
      if (sub_cal ex1 [(X,ex1)]) > (sub_cal (SUB (ex2,REAL 0.1)) [(X,ex1)]) 
        then 0.0
      else
        if  (sub_cal ex1 [(X,ex1)] = (sub_cal (SUB (ex2,REAL 0.1)) [(X,ex1)]))
          then sub_cal (MUL(ex3,REAL 0.1)) [(X,ex1)]
          else sub_cal (MUL(ex3,REAL 0.1)) [(X,ex1)] +. (sub_cal (INTEGRAL (ADD (ex1,REAL 0.1),ex2, ex3)) [(X,ex1)]) 

    

let calculate : exp -> float =
    fun exp  ->
        sub_cal exp []
