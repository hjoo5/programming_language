type exp = X | INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp


let rec sub_cal exp env =
  match exp with
   | INT n -> (float_of_int n)
   | REAL n -> n
   | X -> sub_cal (List.assoc X env) env
   | ADD (ex1,ex2) -> (sub_cal ex1 env) +. (sub_cal ex2 env) 
   | SUB (ex1,ex2) -> (sub_cal ex1 env) -. (sub_cal ex2 env) 
   | MUL (ex1,ex2) -> (sub_cal ex1 env) *. (sub_cal ex2 env) 
   | DIV (ex1,ex2) -> (sub_cal ex1 env) /. (sub_cal ex2 env) 
   | SIGMA(ex1,ex2,ex3) -> 
         if (sub_cal ex1 [(X,ex1)]) > (sub_cal ex2 [(X,ex1)]) 
           then 0.0
         else
           if  (sub_cal ex1 [(X,ex1)] = (sub_cal ex2 [(X,ex1)]))
             then sub_cal ex3 [(X,ex1)]
             else (sub_cal ex3 [(X,ex1)]) +. (sub_cal (SIGMA (ADD (ex1,INT 1),ex2, ex3)) [(X,ex1)])

let calculate : exp -> float =
    fun exp  ->
        sub_cal exp []
          

            
            
           
and value_change exp1 exp2 = 


        calculate (SUB ((INT 0),(REAL 4.)));;

        calculate (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)))
        let a = (3,4) let x =4 in x+3;;
        let env = [(X,INT 0)]
        let x = 3  match x with | 0 -> 1 | 1 ->2| |_ -> 10;;
        let z= 4;;
        if z = 4 then let y = 4 in z+y else 4;;
        let env =[(X,INT 3)] in calculate (SUB(MUL(X, X), INT 1))
        calculate (SIGMA(INT 1, INT 10, SUB (MUL (X, X), INT 1)))