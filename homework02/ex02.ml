type ae = 
    | CONST of int
    | VAR of string
    | POWER of string * int
    | TIMES of ae list
    | SUM of ae list



let rec diff : ae * string -> ae
  = fun (ae, var) ->
    match ae with
    | CONST n -> CONST 0
    | VAR x -> if x = var 
        then CONST 1 
        else CONST 0
    | POWER (v, i) -> 
        if v = var 
            then TIMES[CONST i;POWER (v, i - 1)]
            else CONST 0
    | TIMES l ->
            (match l with
            | [] -> CONST 0
            | hd::tl -> mul_diff l var
            )
    
    | SUM l -> 
       ( match l with
        | [] -> SUM []
        | hd::tl -> SUM ([diff(hd,var)]@sum_diff tl var) )
and sum_diff l var  = 
    (match l with
        | [] -> []
        | hd::tl -> diff(hd,var)::sum_diff tl var)
and mul_diff l var =
    match l with
    | [] ->  CONST 0
    | hd::tl ->  SUM ( [(TIMES ([diff(hd,var)]@tl));(TIMES ([hd]@[(mul_diff tl var)]) )])
    ;;

       
  
