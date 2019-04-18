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
            | [] ->CONST 1
            | hd::tl ->
                (match hd with
                | CONST n -> TIMES [(CONST n);list_diff tl var]
                | _ -> TIMES [diff(hd,var);list_diff tl var ]
                )
            )
    
    | SUM l -> 
       ( match l with
        | [] -> SUM []
        | hd::tl -> SUM ([diff(hd,var)]@list_diff tl var) )
and list_diff l var  = 
    match l with
        | [] -> []
        | hd::tl -> diff(hd,var)::list_diff tl var
       
  
diff ( SUM[POWER ("x", 2);POWER ("x", 1)],"x")