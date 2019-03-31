let rec fold3 f a b c d = 
  match b with 
  | []->a
  | bhd::btl ->
    match c with
    | [] ->a
    | chd::ctl -> 
      match d with
      | [] ->a
      | dhd::dtl -> fold3 f (f a bhd chd dhd) btl ctl dtl
;;