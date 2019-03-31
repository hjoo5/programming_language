let rec zipper listA listB = 
  match listB with
  | [] -> listA
  | bhd::btl ->
        match listA with 
                | [] -> listB
                | ahd::atl ->  ahd::bhd::(zipper atl btl)
;;