let rec gcd n m = 
  if m = 0 
    then n
  else
    if n < m 
      then gcd m n
    else
      gcd (n-m) m
;;
