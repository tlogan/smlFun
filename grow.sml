fun grow (result, seed, years) = 
  if years = 0 then 
    result 
  else 
    grow ((result * 1.5) + seed, seed, years - 1) 
