
structure A = struct
  fun fact n = 
    if (n < 1)
    then 1 
    else n * (fact (n - 1))
end

structure B = struct
  fun fact n = let
    fun loop (n', k) =
      if (n' < 1)   
      then k 1
      else loop (n' - 1, fn acc => k (n' * acc)) 
  in
    loop (n, fn acc => acc)
  end
end

structure C = struct
  fun fact n = let
    fun loop (n', stack) =
      if n' < 1
      then foldl (fn (k, acc) => (k acc)) 1 stack 
      else loop (n' - 1, (fn acc => n' * acc) :: stack)
  in 
    loop (n, nil)
  end
end

structure D = struct
  fun fact n = let
    fun loop (n', ns) = 
      if n' < 1
      then foldl (fn (n', acc) => n' * acc) 1 ns 
      else loop (n' - 1, n' :: ns)
  in 
    loop (n, nil)
  end
end

structure E = struct
  fun fact n = let
    fun loop (i, acc) = 
      if (i > n)
      then acc 
      else loop (i + 1, i * acc)
  in
    loop (1, 1)
  end
end

