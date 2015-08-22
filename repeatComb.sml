
fun numList (start, size) = 
  List.tabulate(size, fn i => i + start)

fun tails lst = case lst
  of x :: xs =>
    lst :: (tails xs)
  | nil => []

structure A = struct

  fun subsets (set, size) =  
    if size = 1
    then map (fn i => [i]) set
    else List.concat (List.map (fn x :: xs' =>
      List.map (fn s => x :: s) (subsets (xs', size - 1))
    ) (List.filter (fn xs => (length xs) >= size) (tails set)))

end

structure B = struct

  fun subsets (set, size) = let

    fun loop tks = case tks
      of (nil, k) :: nil => k []
      | (nil, k1) :: (ts, k2) :: tks' => 
        let
          val prev = (ts, fn acc => k2 ((k1 []) @ acc))
        in 
          loop (prev :: tks')
        end
      | ((opt, s, n) :: ts, k) :: tks' => 
        let 
          val curr = (ts, k)

          val (nextTs, stageK)  = 
            if n = 1
            then ([], fn acc => map (fn x => [x]) s)
            else (
              (List.map (fn x :: xs' =>
                (SOME x, xs', n - 1)
              ) (List.filter (fn xs => (length xs) >= n) (tails s))),
              fn acc => acc
            )

          val next = (
            nextTs, 
            fn acc => case opt
              of SOME prefix => map (fn s => prefix :: s) (stageK acc)
               | NONE => (stageK acc)
          )

        in
          loop (next :: curr :: tks')
        end
  in
    loop [([(NONE, set, size)], fn acc => acc)] 
  end

end


(*

12345 C 1
    1
    2
    3
    4
    5

[[12345]] fn acc => acc

[], fn acc => acc
[], fn acc => [[1], [2], [3], [4], [5]]


*****

3: [[12345]] fn acc => acc


3: [], fn acc => acc
2: [[12345], [2345], [345]], fn acc => acc

3: [], fn acc => acc
2: [[2345], [345]], fn acc => acc
1: [[12345], 



12345 C 3             
    1 > 2345 C 2
        2 > 345 C 1
            3            123
            4            124
            5            125
        3 > 45 C 1
            4            134
            5            135
        4 > 5 C 1
            5            145
    2 > 345 C 2
        3 > 45 C 1
            4            234
            5            235
        4 > 5 C 1
            5            245
    3 > 45 C 2
        4 > 5 C 1
            5            345

*)
