datatype 'a tree = T of 'a * ('a tree list) 

val t = 
  T("A", [
    T("B", [
      T("C", [
        T("D", nil)
      ]),
      T("E", [
        T("F", nil), 
        T("G", nil)
      ])
    ]), 

    T("H", [
      T("I", [
        T("J", nil), 
        T("K", nil),
        T("L", nil)
      ]), 
      T("M", nil),
      T("N", nil)
    ])
  ]);

(* val it = "(A ((B ((C D) (E (F G)))) (H ((I (J (K L))) (M N)))))" : string *)

fun strcat (a, b) = 
  if b = ""
  then a 
  else "(" ^ a ^ " " ^  b ^ ")"


structure A = struct
  fun strcatTree (T (str, treeList)) = 
  if length(treeList) = 0
  then str
  else let
    val strList = map strcatTree treeList 
  in
    strcat (str, (foldr (fn (s, acc) => strcat(s, acc)) "" strList))
  end
end


structure B = struct
  fun strcatTree t = let
    fun loop (tks) = case tks
      of (nil, k) :: nil => k ""
      | (nil, k1) :: (ts, k2) :: tks' => 
        let
          val prevPair = (ts, fn acc => k2(strcat(k1 "", acc)))
        in 
          loop (prevPair :: tks')
        end
      | (T (str, treeList) ::ts, k) :: tks' => 
        let
          val currPair = (ts, k)
          val nextPair = 
            if length(treeList) = 0
            then ([], fn acc => str)
            else (treeList, fn acc => strcat (str, acc))
        in 
          loop (nextPair :: currPair :: tks')
        end
      | nil => ""
  in
    loop [([t], fn acc => acc)]
  end
end
