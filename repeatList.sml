(* datatype 'a list = :: of 'a * 'a list | nil *)

val test = ["A", "B", "C", "D", "E", "F"]
(* val test = "A" :: "B" :: "C" :: "D" :: "E" :: "F" :: nil *)
(* val test = ("A" :: ("B" :: ("C" :: ("D" :: ("E" :: ("F" :: nil)))))) *)
(* val it = "(A (B (C (D (E F)))))" : string *)


fun strcat (a, b) = 
  if b = ""
  then a 
  else "(" ^ a ^ " " ^  b ^ ")"

structure A = struct
  fun strcatList l = case l
    of nil => "" 
    | x :: xs => strcat (x, (strcatList xs))
end

structure AA = struct

  (* a list contains initial state, a final state, and incrementation 
  * information in a single object. Thus, it is possible to define a 
  * concise abstraction called "fold right"*)

  fun foldr f base l = case l 
    of nil => base 
    | x :: xs => f (x, (foldr f base xs))

  val strcatList = foldr strcat ""
end


structure B = struct
  fun strcatList l = let
    fun loop (l, k) = case l
      of nil => k "" 
      | x :: xs => loop (xs, fn acc => k (strcat (x, acc)))
  in
    loop (l, fn acc => acc)
  end
end

structure C = struct
  fun strcatList l = let

    fun mkStack (l, stack) = case l
      of nil => stack 
      | x :: xs => mkStack (xs, (fn acc => strcat (x, acc)) :: stack)

    fun evalStack (stack, acc) = case stack 
      of nil => acc 
      | k :: ks => evalStack (ks, k acc)

  in
    evalStack (mkStack (l, nil), "")
  end
end

structure D = struct
  fun strcatList l = let

    fun mkStack (l, stack) = case l
      of nil => stack 
      | x :: xs => mkStack (xs, x :: stack)

    fun evalStack (stack, acc) = case stack 
      of nil => acc 
      | x :: xs => evalStack (xs, strcat (x, acc))

  in
    evalStack (mkStack (l, nil), "")
  end
end

structure DD = struct

  fun rev l = let
    fun loop (l, rs) = case l
      of nil => rs 
      | x :: xs => loop (xs, x :: rs)
  in
    loop (l, nil)
  end

  fun foldl f acc l = case l 
      of nil => acc 
      | x :: xs => foldl f (f (x, acc)) xs

  fun strcatList l = foldl strcat "" (rev l)
end
