
structure AVL_Set = struct

type bal = int
type key = int
datatype btree = E | N of bal * key * btree * btree

fun member (t, k) = case t of
  E => false |
  N (_, k', left, right) =>
    if k < k' then member (left, k)
    else if k > k' then member (right, k)
    else true


fun balance n = case n of
    (~2, k3, N (~1, k1, a, c), d) =>
      N (0, k1, a, N (0, k3, c, d)) |

    (~2, k3, N (1, k2, a, N (~1, k1, b, c)), d) => 
      N (0, k1, N (0, k2, a, b), N (1, k3, c, d)) |

    (~2, k3, N (1, k2, a, N (0, k1, b, c)), d) => 
      N (0, k1, N (0, k2, a, b), N (0, k3, c, d)) |

    (~2, k3, N (1, k2, a, N (1, k1, b, c)), d) => 
      N (0, k1, N (~1, k2, a, b), N (0, k3, c, d)) |


    (2, k2, a, N (1, k1, b, c)) => 
      N (0, k1, N (0, k2, a, b), c) |

    (2, k2, a, N (~1, k3, N (1, k1, b, c), d)) => 
      N (0, k1, N (~1, k2, a, b), N (0, k3, c, d)) |

    (2, k2, a, N (~1, k3, N (0, k1, b, c), d)) => 
      N (0, k1, N (0, k2, a, b), N (0, k3, c, d)) |

    (2, k2, a, N (~1, k3, N (~1, k1, b, c), d)) => 
      N (0, k1, N (0, k2, a, b), N (1, k3, c, d)) |

    n => N n


fun insert' (t, k) =
  case t of
    E => (N (0, k, E, E), 1) |
    N (b, k', left, right) =>
      if k < k' then 
        let 
          val (new_left, change) = insert' (left, k)
          val new_bal = b - change
          val new_change = if change = 1 andalso new_bal = ~1 then 1 else 0
        in 
          (balance (new_bal, k', new_left, right), new_change)
        end
      else if k > k' then 
        let 
          val (new_right, change) = insert' (right, k)
          val new_bal = b + change
          val new_change = if change = 1 andalso new_bal = 1 then 1 else 0
        in 
          (balance (new_bal, k', left, new_right), new_change)
        end
      else raise (Fail "duplicate key")


val insert = #1 o insert'
fun insert_many (t, ks) = 
  foldl (fn (k, t) => insert (t, k)) t ks


fun height t = case t of
  E => 0 |
  N (_, _, left, right) => 1 + Int.max (height left, height right)


fun balanced t = case t of
  E => true |
  N (_, _, left, right) => let
    val hl = height left
    val hr = height right
  in
    balanced left andalso balanced right andalso (Int.abs (hl - hr)) < 2
  end


structure Q = Queue
fun bfs f t = let
  val q = Q.mkQueue ()
  val _ = Q.enqueue (q, (t, 1))

  fun loop () = 
    if (not (Q.isEmpty q)) then
      let
        val (t, d) = Q.dequeue q
        val _ = case t of
          E => () |
          N (_, _, l, r) =>
            (Q.enqueue (q, (l, d+1)) ; Q.enqueue (q, (r, d+1)))
      in
        (f (t, d)) :: (loop ())
      end
    else []

in
  loop ()
end


fun tree_string t = case t of
  E => " _ " |
  N (_, k, _, _) => " " ^ (Int.toString k) ^ " "



fun add_breaks l = case l of
  [] => ["\n"] |
  [(t, d)] => [tree_string t, "\n"] |
  (t,d)::(t', d')::tds =>
    if (d = d') then
      (tree_string t) :: (add_breaks ((t', d')::tds))
    else 
      (tree_string t) :: "\n" :: (add_breaks ((t', d')::tds))



fun test l = let
  val t = insert_many (E, l)
  val tp_list = bfs (fn tp => tp) t
  val _ = List.app print (add_breaks tp_list) 
in
  balanced t
end

end
