signature DEQUE = sig

  type 'a queue

  val empty : 'a queue
  val isEmpty : 'a queue -> bool

  val cons : 'a queue * 'a -> 'a queue
  val head : 'a queue -> 'a option
  val tail : 'a queue -> 'a queue

  val snoc : 'a queue * 'a -> 'a queue
  val last : 'a queue -> 'a option
  val init : 'a queue -> 'a queue


  val toList : 'a queue -> 'a list

end

structure Deque :> DEQUE = struct

  type 'a queue = 'a list * 'a list


  val empty = ([], [])

  fun isEmpty q = case q of 
    ([], []) => true |
    _ => false


  fun cons (q, x) = case q of
    (front, rear) => (x :: front, rear)


  fun head q = case q of 
    ([], []) => NONE |
    ([], rear) => SOME (hd (rev rear)) |
    (x :: xs, _) => SOME x


  fun tail q = case q of 
    ([], []) => ([], []) |
    ([], rear) => (tl (rev rear), []) |
    (x :: xs, rear) => (xs, rear)



  fun snoc (q, x) = case q of
    (front, rear) => (front, x :: rear)


  fun last q = case q of
    ([], []) => NONE |
    (front, []) => SOME (hd (rev front)) |
    (_, x :: xs) => SOME x

  fun init q = case q of
    ([], []) => ([], []) |
    (front, []) => ([], tl (rev front)) |
    (front, x :: xs) => (front, xs) 

  
  fun toList (front, rear) = front @ (rev rear)
  

end 

