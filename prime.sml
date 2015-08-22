structure PrimeIterator = struct

  fun iterateNatFromTwo () =
    let val natRef = ref (fn () => 2)
    in fn () =>
      let 
        val result = (!natRef) ()
      in 
        natRef := (fn () => result + 1);
        result 
      end
    end

  fun filter (iter, filt) =
    let
      fun loop () =
        let 
          val nat = iter () 
        in
          if (nat mod filt) = 0
          then loop ()
          else nat
        end
    in loop end


  fun mk () =
    let 
      val itRef = ref (iterateNatFromTwo ())
    in fn () =>
      let 
        val result = (!itRef) ()
      in
        itRef := filter (!itRef, result);
        result
      end
    end


  fun toList (iter, num) = 
    if num < 1
    then []
    else iter () :: toList (iter, num - 1)

end

datatype streamCell = Cons of int * stream 
withtype stream = unit -> streamCell

structure PrimeStream = struct

  val natStreamFromTwo =
    let
      fun loop x () = Cons (x, loop(x + 1))
    in loop 2 end

  fun filter (xsf, filt) () = 
    case xsf () of Cons (nat, xsf') =>
      if (nat mod filt) = 0
      then filter (xsf', filt) ()
      else Cons (nat, filter (xsf', filt))
          
  val mk = 
    let 
      fun loop xsf () =
        case xsf() of Cons (x, xsf') => 
          Cons(x, loop (filter (xsf', x))) 
    in
      loop (natStreamFromTwo)
    end

  fun toList (xsf, num) = 
    if num < 1
    then [] 
    else case xsf () of Cons (x, xsf') =>
      x :: toList (xsf', num - 1)

end
