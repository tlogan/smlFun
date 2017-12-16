datatype node = Nil | Cons of int * node ref 


fun remove (headRef: node ref, rem: node) = 
  let 
    fun loop (indirect) =
      if (!indirect <> Nil andalso !indirect <> rem)
      then loop(
        case (!indirect)
          of Cons (_, next) => next 
           | Nil => ref Nil
      )
      else (indirect := (
        case rem
          of Cons (_, next) => !next 
           | Nil => Nil
      ); ())
  in
    loop(headRef)
  end
