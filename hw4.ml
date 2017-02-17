fun optionMax(NONE,b) = b
  | optionMax(a,NONE) = a
  | optionMax(SOME(x),SOME(y)) = if x > y then SOME(x) else SOME(y)


type 'a set = 'a list 

fun setAddElem(x : int,nil : int set) : int set = [x]
  | setAddElem(x,y::ys) = if x < y then x::y::ys else if x = y then y::ys else y::setAddElem(x,ys)

fun setUnion(nil : int set,ys : int set) : int set = ys
  | setUnion(xs,nil) = xs
  | setUnion(x::xs,y::ys) = if x < y then x::setUnion(xs,y::ys) else if x = y then x::setUnion(xs,ys) else y::setUnion(x::xs,ys)

fun setIntersection(nil : int set,ys : int set) : int set = nil
  | setIntersection(xs,nil) = nil
  | setIntersection(x::xs,y::ys) = if x < y then setIntersection(xs,y::ys) else if x = y then x::setIntersection(xs,ys) else setIntersection(x::xs,ys)


exception MaxOfEmptySet

fun setMax(nil : int set) = raise MaxOfEmptySet
  | setMax(x::nil) = x
  | setMax(x::xs) = setMax(xs)




datatype natural = Zero | Successor of natural

fun isEven(Zero) = true
  | isEven(Successor(Zero)) = false
  | isEven(Successor(Successor(n))) = isEven(n)

fun isEven2(Zero) = true
  | isEven2(Successor(n)) = not(isEven2(n))



fun plus(n,Zero) = n
  | plus(n,Successor(m)) = Successor(plus(n,m))

fun times(n,Zero) = Zero
  | times(n,Successor(m)) = plus(times(n,m),n)




