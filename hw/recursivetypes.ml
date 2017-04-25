Control.Print.printLength := 10000;
Control.Print.printDepth := 100;


(*datatype intOption = NONE | SOME of int*)

fun inPosition(nil,_) = NONE
  | inPosition(x::xs,0) = SOME(x)
  | inPosition(x::xs,k) = inPosition(xs,k-1)






datatype natural = Zero | Successor of natural;

val two = Successor(Successor(Zero))
val three = Successor(Successor(Successor(Zero)))

fun toInt(Zero) = 0
  | toInt(Successor(n)) = 1 + toInt(n)

fun fromInt(0) = Zero
  | fromInt(n) = Successor(fromInt(n-1))

fun pred(Zero) = Zero
  | pred(Successor(n)) = n

fun isEqual(Zero,Zero) = true
  | isEqual(Zero,n) = false
  | isEqual(n,Zero) = false
  | isEqual(Successor(m),Successor(n)) = isEqual(m,n)

fun isLessThanOrEqual(Zero,n) = true
  | isLessThanOrEqual(n,Zero) = false
  | isLessThanOrEqual(Successor(m),Successor(n)) = isLessThanOrEqual(m,n)

fun isLessThan(n,Zero) = false
  | isLessThan(Zero,n) = true
  | isLessThan(Successor(m),Successor(n)) = isLessThan(m,n)

fun plus(n,Zero) = n
  | plus(n,Successor(m)) = Successor(plus(n,m))






datatype intList = Empty | Cons of int * intList

val bs = Cons(9,Empty)

val cs = Cons(7,Cons(1,Cons(3,Empty)))

val ds = Cons(5,Cons(2,Cons(1,Cons(2,Empty))))

fun append(Empty,ys) = ys
  | append(Cons(x,xs),ys) = Cons(x,append(xs,ys))

fun take(xs,0) = Empty
  | take(Empty,k) = Empty
  | take(Cons(x,xs),k) = Cons(x,take(xs,k-1))









