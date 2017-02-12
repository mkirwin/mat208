fun listMap(f,nil) = nil
    | listMap(f,x::xs) = f(x) :: listMap(f,xs);

fun powerSet(nil) = [nil]
    | powerSet(x::xs) = 
        let val p = powerSet(xs) 
        in p @ listMap(fn y => x::y, p) end;
 
(*----------------------------------------------------------------------*)

datatype suit = Heart|Spade|Club|Diamond;
datatype money = USE of real|CND of real;

datatype intOption = NONE | SOME of int;

fun inPosition(nil,k) = NONE
    | inPosition(x::xs,0) = SOME(x)
    | inPosition(x::xs,k) = inPosition(xs,k-1);

fun f(NONE) = 0
    |f(SOME x) = x;

(* Don't use this lol *)
datatype natural = Zero | Successor of natural;
datatype natural = Successor of natural | Zero;

fun isEqual(Zero,Zero) = true
    | isEqual (Zero,Successor(n)) = false
    | isEqual (Successor(m), Zero) = false
    | isEqual (Successor(m), Successor(n)) = isEqual(m,n);

fun plus(m,Zero) = m
    | plus(m, Successor(n)) = Successor(plus(m,n))


datatype intList = Empty | Cons of int * intList;

fun append(Empty,ys) = ys
    | append(Cons(x,xs),ys) = Cons(x,append(xs,ys));

Control.Print.printDepth := 1000; (* How to see the depth *)

type 'a set = 'a list
val emptySet = nil : 'a set

fun powerSet(nil : 'a set) : 'a set set = [nil] (* Specifies an empty alpha set*)
    | powerSet(x::xs) = let val p = powerSet(xs) 
        in p @ listMap(fn y => x::y, p) end
