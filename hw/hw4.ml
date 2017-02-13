(*datatype intOption = NONE | SOME of int;*)

type 'a set = 'a list
val emptySet = nil : 'a set

fun optionMax(NONE,NONE) = NONE
    | optionMax(NONE,SOME(b)) = SOME(b)
    | optionMax(SOME(a), NONE) = SOME(a)
    | optionMax(SOME(a), SOME(b)) = if a > b then SOME(a) else SOME(b);

(*setAddElem takes the set you're adding to and the element to add to the set*)    
fun isElem(_, nil) = false 
    | isElem(x,y::ys) = if x = y then true else isElem(x,ys);

fun setAddElem(x,nil) = x :: nil
    | setAddElem(x,y::ys) = if isElem(x,y::ys) then y::ys
                            else if (x<y) then x::y::ys 
                            else y::setAddElem(x,ys);

fun setUnion(nil,nil) = nil
    | setUnion(x::xs,nil) = x::xs
    | setUnion(nil, y::ys) = y::ys
    | setUnion(x::xs,y::ys) = if x = y then x::setUnion(xs,ys) 
                            else if x < y then x::setUnion(xs,y::ys)
                            else y::setUnion(x::xs,ys); 
(* Repeat problems *)
fun setIntersection(nil,_) = nil
    | setIntersection(_,nil) = nil
    (*| setIntersection(nil,y::ys) = nil*)
    | setIntersection(x::xs,y::ys) = if x=y then x::setIntersection(xs,ys)
        else if x<y then setIntersection(xs,y::ys)
        else setIntersection(x::xs,ys);
                                                     
fun setMax(nil : int list) = NONE (* instead of throwing exception *)
    | setMax(x::xs) =  SOME(List.last(x::xs)); 
    

datatype natural = Zero | Successor of natural;
(*
val two = Successor(Successor(Zero))
val three = Successor(two);
*)

(*
fun toInt(Zero) = 0
    toInt(Successor(n)) = 1 + toInt(n);
*)
fun isEven(Zero) = true
    | isEven(Successor(n)) = if (isEven(n)) then false else true;

fun plus(m,Zero) = m
    | plus(m,Successor(n)) = Successor(plus(m,n));

fun times(m,Zero) = Zero
    | times(m,Successor(n)) = plus(m,times(m,n));
