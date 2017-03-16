(* Homework 9 *)

type 'a set = 'a list 

val emptySet = nil : 'a set

val R = [(1,1),(2,2),(2,3),(2,5),(3,2),(3,3),(3,5),(4,4),(5,2),(5,3),(5,5)];

fun len(nil) = 0
  |len(x::xs) = 1 + len(xs);
  
fun isElem(_ : ''a,nil : ''a set) = false
  | isElem(x,y::ys) = if x = y then true else isElem(x,ys);

fun setAddElem(x : ''a,ys : ''a set) = if isElem(x,ys) then ys else x::ys  

fun setUnion(nil : ''a set,ys : ''a set) = ys
  | setUnion(x::xs,ys) = setAddElem(x,setUnion(xs,ys))

fun setIntersection(nil : ''a set,ys : ''a set) = emptySet
  | setIntersection(x::xs,ys) = if isElem(x,ys) then x::setIntersection(xs,ys) else setIntersection(xs,ys)
      
fun image(a: ''a ,nil:(''a*'b)list) = nil : 'b list
    | image(a,(x,y)::rs) = if a=x then y::image(a,rs) else image(a,rs);

fun obtainSet(nil) = nil
    | obtainSet((a,b)::rest) = setUnion([a], obtainSet(rest));

fun thinHelp(xs, nil) = xs
    | thinHelp(xs,y::ys) = if setIntersection(xs,ys) = nil then setAddElem(y,xs)
    else thinHelp(xs,ys);

fun thinSet(R, nil) = nil
    | thinSet(R, a::Y) = thinHelp(thinSet(R,Y),setIntersection(image(a,R),a::Y));

fun uniqueReps(R) = thinSet(R, obtainSet(R));

fun helper(nil,R) = nil
    | helper(x::xs, R) = let val m = image(x,R)
    in if isElem(m,helper(xs,R)) then helper(xs,R)
    else m::helper(xs,R) end;

fun equivClasses(R) = helper(obtainSet(R),R);
