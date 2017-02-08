fun len1(xs) = if xs = nil then 0 else 1 + len1(tl(xs))

fun len(nil) = 0
  | len(x::xs) = 1 + len(xs)

fun sumProduct(nil) = (0,1)
  | sumProduct(x::xs) = let val (s,p) = sumProduct(xs) in (x+s,x*p) end  

(*Maybe also called listMap*)
fun myMap(f,nil) = nil
  | myMap(f,x::xs) = f(x)::myMap(f,xs)


val emptySet = nil

(*Cute little notation, "_"*)
fun isElem(_,nil) = false
  | isElem(x,y::ys) = if x = y then true else isElem(x,ys)

fun setAddElem(x,ys) = if isElem(x,ys) then ys else x::ys

fun setUnion(nil,ys) = ys
  | setUnion(x::xs,ys) = setAddElem(x,setUnion(xs,ys))

fun setFilter(nil,g) = emptySet
  | setFilter(x::xs,g) = if g(x) then x::setFilter(xs,g) else setFilter(xs,g)
  

fun powerSet(nil) =[nil] (*Maybe need another box? questionmark*)
    | powerSet(x::xs) = (* listMap(  , powerSet(xs))*) @ powerSet(xs);
