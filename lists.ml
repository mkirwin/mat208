fun len1(xs) = if xs = nil then 0 else 1 + len1(tl(xs))

fun len(nil) = 0
  | len(x::xs) = 1 + len(xs)

fun sumProduct(nil) = (0,1)
  | sumProduct(x::xs) = let val (s,p) = sumProduct(xs) in (x+s,x*p) end  

fun listMap(f,nil) = nil
  | listMap(f,x::xs) = f(x)::listMap(f,xs)


val emptySet = nil

fun isElem(_,nil) = false
  | isElem(x,y::ys) = if x = y then true else isElem(x,ys)

fun setAddElem(x,ys) = if isElem(x,ys) then ys else x::ys

fun setUnion(nil,ys) = ys
  | setUnion(x::xs,ys) = setAddElem(x,setUnion(xs,ys))

fun setFilter(nil,g) = emptySet
  | setFilter(x::xs,g) = if g(x) then x::setFilter(xs,g) else setFilter(xs,g)
