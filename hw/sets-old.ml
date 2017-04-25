type 'a set = 'a list 

val emptySet = nil : 'a set

fun isElem(_ : ''a,nil : ''a set) = false
  | isElem(x,y::ys) = if x = y then true else isElem(x,ys)

fun setAddElem(x : ''a,ys : ''a set) = if isElem(x,ys) then ys else x::ys

fun setUnion(nil : ''a set,ys : ''a set) = ys
  | setUnion(x::xs,ys) = setAddElem(x,setUnion(xs,ys))

fun setFilter(nil : 'a set,g) = emptySet
  | setFilter(x::xs,g) = if g(x) then x::setFilter(xs,g) else setFilter(xs,g)

fun isSubset(nil : ''a set,ys : ''a set) = true
  | isSubset(x::xs,ys) = isElem(x,ys) andalso isSubset(xs,ys)

fun areEqualSets(xs : ''a set,ys : ''a set) = isSubset(xs,ys) andalso isSubset(ys,xs)


fun listMap(f,nil) = nil
  | listMap(f,x::xs) = f(x)::listMap(f,xs)

fun cartProd(nil : 'a set,ys : 'b set) : ('a * 'b) set = nil
  | cartProd(x::xs,ys) = listMap(fn y => (x,y),ys) @ cartProd(xs,ys)

fun powerSet(nil : 'a set) : 'a set set = [nil]
  | powerSet(x::xs) = let val p = powerSet(xs) in p @ listMap(fn y => x::y,p) end
