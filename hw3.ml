type 'a set = 'a list 

val emptySet = nil : 'a set

fun isElem(_ : ''a,nil : ''a set) = false
  | isElem(x,y::ys) = if x = y then true else isElem(x,ys)

fun setAddElem(x : ''a,ys : ''a set) = if isElem(x,ys) then ys else x::ys

fun setUnion(nil : ''a set,ys : ''a set) = ys
  | setUnion(x::xs,ys) = setAddElem(x,setUnion(xs,ys))

fun setFilter(nil : 'a set,g) = emptySet
  | setFilter(x::xs,g) = if g(x) then x::setFilter(xs,g) else setFilter(xs,g)





exception BadPosition

fun inPosition(nil,_) = raise BadPosition
  | inPosition(x::xs,0) = x
  | inPosition(x::xs,k) = inPosition(xs,k-1)

fun take(xs,0) = nil
  | take(nil,k) = nil
  | take(x::xs,k) = x::take(xs,k-1)

fun flatten(nil) = nil
  | flatten(x::xs) = x @ flatten(xs)

fun zip(nil,ys) = nil
  | zip(xs,nil) = nil
  | zip(x::xs,y::ys) = (x,y)::zip(xs,ys)




fun intervalSet(m,n) : int set = if m > n then emptySet else m::intervalSet(m+1,n)

fun setIntersection(nil : ''a set,ys : ''a set) = emptySet
  | setIntersection(x::xs,ys) = if isElem(x,ys) then x::setIntersection(xs,ys) else setIntersection(xs,ys)

exception MaxOfEmptySet

fun setMax(nil : int set) = raise MaxOfEmptySet
  | setMax(x::nil) = x
  | setMax(x::xs) = let val m = setMax(xs) in if x > m then x else m end

fun setMap(g,nil : ''a set) : ''b set = nil
  | setMap(g,x::xs) = setAddElem(g(x),setMap(g,xs))




fun divisorSet(n) = setFilter(intervalSet(1,n),fn d => n mod d = 0)

fun gcd(m,n) = setMax(setIntersection(divisorSet(m),divisorSet(n)))




