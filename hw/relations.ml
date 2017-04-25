type 'a set = 'a list 

val emptySet = nil : 'a set

fun isElem(_ : ''a,nil : ''a set) = false
  | isElem(x,y::ys) = if x = y then true else isElem(x,ys)

fun setAddElem(x : ''a,ys : ''a set) = if isElem(x,ys) then ys else x::ys

fun setUnion(nil : ''a set,ys : ''a set) = ys
  | setUnion(x::xs,ys) = setAddElem(x,setUnion(xs,ys))

fun listMap(f,nil) = nil
  | listMap(f,x::xs) = f(x)::listMap(f,xs)



val R1 = [(1,1),(1,5),(3,4),(4,1),(4,7),(4,4),(5,3)]

val R2 = [(5,2),(1,2),(4,8)]


fun image(a : ''a,nil : (''a * 'b) set) = nil : 'b set
  | image(a,(x,y)::rs) = if a = x then y::image(a,rs) else image(a,rs)

fun imageSet(nil : ''a set,rs : (''a * ''b) set) = nil : ''b set
  | imageSet(c::cs,rs) = setUnion(image(c,rs),imageSet(cs,rs))

fun inverseRelation(nil : ('a * 'b) set) = nil : ('b * 'a) set
  | inverseRelation((x,y)::rs) = (y,x)::inverseRelation(rs)

fun inverseRelation2(rs : ('a * 'b) set) : ('b * 'a) set = listMap(fn (x,y) => (y,x),rs)

fun composeRelations(nil : (''a * ''b) set,ws : (''b * ''c) set) = nil : (''a * ''c) set
  | composeRelations((x,y)::rs,ws) = setUnion(listMap(fn z => (x,z),image(y,ws)),composeRelations(rs,ws))





val R = [(1,1),(2,2),(2,3),(2,5),(3,2),(3,3),(3,5),(4,4),(5,2),(5,3),(5,5)]


fun obtainSet(nil : (''a * ''a) set) : ''a set = nil
  | obtainSet((x,y)::rs) = if x=y then x::obtainSet(rs) else obtainSet(rs)

fun thinSet(nil : ''a set,rs : (''a * ''a) set) : ''a set = nil
  | thinSet(y::ys,rs) = let val zs = thinSet(ys,rs) in if isElem(y,imageSet(zs,rs)) then zs else y::zs end

fun uniqueReps(rs : (''a * ''a) set) : ''a set = thinSet(obtainSet(rs),rs)

fun equivClasses(rs : (''a * ''a) set) : ''a set set = listMap(fn y => image(y,rs),uniqueReps(rs))

