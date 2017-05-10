Control.Print.printLength := 10000;
Control.Print.printDepth := 1000;
Control.Print.intinfDepth := 10000;

type 'a set = 'a list 

val emptySet = nil : 'a set

fun isElem(_ : ''a,nil : ''a set) = false
  | isElem(x,y::ys) = if x = y then true else isElem(x,ys)

fun setAddElem(x : ''a,ys : ''a set) = if isElem(x,ys) then ys else x::ys

fun setUnion(nil : ''a set,ys : ''a set) = ys
  | setUnion(x::xs,ys) = setAddElem(x,setUnion(xs,ys))

fun setIntersection(nil : ''a set,ys : ''a set) = emptySet
  | setIntersection(x::xs,ys) = if isElem(x,ys) then x::setIntersection(xs,ys) else setIntersection(xs,ys)

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

fun setMap(g,nil : ''a set) : ''b set = nil
  | setMap(g,x::xs) = setAddElem(g(x),setMap(g,xs))


fun image(a : ''a,nil : (''a * 'b) set) = nil : 'b set
  | image(a,(x,y)::rs) = if a = x then y::image(a,rs) else image(a,rs)

fun flatten(nil) = nil
  | flatten(x::xs) = x @ flatten(xs)



fun intervalSet(m,n) : int set = if m > n then emptySet else m::intervalSet(m+1,n)


type 'a graph = ('a set) * (('a * 'a) set)

fun pathDigraph(n) : int graph = let val vs = intervalSet(2,n) in (1::vs,listMap(fn v => (v-1,v),vs)) end

fun cycleDigraph(n) : int graph = let val vs = intervalSet(2,n) in (1::vs,(n,1)::listMap(fn v => (v-1,v),vs)) end

fun completeDigraph(n) : int graph = let val vs = intervalSet(1,n) in (vs,cartProd(vs,vs)) end

fun divDigraph(n) : int graph = let val vs = intervalSet(1,n) in (vs,setFilter(cartProd(vs,vs),fn (v,w) => w mod v = 0)) end

val digraphExample = (intervalSet(1,8),[(1,2),(2,3),(3,4),(4,5),(3,8),(1,7),(7,5),(6,7)]) : int graph


fun outDegree(v : ''a,(vs,es) : ''a graph) = length(setFilter(es,fn (u,w) => u = v))

fun inDegree(v : ''a,(vs,es) : ''a graph) = length(setFilter(es,fn (u,w) => w = v))


fun dfsHelper(nil,es,visited) = rev(visited)
  | dfsHelper(b::bs,es,visited) = if isElem(b,visited) then dfsHelper(bs,es,visited)
        else dfsHelper(image(b,es) @ bs,es,b::visited) 

fun dfs(bs : ''a list,(vs,es) : ''a graph) = dfsHelper(bs,es,nil)



fun bfsHelper(nil,es,visited) = rev(visited)
  | bfsHelper(b::bs,es,visited) = if isElem(b,visited) then bfsHelper(bs,es,visited)
        else bfsHelper(bs @ image(b,es),es,b::visited) 

fun bfs(bs : ''a list,(vs,es) : ''a graph) = bfsHelper(bs,es,nil)



