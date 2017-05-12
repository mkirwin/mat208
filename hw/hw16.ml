(* Homework 16 *)
use "../graphs.ml";
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


fun intervalSet(m,n) : int set = if m > n then emptySet else m::intervalSet(m+1,n)

fun listMap(f,nil) = nil
  | listMap(f,x::xs) = f(x)::listMap(f,xs)

fun cartProd(nil : 'a set,ys : 'b set) : ('a * 'b) set = nil
  | cartProd(x::xs,ys) = listMap(fn y => (x,y),ys) @ cartProd(xs,ys)

fun powerSet(nil : 'a set) : 'a set set = [nil]
  | powerSet(x::xs) = let val p = powerSet(xs) in p @ listMap(fn y => x::y,p) end

fun setMap(g,nil : ''a set) : ''b set = nil
  | setMap(g,x::xs) = setAddElem(g(x),setMap(g,xs))

fun setRemoveElem(_,nil) = nil
  | setRemoveElem(a,b::bs) = if a = b then bs else b::setRemoveElem(a,bs)

fun flatten(nil) = nil
  | flatten(x::xs) = x @ flatten(xs)

fun perm([]) = [[]]
  | perm(bs) = flatten(listMap(fn b => listMap(fn p => b::p,perm(setRemoveElem(b,bs))),bs))

fun subsetsOfSize(_,0) = [[]]
  | subsetsOfSize([],_) = []
  | subsetsOfSize(b::bs,k) = listMap(fn cs => b::cs,subsetsOfSize(bs,k-1)) @ subsetsOfSize(bs,k)

fun obtainSet(nil:(''a * ''a) set): ''a set = nil | obtainSet((x,y)::rs) = if x = y then x ::obtainSet(rs) else obtainSet(rs);

fun imageSet(nil : ''a set, rs : (''a * ''b) set) = nil : ''b set | imageSet(c::cs,rs) = setUnion(image(c, rs), imageSet(cs, rs));

fun thinSet(nil: ''a set, rs: (''a*''a) set): ''a set = nil | thinSet(y::ys,rs) = let val zs = thinSet(ys,rs) in if isElem(y, imageSet(zs, rs)) then zs else y::zs end;

fun uniqueReps(rs: (''a * ''a) set): ''a set = thinSet(obtainSet(rs),rs);

fun equivClasses(rs : (''a * ''a) set) : ''a set set = listMap(fn y => image(y, rs), uniqueReps(rs));
(*** START HW 16 CODE ***)

fun len([]) = 0
    | len (x::xs) = 1 + len(xs);

fun isDHelper([], n, len) = n = len 
    | isDHelper([x], n, len) = not (x = n)
    | isDHelper(x::xs, n, len) = 
        if x = n then false else isDHelper(xs, n+1, len);

fun isDerangement(xs) = 
    let val length = len(xs)
    in isDHelper(xs, 1, length) end;

fun derangements(n) = setFilter(perm(intervalSet(1,n)),isDerangement);

fun derangementRatio(n) = 
    let val set = intervalSet(1,n)
        val totals = perm(set)
        val numerator = derangements(n)
        val numTotals = len(totals)
        val numNumers = len(numerator)
        val frac = real(numNumers)/real(numTotals)
    in frac end;

(* Lukas helped me put all the pieces together and fix compile errors *)
fun zeroes(0) = [] | zeroes(k) = 0::zeroes(k-1);

fun composition(0,0) = [nil]
    | composition(n,1) = [[n]]
    | composition(0,k) = [zeroes(k)]
    | composition(n,0) = nil
    | composition(n,k) = 
        setUnion(setMap(fn n=>0::n, composition(n,k-1)),setMap(fn n::ns=> n+1::ns,composition(n-1,k)));

fun image(a : ''a, nil: (''a*'b) set) = nil : 'b set
    | image(a,(x,y)::rs) = if a = x then y ::image(a,rs) else image(a,rs);

fun codeGraph(nil,es) = nil
    | codeGraph(v::vs,es) = (v::image(v,es))::codeGraph(vs,es);

fun cross(x, []) = [] | cross(x,y::ys) = (x,y)::cross(x,ys);

fun walksH([], a) = [] | walksH(x::xs, a) = cross(x, (dfs(x::xs,a)))@walksH(xs,a);

fun getUWWalks((x::xs, ys): ''a graph) = walksH(x::xs, (x::xs,ys));
  
fun numConnected((x::xs,ys): ''a graph) = len(equivClasses(getUWWalks(x::xs,ys)));
