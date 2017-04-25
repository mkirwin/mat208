Control.Print.printLength := 10000;
Control.Print.printDepth := 1000;

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




datatype propForm = Var of string
              | Neg of propForm
              | And of propForm * propForm
              | Or of propForm * propForm

val p = And(Neg(Var("A")),Or(Var("B"),Var("C")))

val q = Or(Var("C"),And(p,Var("B")))

fun numConnectives(Var(a)) = 0
  | numConnectives(Neg(p)) = 1 + numConnectives(p)
  | numConnectives(And(p,q)) = 1 + numConnectives(p) + numConnectives(q)
  | numConnectives(Or(p,q)) = 1 + numConnectives(p) + numConnectives(q)


fun max(a,b) = if a > b then a else b

fun depth(Var(a)) = 0
  | depth(Neg(p)) = 1 + depth(p)
  | depth(And(p,q)) = 1 + max(depth(p),depth(q))
  | depth(Or(p,q)) = 1 + max(depth(p),depth(q))


fun occurringVars(Var(a)) : string set = [a]
  | occurringVars(Neg(p)) = occurringVars(p)
  | occurringVars(And(p,q)) = setUnion(occurringVars(p),occurringVars(q))
  | occurringVars(Or(p,q)) = setUnion(occurringVars(p),occurringVars(q))

fun display(Var(a)) = a
  | display(Neg(p)) = "(~" ^ display(p) ^ ")"
  | display(And(p,q)) = "(" ^ display(p) ^ " & " ^ display(q) ^ ")"
  | display(Or(p,q)) = "(" ^ display(p) ^ " | " ^ display(q) ^ ")"



fun truthValue(Var(a),xs : string set) = isElem(a,xs)
  | truthValue(Neg(p),xs) = not(truthValue(p,xs))
  | truthValue(And(p,q),xs) = truthValue(p,xs) andalso truthValue(q,xs)
  | truthValue(Or(p,q),xs) = truthValue(p,xs) orelse truthValue(q,xs)


fun allTruthValues(p) = listMap(fn xs => truthValue(p,xs),powerSet(occurringVars(p)))

fun truthTable(p) = listMap(fn xs => (xs,truthValue(p,xs)),powerSet(occurringVars(p)))



fun implies(p,q) = Or(Neg(p), q)

val r = implies(Var("C"),p)



  
