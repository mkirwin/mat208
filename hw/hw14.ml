Control.Print.printLength := 10000;
Control.Print.printDepth := 1000;

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


(*** Start Homework 14 Code ***)
datatype suit = Club | Diamond | Heart | Spade;
datatype rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King;
type card = rank*suit;

val allSuits = [Club,Diamond,Spade,Heart];
val allRanks = [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King];

fun isNotEqual((a,b):card, (c,d)) = if (a,b) = (c,d) then false else true;
fun isSpade((a,b):card, (c,d)) = if b = Spade then true else false;
fun isNotAce((a,b):card,(c,d)) = if c = Ace then false else true;

val allCards = cartProd(allRanks,allSuits);
val allPairs = cartProd(allCards,allCards);

val prob4 = setFilter(setFilter(setFilter(allPairs,isNotEqual),isSpade),isNotAce); 
val lenProb4 = len(prob4);
fun len([]) = 0 | len(x::xs) = 1 + len(xs);
