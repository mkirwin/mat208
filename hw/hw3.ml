(*Problem 1:*)

(* A *)
(*inPosition: Takes two inputs, xs and k, and produces the kth element of the list. If k is not less than the length of the list, your program should raise an exception*)

exception NotValidPos of int

fun inPosition(nil,k) = raise NotValidPos(k)
    | inPosition(x::xs,k) = if k=0 then x else inPosition(xs,k-1);

(* go through the list until you reach the index, k
 * if you reach the end of the list before k, then throw error?*)


(* B *)
(*take: Takes two inputs, xs and k, and produces the list of the first k values of xs.*)
(*Throws weird error for take([],k)*)
fun take(nil,k) = nil
    | take(x::xs,0) = nil
    | take(x::xs,k) = x :: take(xs,k-1)

(* C *)    
(*flatten: Takes xs, an alpha list list, and produces the alpha list that results from conatenating the elements of xs. *)
 fun flatten(nil) = nil
    | flatten(x::xs) = x @ flatten(xs);


(* D *)    
(*zip: Takes 'a list and 'b list and produces ('a * 'b) list *) 
fun  zip (nil, _) = []
    | zip (_, nil) = []  
    | zip(x::xs, y::ys) = (x,y) :: zip(xs,ys); 

(***************************************************************************************)
(*Problem 2:*)

(* A *)    
(*intervalSet: Takes two integer inputs, m and n, and produces the integers set {m,m+1,m+2,...n }. If m>n, then produce empty set.}*)

fun intervalSet(m,n) = if m <= n then m :: intervalSet(m+1,n) else [];    

(* B *)
(*setIntersection: Takes two 'a sets and produces their intersections.*)
fun isElem(_,nil) = false
    | isElem(x,y::ys) = if x = y then true else isElem(x,ys);
   
(*Make sure it's a set? so remove repeats?*)
(*setify makes lists sets by removing repeated elements *)
fun setify(nil) = nil
    | setify(x::xs) = if isElem(x,xs) then setify(xs) else x :: setify(xs);


(*If you want any of the arguments to setIntersection to be [], you must indicate the 
 * type of the sets. For example: setIntersection([] : int list, []); 
 * The sets can be of any type, but they must both be of the same type. *)
fun setIntersection(nil, _) = []
    | setIntersection(_, nil) = []
    | setIntersection(x::xs, y::ys) = if isElem(x,y::ys) then setify(x :: setIntersection (xs,y::ys)) else setify(setIntersection(xs,y::ys));


(* C *)    
exception EmptySet
fun setMax([]) =  raise EmptySet 
    | setMax([x]) = x
    | setMax(x::xs) = if x > setMax(xs) then x else setMax(xs);


(* D *)    
fun setMap(nil, g) = nil
    | setMap(x::xs, g) = setify (g(x) :: setMap(xs,g));

(**************************************************************************************)
(* Problem 3: *) 

(* A *)
fun setFilter(nil,g) = nil
    | setFilter(x::xs,g) = if g(x) then x :: setFilter(xs,g) else setFilter(xs,g);


(* B *)    
fun divisorSet(n) = setFilter(intervalSet(1,n), fn x=>n mod x = 0);

(* C *)
fun gcd(a,b) = setMax(setIntersection(divisorSet(a), divisorSet(b)));

(* D *)
(* I tested gcd on many inputs and noticed a few trends. When the arguments were 100000
 * and 100000, the runtime got slower. However, when I tested gcd(99999,99999), arguments only one less than the previous arguments, the runtime was lower again. 
 *
 * *)

