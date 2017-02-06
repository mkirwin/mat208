(*Problem 1:*)

(*inPosition: Takes two inputs, xs and k, and produces the kth element of the list. If k is not less than the length of the list, your program should raise an exception*)

fun inPosition(x::xs,k) = if k=0 then x else inPosition(xs,k-1);

(* go through the list until you reach the index, k
 * if you reach the end of the list before k, then throw error?*)


(*take: Takes two inputs, xs and k, and produces the list of the first k values of xs.*)
(*Throws weird error for take([],k)*)
fun take(nil,k) = nil
    | take(x::xs,0) = nil
    | take(x::xs,k) = x :: take(xs,k-1)

(*flatten: Takes xs, an alpha list list, and produces the alpha list that results from conatenating the elements of xs.
 fun flatten(nil) = nil
    | flatten(x) = x
    | flatten(x::xs) = flatten(x) @ flatten(xs);
    *)


(***************************************************************************************************************)
(*Problem 2:*)

(*intervalSet: Takes two integer inputs, m and n, and produces the integers set {m,m+1,m+2,...n }. If m>n, then produce empty set.}*)

fun intervalSet(m,n) = if m>n then nil 
    else if m=n then m 
        else m::intervalSum(m+1,n);
