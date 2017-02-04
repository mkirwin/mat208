(*Problem 1:*)

(*inPosition: Takes two inputs, xs and k, and produces the kth element of the list. If k is not less than the length of the list, your program should raise an exception*)

fun inPosition(x::xs,k) = if k=0 then x else inPosition(xs,k-1);

(* go through the list until you reach the index, k
 * if you reach the end of the list before k, then throw error?*)


(*take: Takes two inputs, xs and k, and produces the list of the first k values of xs.*)

fun take(x::xs,0) = nil
    | take(x::xs,k) = x :: take(xs,k-1)

(*Doesn't work on k = size(xs), need to fix.*)
