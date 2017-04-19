(* Mentor Session *)

(* Define a sequence by a0 = 1, a1 = 3, and an = 3*a(n-1) + 2*a(n-2) for all n \in \N with n > 1.*)

fun s(0,a,b) = a
    | s(n,a,b) = s(n-1,b,3*b+2*a);

fun sequence(n) = s(n,1,3);
