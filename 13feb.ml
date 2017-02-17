datatype propForm = Var of string 
    | Neg of propForm
    | And of propForm * propForm
    | Or of propForm * propForm;


fun numConnectives(Var(v)) = 0
    | numConnectives(Neg(v)) = 1 + numConnectives(v)
    | numConnectives(And(v,w)) = 1 + numConnectives(v) + numConnectives(w)
    | numConnectives(Or(v,w)) = 1 + numConnectives(v) + numConnectives(w);


fun max(a,b) = if a > b then a else b;

fun depth(Var(v)) = 0
    | depth(Neg(v)) = 1 + depth(v) 
    | depth(And(v,w)) = 1 + max(depth(v),depth(w)) 
    | depth(Or(v,w)) = 1 + max(depth(v),depth(w));

fun display(Var(a)) = a
    | display(Neg(p)) = "(~" ^ display(p) ^ ")"
    | display(And(v,w)) = "" + display(v) + "&" + display(w) + ""
(* Need to finish *)


(*fun truthVal(p,xs)*)
