datatype propForm = Var of string
    | Neg of propForm
    | And of propForm * propForm
    | Or of propForm * propForm;

    (*
fun getVars(Var(v)) = "v"
    | getVars(Neg(v)) = getVars(v)
    | getVars(And(p,q)) = getVars(p): propForm  @ getVars(q) : propForm
    | getVars(Or(p,q)) = getVars(p): propForm @ getVars(q): propForm;
    *)

fun numConnectives(Var(v)) = 0
    | numConnectives(Neg(v)) = 1 + numConnectives(v)
    | numConnectives(And(p,q)) = 1 + numConnectives(p) + numConnectives(q)
    | numConnectives(Or(p,q)) = 1 + numConnectives(p) + numConnectives(q);

fun numVars(Var(v)) = 1
    | numVars(Neg(v)) = numVars(v)
    | numVars(And(p,q)) = numVars(p) + numVars(q)
    | numVars(Or(p,q)) = numVars(p) + numVars(q);
