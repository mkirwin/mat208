datatype propForm = Var of string 
    | Neg of propForm
    | And of propForm * propForm
    | Or of propForm * propForm;


fun numConnectives(Var(v)) = 0
    | numConnectives(Neg(v)) = 1 + numConnectives(v)
    | numConnectives(And(v,w)) = 1 + numConnectives(v) + numConnectives(w)
    | numConnectives(Or(v,w)) = 1 + numConnectives(v) + numConnectives(w);

