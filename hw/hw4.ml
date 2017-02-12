datatype intOption = NONE | SOME of int;

fun optionMax(NONE,NONE) = NONE
    | optionMax(NONE,SOME(b)) = SOME(b)
    | optionMax(SOME(a), NONE) = SOME(a)
    | optionMax(SOME(a), SOME(b)) = if a > b then SOME(a) else SOME(b);
