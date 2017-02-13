fun double(nil) = nil
    | double(x::xs) = x :: x :: double(xs);

fun prodList(nil) = 1
    | prodList(x::xs) = x * prodList(xs);
