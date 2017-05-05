(*Homework 15*)


fun addLists(_,[] : IntInf.int list) = []
    | addLists([],_) = []
    | addLists(x::xs, y::ys) = x+y::addLists(xs,ys);

(*fun nextPascalRow([] : IntInf.int list) = [1]
    | nextPascalRow(x::xs) = x::addLists(x,xs)::nextPascalRow(xs);
    *)

fun nextPascalRow([]) = [1]
    | nextPascalRow(xs: IntInf.int list) =
      let val c = 0 ::xs in
      let val d = xs @ [0]
      in addLists(c,d) end end;

fun pascalRowH(row,0) = row
    | pascalRowH(row,n) = pascalRowH(nextPascalRow(row),n-1);

fun pascalRow(n) = pascalRowH([1],n);
