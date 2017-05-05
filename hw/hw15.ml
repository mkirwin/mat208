(*Homework 15*)

fun addLists(_,[] : IntInf.int list) = []
    | addLists([],_) = []
    | addLists(x::xs, y::ys) = x+y::addLists(xs,ys);

fun nextPascalRow([]) = [1]
    | nextPascalRow(xs: IntInf.int list) =
      let val placeholder = 0 ::xs 
          val bookend = xs @ [0]
      in addLists(placeholder,bookend) end;

fun pascalRowH(row,0) = row
    | pascalRowH(row,n) = 
        let val nxtrow = nextPascalRow(row)
        in pascalRowH(nxtrow,n-1) end;

fun pascalRow(n) = pascalRowH([1],n);

fun getKElem(0,x::xs) = x
    | getKElem(k, x::xs) = getKElem(k-1, xs);

fun choose(n,k) =
    let val row = pascalRow(n)
    in getKElem(k, row) end;
