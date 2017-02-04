fun sumList(nil) = 0 | sumList(x::xs) = x + sumList(xs)

fun sumProduct(nil) = (0,1) 
| sumProduct(x::xs) = 
    let val(s,p) = sumProduct(xs)
    in (x+s, x*p) end


fun summation(1) = 1 
    | summation(n) = n + summation(n-1);


(*03 February 2017*)
(*Illustrating value storage: Use this on "command line"*)
    let val x = 3 val y = 4 in x + y end;
    val (a,b) = (1,5); (*Using pattern matching to assign variables*)

(* xs type: 'a list
 * f type: 'a->'b
 * output type: 'b list
 * myMap type: (('a->'b) * 'a list) -> 'b list) 
 * cannot compare functions *)

fun myMap(f,nil) = nil
    | myMap(f,x::xs) = f(x) :: myMap(f,xs);


fun isElem(x,nil) = false
    | isElem(x,y::ys) = if x = y then true else isElem(x,ys);

fun setAddElem(x,nil) = [x]
    | setAddElem(x,y::ys) = if not(isElem(x,ys)) then x::ys else y::ys;

(*fun setUnion(xs,ys) = ; *)
