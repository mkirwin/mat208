fun multList(nil,a) = [] 
    | multList(x::xs,a) = x*a :: multList(xs,a); 

fun even(x) = if x mod 2 = 0 then true else false;

fun myFilter (nil, f) = []
    | myFilter (x::xs, f) = if f(x) = true then x::myFilter(xs,f) else myFilter(xs,f);

(*Class notes: 06 Feb 2017*)
(*ANONYMOUS FUNCTIONS*) fn n => n mod 3 = 0;
fun isEven(n) = n mod 2 = 0;

fun cartProd()
