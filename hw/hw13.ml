Control.Print.printLength := 10000;
Control.Print.printDepth := 10;
Control.Print.intinfDepth := 10000;


fun g(0,c) = c
  | g(n,c) = g(n-1,c*n)

fun fact(n) = g(n,1)


fun f(0 : IntInf.int) = 0 : IntInf.int
  | f(1) = 1
  | f(n) = f(n-1) + f(n-2)


fun h(0,a : IntInf.int,b : IntInf.int) = a : IntInf.int
  | h(n,a,b) = h(n-1,b,a+b)

fun fib(n) = h(n,0,1)


fun power(a : IntInf.int, 0 : IntInf.int) = 1 : IntInf.int
  | power(a,n) = if (n mod 2 = 0) then
       let val halfPower = power(a,n div 2) in halfPower * halfPower end
       else a * power(a,n-1)



fun intSquareRoot(0 : IntInf.int) = 0 : IntInf.int
  | intSquareRoot(n) = let val m = 2 * intSquareRoot(n div 4) in 
          if (m+1)*(m+1) > n then m else m+1 end



fun gcd(a : IntInf.int, 0 : IntInf.int) = a
  | gcd(a,b) = gcd(b,a mod b)


(* Start HW 13 here*)

(* Problem 1 *)
fun addRat((a:IntInf.int, b),(c, d)) =
    let val numer = a*d+c*b
        val denom = d*b
        val gcd = gcd(numer,denom)
    in 
        (numer div gcd, denom div gcd)
    end;

(* Problem 2 *)
(* Jemuel reminded me what you do with r in the first if statement! *)
fun prob2a(0:int,b) = (0,0)
    | prob2a(a:int ,b:int) =
        let val (q,r) = prob2a(a-1,b)
        in if r = b-1  then (q+1,0)  else (q,r+1) end;

(* Problem 3 *)    
fun mult(a:IntInf.int,b) =
    let val q = b div 2
        val r = b mod 2
    in (a+a)*q+r end;



