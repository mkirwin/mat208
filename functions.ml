datatype suit = Club | Diamond | Heart | Spade

datatype rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King

datatype card = Card of rank * suit

datatype color = RGB of int * int * int;

datatype money = USD of real | CND of real | Euro of real | Pound of real

datatype number = Int of int | Real of real



fun f(x : int) : int = 2*x + 7

fun g(x) = 2*x + 7

fun h(x) = 2.0*x + 7.0

fun squareZ(x) = x*x;

fun squareR(x : real) = x*x;



fun myNot1(x) = if x = true then false else true

fun myNot2(x) = case x of
    true => false
  | false => true

fun myNot3(true) = false
  | myNot3(false) = true

val myNot4 = fn x => if x = true then false else true




fun inUSD(USD r) = USD r
  | inUSD(CND r) = USD (r * 0.76)
  | inUSD(Euro r) = USD (r * 1.07)
  | inUSD(Pound r) = USD (r * 1.25)

fun inUSD2(x) = case x of 
    (USD r) => USD r
  | (CND r) => USD (r * 0.76)
  | (Euro r) => USD (r * 1.07)
  | (Pound r) => USD (r * 1.25)


fun nextSuit(Club) = Diamond
  | nextSuit(Diamond) = Heart
  | nextSuit(Heart) = Spade
  | nextSuit(Spade) = Club


fun redValue(RGB(a,b,c)) = a

fun square(Int x) = Int(x*x)
  | square(Real x) = Real(x*x)




fun fact1(n) = if n = 0 then 1 else n * fact1(n-1)

fun fact2(0) = 1
  | fact2(n) = n * fact2(n-1)

local
  fun helper(0,r) = r
    | helper(n,r) = helper(n-1,n*r)
in
  fun fact3(n) = helper(n,1)
end





fun exp1(a,0) = 1.0
  | exp1(a,n) = a * exp1(a,n-1)

fun exp2(a,0) = 1.0
  | exp2(a,n) = if n > 0 then a * exp2(a,n-1) else exp2(a,n+1) / a

fun exp3(a,n) = if n < 0 then 1.0/exp1(a,~n) else exp1(a,n)



fun sumInitial(0) = 0
  | sumInitial(n) = sumInitial(n-1) + n


