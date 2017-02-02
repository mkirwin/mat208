(*Problem 5: divideAll takes two ints and returns the result of dividing the first by the second
 * It should return a tuple with componenets: 
     * the integer quotient,
     * the integer remainder,
     * the real quotient*)
fun divideAll(a,b) = (trunc(real(a)/real(b)), a mod b, real(a)/real(b));


(*Problem 6: Write a function, reverse, that takes a string and reverses it.*)
fun reverse("") = ""
    | reverse(str) = reverse(substring(str,1,size(str)-1)) ^ substring(str,0,1);


(*Problem 7: Write a function that computes a geometric series, taking n, a, and r.*)
fun exp(a,0) = 1.0
    | exp(a,n) = a * exp(a,n-1);

fun geoSum(a,r,0) = a
    | geoSum(a,r,n) = a * exp(r,n) + geoSum(a,r,n-1)
(*Add exponentiation function from class*)
