fun sumList(nil) = 0 | sumList(x::xs) = x + sumList(xs)
fun sumProduct(nil) = (0,1) 
| sumProduct(x::xs) = 
    let val(s,p) = sumProduct(xs)
    in (x+s, x*p) end
