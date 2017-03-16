(* Homework 8 *)

type 'a set = 'a list

val emptySet = nil : 'a set

val p = [(1,2),(1,3),(1,5),(5,6),(3,2),(6,1)];
val m = [(~1,0), (2,4)];
val n = [(1,2),(0,1),(6,1)];

fun isElem(_ : ''a, nil : ''a set) = false
    | isElem(x,y::ys) = if x = y then true else isElem(x,ys);

fun setAddElem(x : ''a, ys : ''a set) = if isElem(x,ys) then ys else x::ys;

fun setUnion(nil:''a set, ys : ''a set) = ys
    | setUnion(x::xs,ys) = setAddElem(x,setUnion(xs,ys));        

fun image(a,nil) = nil
    | image(a, (c1,c2)::rest) = if c1 = a then c2::image(a,rest)
        else image(a,rest);

fun imageSet(cs, nil) = nil
    | imageSet(nil,s) = nil
    | imageSet(c::cs,s) = setUnion(image(c,s), imageSet(cs,s));

fun inverseRelation(nil) = nil
    | inverseRelation((c1,c2)::rest) = (c2,c1)::inverseRelation(rest);

fun compHelper((h1,h2),nil) = nil
    | compHelper((h1,h2),(h3,h4)::rest) = if h2 = h3 then (h1,h4)::compHelper((h1,h2),rest)
        else compHelper((h1,h2), rest);

fun composeRelations(nil, cs) = nil
    | composeRelations((h1,h2)::rest,cs) =
        setUnion(compHelper((h1,h2), cs),composeRelations(rest,cs));
