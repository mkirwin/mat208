datatype binTree = Null | Node of (int * binTree * binTree);

val t = Node(1,Null, Node(2,Null,Null));
val p = Node(1, t, t);
val q = Node(1,Node(4,Null,Null), Node(2,Node(3,Null,Null),Null));
val r = Node(7,Node(4,Node(2,Node(1,Null,Null),Node(3,Null,Null)),Node(5,Null,Null)),Node(11,Null,Null));

(*=====Problem=2=====*)
fun numNulls(Null) = 1
    | numNulls(Node(a,s,t)) = numNulls(s) + numNulls(t);

fun numNodes(Null) = 0
    | numNodes(Node(a, s, t)) = 1 + numNodes(s) + numNodes(t);


(*=====Problem=4=====*)

fun binTreeMap(f : int->int, Null) = Null
    | binTreeMap(f, Node(a,s,t)) = Node(f(a), binTreeMap(f,s), binTreeMap(f,t));

fun flattenBinTree(Null) = nil : int list
    | flattenBinTree(Node(a,s,t)) = flattenBinTree(s)@[a]@flattenBinTree(t);

