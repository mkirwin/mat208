Control.Print.printLength := 10000;
Control.Print.printDepth := 1000;

datatype binTree = Null | Node of (int * binTree * binTree)

fun numNulls(Null) = 1
  | numNulls(Node(a,s,t)) = numNulls(s) + numNulls(t)

fun numNodes(Null) = 0
  | numNodes(Node(a,s,t)) = numNodes(s) + numNodes(t) + 1

val t1 = Node(1,Node(4,Null,Null),Node(2,Node(3,Null,Null),Null))

val t2 = Node(7,Node(4,Node(2,Node(1,Null,Null),Node(3,Null,Null)),Node(5,Null,Null)),Node(11,Null,Null))


fun flattenBinTree(Null) = []
  | flattenBinTree(Node(a,s,t)) = flattenBinTree(s) @ (a::flattenBinTree(t))

fun binTreeMap(f,Null) = Null
  | binTreeMap(f,Node(a,s,t)) = Node(f(a),binTreeMap(f,s),binTreeMap(f,t))


fun max(a,b) = if a > b then a else b

fun depth(Null) = 0
  | depth(Node(a,s,t)) = max(depth(s),depth(t)) + 1


exception BadPosition

fun split(_,nil) = raise BadPosition
  | split(0,b::bs) = (nil,b,bs)
  | split(k,b::bs) = let val (cs,d,ds) = split(k-1,bs) in (b::cs,d,ds) end

fun listToBinTree(nil) = Null
  | listToBinTree(bs) = let val (cs,d,ds) = split(length(bs) div 2,bs) in Node(d,listToBinTree(cs),listToBinTree(ds)) end

