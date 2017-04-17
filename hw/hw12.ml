Control.Print.printDepth := 10000;
datatype binTree = Null | Node of (int * binTree * binTree);

val t = Node(1,Null, Node(2,Null,Null));
val p = Node(1, t, t);
val q = Node(1,Node(4,Null,Null), Node(2,Node(3,Null,Null),Null));
val r = Node(7,Node(4,Node(2,Node(1,Null,Null),Node(3,Null,Null)),Node(5,Null,Null)),Node(11,Null,Null));
val z = Node(3, Node(5,Null,Null), Node(9,Null,Null));

fun numNulls(Null) = 1
    | numNulls(Node(a,s,t)) = numNulls(s) + numNulls(t);

fun numNodes(Null) = 0
    | numNodes(Node(a, s, t)) = 1 + numNodes(s) + numNodes(t);


fun binTreeMap(f : int->int, Null) = Null
    | binTreeMap(f, Node(a,s,t)) = Node(f(a), binTreeMap(f,s), binTreeMap(f,t));

fun flattenBinTree(Null) = nil : int list
    | flattenBinTree(Node(a,s,t)) = flattenBinTree(s)@[a]@flattenBinTree(t);

(*Problem 4: 
    *Citation: Talked to Black about Let-Expressions*)
fun depth(Null) = 0
    | depth(Node(a,s,t)) =
        let val Left = depth(s) val Right = depth(t)
        in if Left > Right then 1 + Left else 1 + Right
        end;

(*Problem 5*)
fun klist(0, b::bs) = nil
    | klist(k,b::bs) = b::klist(k-1,bs);

fun middle(0, b::bs) = b
    | middle(k, b::bs) = middle(k-1,bs);

fun rest(~1, bs) = bs
    | rest(k, b::bs) = rest(k-1,bs);

fun split(k:int, b::bs) = 
    let val lst = klist(k, b::bs)
        val mid = middle(k, b::bs)
        val rst = rest(k, b::bs)
    in (lst, mid, rst) end;

fun len(nil) = 0
    | len(b::bs) = 1 + len(bs);

fun listToBinTree([]) = Null
    | listToBinTree([a]) = Node(a,Null,Null)
    |listToBinTree(bs) =
        let 
            val splitval = ceil(real(len(bs))/2.0)
            val tuple = split(splitval,bs)
            val left = #1(tuple)
            val mid = #2(tuple)
            val right = #3(tuple)
         in Node(mid, listToBinTree(left), listToBinTree(right)) end;
