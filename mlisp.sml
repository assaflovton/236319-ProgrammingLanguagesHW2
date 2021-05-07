exception Undefined;
exception Empty;
fun initEnv () =fn s:string=> raise Undefined;
val env : (string -> int) = initEnv();
fun define (s:string) f (v) =fn (s2:string) => if s2=s then v else (f s2);
 val env : (string -> int) = initEnv();
fun emptyNestedEnv () = [initEnv ()];
fun pushEnv f list= f::list;
fun popEnv  []= raise Empty|
popEnv (x::xs) =  (xs);
fun topEnv []= raise Empty|
topEnv (x::xs) = x;
fun defineNested (s:string) [] v = raise Empty|
defineNested (s:string) (x::xs) v = (define s (topEnv (x::xs)) v)::xs; 
fun find (s:string) []  = raise Undefined|
find (s:string) (x::xs) = ((topEnv (x::xs)) s)  handle Undefined=>(find s (popEnv (x::xs)));
