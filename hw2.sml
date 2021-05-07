
open String;
open Char;

fun valAt (x::xs) 0 = x | valAt [] i = 0 | valAt (x::xs) i = (valAt (xs) (i-1));

 fun sumAtIndices (x::xs) [] = 0 | sumAtIndices [] (x::xs) = 0 | sumAtIndices [] [] = 0 |
 sumAtIndices (x::xs) (i::is) = (valAt (x::xs) i) + sumAtIndices (x::xs) (is);

local
    fun toLowerAux  [] =nil |
    toLowerAux (x::xs)=if (isUpper x) then ((toLower x)::(toLowerAux (xs))) else (x::(toLowerAux (xs))); 
in
    fun toLower s:string = (implode(toLowerAux (explode s)));
end;

local
    fun countOccurrsAux  ([],c)=0|
    countOccurrsAux ((x::xs),c:char)  = if (x=c) then 1+(countOccurrsAux ((xs),c)) else (countOccurrsAux ((xs),c));
    in
fun countOccurrs (s,c)=(countOccurrsAux ((explode (toLower s)),c));
end;

local
    fun getAllOccurrsAux (s,[]) = nil |
    getAllOccurrsAux (s,(i::is))= ((i,countOccurrs(s,i))::(getAllOccurrsAux(s,(is))));
in
fun getAllOccurrs s=(getAllOccurrsAux (s,[#"a",#"b",#"c",#"d",#"e",#"f",#"g",#"h",#"i",#"g",#"k",#"l",#"m",#"n",#"o",#"p",#"q",#"r",#"s",#"t",#"u",#"v",#"w",#"x",#"y",#"z"]));
end;

local
fun compereTuple ((x,y:int),(a,b:int))=if(y=b) then true else false;
fun compereLists ([],[])=true|
compereLists ((x::xs),[])=false|
compereLists ([],(i::is))=false|
compereLists ((x::xs),(i::is))=if (compereTuple(x,i)) then  compereLists((xs),(is)) else false;
in
fun areAnagrams (s1,s2)=(compereLists ((getAllOccurrs s1),(getAllOccurrs s2)));
end;
