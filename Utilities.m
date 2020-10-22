(* ::Package:: *)

(* Utility functions. The context ACDev` prevents the pollution of the Global` context. *)
BeginPackage["Utilities`"];

ACDev`PadLists::usage = "\
PadLists[list1] returns list1
PadLists[list1, list2, ...] returns {list1, list2, ...} with all lists padded with zeros on the right to have equal length
";

ACDev`LexiLess::usage = "\
LexiLess[list1, list2] returns True if list1 < list2 in the lexicographic order and False otherwise
";

ACDev`LexiLessEq::usage = "\
LexiLessEq[list1, list2] returns True if list1 \[LessEqual] list2 in the lexicographic order and False otherwise
";

NonNegativeIntegerQ::usage ="\
NonNegativeIntegerQ[x] returns True if x can be simplified into a non negative integer and False otherwise
";

ACDev`OrdersArray::usage ="\
OrdersArray[x,y,m] creates the list {{x,y},{x,y-1},...,{x,0},{x-1,y},...} and takes its first m elements
";


Begin["`Private`"]; (* Begin Private Context *)

Clear[Utilities`ACDev`PadLists];
ACDev`PadLists[x_List]:= x;
ACDev`PadLists[x_List, xs__List]:= PadRight[#,Max[Length/@{x,Sequence@xs}]]&/@{x,Sequence@xs};

Clear[Utilities`ACDev`LexiLess];
ACDev`LexiLess[x2_List,y2_List] := With[
	{
	x=(ACDev`PadLists@@{x2,y2})[[1]],
	y=(ACDev`PadLists@@{x2,y2})[[2]]
	},
	If[
		x==y,
		False,
		#==1&@(Select[Table[Which[x[[i]]<y[[i]],1,x[[i]]==y[[i]] ,0,True,-1],{i,1,Min[Length/@{x,y}]}],#!=0&][[1]])
	]
];

Clear[ACDev`LexiLessEq];
ACDev`LexiLessEq[x_List,y_List]:= Equal@@(ACDev`PadLists[x,y]) || ACDev`LexiLess[x,y];

Clear[NonNegativeIntegerQ];
NonNegativeIntegerQ[x_]/;NumericQ[x]:=(x-Round[x]==0)&&x>=0//FullSimplify;

Clear[ACDev`OrdersArray];
ACDev`OrdersArray[x_Integer,y_Integer,m_Integer]:=Flatten[Table[{i,j},{i,x,x-m,-1},{j,y,0,-1}],1][[1;;m]];

End[]; (* End Private Context *)
EndPackage[];
