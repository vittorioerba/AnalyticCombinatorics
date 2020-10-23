(* ::Package:: *)

BeginPackage["AsyScale`"];

AsyScale::usage = "\
AsyScale[{x,y,...},z] represents the expression \!\(\*SuperscriptBox[\(z\), \(x\)]\) Log[z\!\(\*SuperscriptBox[\(]\), \(y\)]\) ..., where z tends to Infinity
"

BigO::usage = "\
BigO[AsyScale[{x,y,...},z],z] represents a function that is asymptotic to the expression \!\(\*SuperscriptBox[\(z\), \(x\)]\) Log[z\!\(\*SuperscriptBox[\(]\), \(y\)]\) ..., when z tends to Infinity
BigO[y] when y is a number is equivalent to BigO[AsyScale[{0},z],z]
"

DropConstant::usage = "\
DropConstant[k AsyScale[...]] = AsyScale[...]
DropConstant[k] = 1
DropConstant[0] = 0
"

GetConstant::usage = "\
GetConstant[k AsyScale[...]] = k
GetConstant[k] = k
"

GetExponents::usage = "\
GetExponents[k AsyScale[xs,z]] = xs
GetExponents[k] = {0}
Works equivalently on BigO expressions
"

GetVariable::usage = "\
GetExponents[k AsyScale[xs,z]] = z
GetExponents[k] = NoVariable
Works equivalently on BigO expressions
"

NoVariable::usage ="\
Indicates that an expression was numeric, i.e. had no variables in the AsyScale sense
"

DropErrorTerms::usage = "\
DropErrorTerms[expr] eliminates all error terms (BigO) from expr
"

GetErrorTerms::usage = "\
GetErrorTerms[expr] retrieves all error terms (BigO) from expr
"

DropCorrections::usage ="\
DropCorrections[expr,z] retrieves the leading order of expr in variable z. \
If lower order terms are dropped, it provides the correct BigO error term. \
If terms in other variables are present, they are not dropped nor modified.
ATTENTION: There may be errors if a product of AsyScales with different variables is present.
"

LeadingOrder::usage ="\
Retrieve the leading order of expr in the variable z, dropping all error terms.
If terms in other variables are present, they are not dropped nor modified.
"

ClassicForm::usage ="\
ClassicForm[expr] converts the AsyScale representation into the classical Mathematica one, preserving the BigO notation
" 

AsyForm::usage ="\
AsyForm[expr] converts the classical Mathematica form into the AsyScale form, preserving the BigO notation
" 

ChangeOfVariable::usage = "\
ChangeOfVariable[z, h, expr] represents a change of variable z = expr(h) such that if h -> Infinity, z -> Infinity.
expr may be an ansymptotic expansion with error terms (BigO). 
"

Begin["`Private`"]; (************************************************************************ Begin Private Context *)

(******************************************************************************************** ASYSCALE ********)

(******** Basic simplification rules ********)
AsyScale[{x__,0},z_]:=AsyScale[{x},z]
AsyScale[{0},z_]:=1

(******** Addition ********)
Unprotect[Plus,Times];
a_. AsyScale[x_,z_]+b_. AsyScale[x_,z_]:=(a+b)AsyScale[x,z];
Protect[Plus,Times];

(******** Multiplication ********)
AsyScale /: AsyScale[x_,z_]*AsyScale[y_,z_]:=AsyScale[Plus@@ACDev`PadLists[x,y],z]

(******** Exponentiation ********)
AsyScale /:AsyScale[x_,z_]^y_:=AsyScale[y*x,z]

(******************************************************************************************** ASYSCALE ACCESS METHODS ********)

DropConstant[y_. AsyScale[x_,z_]]                :=AsyScale[x,z]
DropConstant[y_]                  /; NumericQ[y] :=1;
DropConstant[0]                                   :=0;

GetConstant[y_. AsyScale[x_,z_]]                :=y
GetConstant[y_]                  /; NumericQ[y] :=y;

GetExponents[y_. AsyScale[x_,z_]]                :=x
GetExponents[y_]                  /; NumericQ[y] :={0}

GetVariable[y_. AsyScale[x_,z_]]                := z
GetVariable[y_]                  /; NumericQ[y] := NoVariable 

(******************************************************************************************** BIGO ********)

(*BigO[AsyScale[xs_,z_]]:=BigO[AsyScale[xs,z],z]*)

(******** Constants ********)
BigO[y_ AsyScale[xs_,z_],z_]     := BigO[AsyScale[xs,z],z];
BigO[y_,z_] /; NumericQ[y]&&y!=1  := BigO[1,z]

(******** Composition of BigOs ********)
BigO /: BigO[BigO[expr_,z_],z_] := BigO[expr,z]

(******** Addition ********)
BigO /: BigO[AsyScale[xs_,z_],z_] + BigO[AsyScale[ws_,z_],z_] /; ACDev`LexiLessEq[ws,xs]                 := BigO[AsyScale[xs,z],z]
BigO /: BigO[AsyScale[xs_,z_],z_] + BigO[y_,z_]               /; NumericQ[y] && ACDev`LexiLessEq[{0},xs] := BigO[AsyScale[xs,z],z]
BigO /: BigO[AsyScale[xs_,z_],z_] + BigO[y_,z_]               /; NumericQ[y] && ACDev`LexiLessEq[xs,{0}] := BigO[1,z]
BigO /: BigO[AsyScale[xs_,z_],z_] + b_. AsyScale[ws_,z_]      /; ACDev`LexiLessEq[ws,xs]                 := BigO[AsyScale[xs,z],z]
BigO /: BigO[AsyScale[xs_,z_],z_] + y_                        /; NumericQ[y] && ACDev`LexiLessEq[{0},xs] := BigO[AsyScale[xs,z],z]  

(******** Multiplication ********)
BigO /: BigO[AsyScale[xs_,z_],z_] * BigO[AsyScale[ys_,z_],z_]      := BigO[AsyScale[xs,z]*AsyScale[ys,z],z]
BigO /: AsyScale[xs_,z_] * BigO[AsyScale[ys_,z_],z_]               := BigO[AsyScale[xs,z]*AsyScale[ys,z],z]
BigO /: y_ * BigO[expr_,z_]                         /; NumericQ[y] := BigO[expr,z]

(******** Exponentiation ********)
BigO /: BigO[expr_,z_]^y_ := BigO[expr^y,z]

(******** Distributivity ********)
BigO[terms__Plus,z_]:=(BigO[#,z]&/@terms);

(******************************************************************************************** DROP BIGO ERRORS ********)

(******** No errors ********)
DropErrorTerms[y_]                   /; NumericQ[y] := y;
DropErrorTerms[y_. AsyScale[xs_,z_]]                :=y AsyScale[xs,z];

(******** Errors ********)
DropErrorTerms[BigO[ AsyScale[xs_,z_],z_]]               := 0
DropErrorTerms[BigO[y_,z_]]                /;NumericQ[y] := 0

(******** Distributivity ********)
DropErrorTerms[expr_Plus] := DropErrorTerms /@ expr;

(******************************************************************************************** RETRIEVE BIGO ERRORS ********)

(******** No errors ********)
GetErrorTerms[y_]                   /; NumericQ[y] := 0;
GetErrorTerms[y_. AsyScale[xs_,z_]]                := 0;

(******** Errors ********)
GetErrorTerms[BigO[ AsyScale[xs_,z_],z_]]                := AsyScale[xs,z]
GetErrorTerms[BigO[y_,z_]]                /; NumericQ[y] := y

(******** Distributivity ********)
GetErrorTerms[expr_Plus] := GetErrorTerms /@ expr;

(******************************************************************************************** ASYSCALE ACCESS METHODS COMMUTE WITH BIGO ********)

GetExponents[BigO[AsyScale[x_,z_],z_]]                    :=x
GetExponents[BigO[y_,z_]]                  /; NumericQ[y] :={0}

GetVariable[BigO[AsyScale[x_,z_],z_]]                     := z
GetVariable[BigO[y_,z_]]                   /; NumericQ[y] := z

(******************************************************************************************** LEADING ORDER EXTRACTION ********)

(******** Action over single terms ********)
DropCorrections[y_, h_]                 /; NumericQ[y] := y;
DropCorrections[y_. expr_AsyScale, h_]                 := y expr; (* this works even if the variable of expr is not h! *)
DropCorrections[BigO[expr_,z_], h_]                        := BigO[expr,z] (* this works even if the variable of expr is not h! *)


(******** Action over two terms in the correct variable ********)
DropCorrections[y_. AsyScale[xs_,h_] + z_. AsyScale[ws_,h_], h_] /;  ACDev`LexiLessEq[ws,xs]:= 
	y AsyScale[xs,h] + BigO[AsyScale[ws,h],h]; 
DropCorrections[y_. AsyScale[xs_,h_] + z_, h_]                   /;  NumericQ[z] && ACDev`LexiLessEq[{0},xs]:= 
	y AsyScale[xs,h] + BigO[1,h]; 
DropCorrections[y_. AsyScale[xs_,h_] + z_, h_]                   /;  NumericQ[z] && ACDev`LexiLessEq[xs,{0}]:= 
	BigO[AsyScale[xs,h],h] + z; 
DropCorrections[y_. AsyScale[xs_,h_] + BigO[AsyScale[ys_,h_],h_],h_] :=
	y AsyScale[xs,h] + BigO[AsyScale[ys,h],h]; 
DropCorrections[y_. AsyScale[xs_,h_] + BigO[1,h_], h_]:= 
	y AsyScale[xs,h] + BigO[1,h]; 

(******** Action over more terms ********)
DropCorrections[expr_Plus,h_] /; Length@expr >= 3 && And@@((GetVariable[#]==h || GetVariable[#]==NoVariable)& /@ (List@@expr)) :=
	Fold[ DropCorrections[ #1 + #2 , h]&, expr]

DropCorrections[expr_Plus,h_] := Module[
	{hvar, other},
	hvar = Select[List@@expr, NumericQ[#] || GetVariable[#]==h &];
	other = Complement[List@@expr, hvar];
	Plus@@other + DropCorrections[Plus@@hvar,h]
]

LeadingOrder[expr_,z_]:=DropCorrections[expr,z]//DropErrorTerms

(******************************************************************************************** CUSTOM SIMPLIFY ********)

Unprotect[FullSimplify];
FullSimplify[y_. x_AsyScale]:=FullSimplify[y]x;
Protect[FullSimplify];

(******************************************************************************************** CONVERT TO CLASSIC FORM ********)

ClassicForm[expr_Plus] := ClassicForm/@expr;
ClassicForm[BigO[expr_,z_]] := BigO[ClassicForm@expr,z];
ClassicForm[y_] /; NumericQ[y] := y;
ClassicForm[y_. AsyScale[xs_,z_]] := y Times@@(#[[1]]^#[[2]]&/@(Transpose@{NestList[Log,z,Length[xs]-1],xs}));

(******************************************************************************************** CONVERT TO ASYSCALE FORM ********)

AsyForm[expr_Plus] := AsyForm/@expr;
AsyForm[BigO[expr_,z_]] := BigO[AsyForm@expr,z]
AsyForm[y_] /; NumericQ[y] := y;
AsyForm[expr:_Times] /; !NumericQ[expr] := AsyForm/@expr;
(* single term with single factor  *)
AsyForm[expr:Except[_Times|_Plus]] /; !NumericQ[expr] := 
	expr /. Reverse[#[[2]]^Optional[Pattern[Evaluate@Symbol["x"<>ToString@#[[1]]],Blank[]]]-> AsyScale[Append[Table[0,{#[[1]]-1}],Symbol["x"<>ToString[#[[1]]]]],v]&/@Transpose@({Range@Length@#,#}&@NestList[Log,v_,50])]

(******************************************************************************************** CHANGE OF VARIABLE ********)

(*ApplyChangeOfVariable[expr_Plus, ChangeOfVariable[z_, h_, ch_]] :=
*)

End[]; (************************************************************************************* End Private Context *)

EndPackage[];


(* Formatting of asyscale objects 
MakeBoxes[AsyScale[xs_,z_], _] ^:= ToBoxes[#,StandardForm]&@ Interpretation[Times@@( NestList[Log,z,Length[xs]-1]^xs ), AsyScale[xs,z]];
*)
