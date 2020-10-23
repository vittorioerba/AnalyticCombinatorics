(* ::Package:: *)

VerificationTest[True,True]


(* ::Title::Closed:: *)
(*AsyScale*)


(* ::Section::Closed:: *)
(*Basic simplification rules*)


VerificationTest[
	4 AsyScale[{1,0,0},z]
	,
	4 AsyScale[{1},z]
]


VerificationTest[
	- AsyScale[{0,0,0},z]
	,
	- 1
]


(* ::Section::Closed:: *)
(*Addition*)


VerificationTest[
	3 AsyScale[{1},z]+AsyScale[{1},z]
	,
	4 AsyScale[{1},z]
]


VerificationTest[
	AsyScale[{1},z]+ 4 AsyScale[{1},z]
	,
	5 AsyScale[{1},z]
]


VerificationTest[
	AsyScale[{1,0,0},z]- AsyScale[{1},z]
	,
	0
]


VerificationTest[
	AsyScale[{1},z]- AsyScale[{1},h]
	,
	AsyScale[{1},z]- AsyScale[{1},h]
]


(* ::Section::Closed:: *)
(*Multiplication*)


VerificationTest[
	AsyScale[{1,1},z] * AsyScale[{1},z]
	,
	AsyScale[{2,1},z]
]


VerificationTest[
	AsyScale[{12},z] * AsyScale[{1},h]
	,
	AsyScale[{12},z] * AsyScale[{1},h]
]


VerificationTest[
	AsyScale[{1,0},z] * AsyScale[{0,Pi},z]
	,
	AsyScale[{1,Pi},z]
]


(* ::Section::Closed:: *)
(*Exponentiation*)


VerificationTest[
	AsyScale[{a,b,c},z]^Pi
	,
	AsyScale[Pi{a,b,c},z]
]


(* ::Title::Closed:: *)
(*Access methods for AsyScale*)


(* ::Section::Closed:: *)
(*DropConstant*)


VerificationTest[
	DropConstant[10 AsyScale[{0,0,1},z]]
	,
	AsyScale[{0,0,1},z]
]


VerificationTest[
	DropConstant[10 AsyScale[{0,0,0},z]]
	,
	1
]


VerificationTest[
	DropConstant[0 AsyScale[{0,0,0},z]]
	,
	0
]


(* ::Section::Closed:: *)
(*GetConstant*)


VerificationTest[
	GetConstant[10 AsyScale[{0,0,1},z]]
	,
	10
]


VerificationTest[
	GetConstant[10 AsyScale[{0,0,0},z]]
	,
	10
]


VerificationTest[
	GetConstant[0 AsyScale[{0,0,0},z]]
	,
	0
]


(* ::Section::Closed:: *)
(*GetExponents*)


VerificationTest[
	GetExponents[10 AsyScale[{0,0,1},z]]
	,
	{0,0,1}
]


VerificationTest[
	GetExponents[10 AsyScale[{0,0,0},z]]
	,
	{0}
]


VerificationTest[
	GetExponents[0 AsyScale[{0,0,0},z]]
	,
	{0}
]


(* ::Section::Closed:: *)
(*Get Variable*)


VerificationTest[
	GetVariable[10 AsyScale[{0,0,1},z]]
	,
	z
]


VerificationTest[
	GetVariable[10]
	,
	NoVariable
]


(* ::Title::Closed:: *)
(*BigO*)


(* ::Section::Closed:: *)
(*Constants*)


VerificationTest[
	BigO[5 AsyScale[{1,2},z],z]
,
	BigO[ AsyScale[{1,2},z],z]
]


VerificationTest[
	BigO[5,z]
,
	BigO[1,z]
]


(* ::Section::Closed:: *)
(*Composition*)


VerificationTest[
	BigO[Pi BigO[ 12 AsyScale[{1,2},z],z],z]
,
	BigO[ AsyScale[{1,2},z],z]
]


(* ::Section::Closed:: *)
(*Addition*)


VerificationTest[
	BigO[AsyScale[{1},h],h]+BigO[AsyScale[{1,2},z],z]
	,
	BigO[AsyScale[{1},h],h]+BigO[AsyScale[{1,2},z],z]
]


VerificationTest[
	BigO[AsyScale[{1},z],z]+BigO[AsyScale[{1,2},z],z]
	,
	BigO[AsyScale[{1,2},z],z]
]


VerificationTest[
	78 + BigO[AsyScale[{1,2},h],h]
	,
	BigO[AsyScale[{1,2},h],h]
]


VerificationTest[
	BigO[78,h] + BigO[AsyScale[{1,2},h],h]
	,
	BigO[AsyScale[{1,2},h],h]
]


VerificationTest[
	BigO[78,h] + BigO[AsyScale[{-1,2},h],h]
	,
	BigO[1,h]
]


VerificationTest[
	BigO[AsyScale[{1},h],h] + AsyScale[{1,2},h]
	,
	BigO[AsyScale[{1},h],h] + AsyScale[{1,2},h]
]


VerificationTest[
	AsyScale[{1},h] + BigO[AsyScale[{1,2},h],h]
	,
	BigO[AsyScale[{1,2},h],h]
]


VerificationTest[
	78 + BigO[AsyScale[{-1,2},h],h]
	,
	78 + BigO[AsyScale[{-1,2},h],h]
]


(* ::Section::Closed:: *)
(*Multiplication*)


VerificationTest[
	BigO[AsyScale[{1},h],h] * BigO[AsyScale[{1,2},h],h]
	,
	BigO[AsyScale[{2,2},h],h]
]


VerificationTest[
	BigO[AsyScale[{1},h],h] * BigO[AsyScale[{1,2},z],z]
	,
	BigO[AsyScale[{1},h],h] * BigO[AsyScale[{1,2},z],z]
]


VerificationTest[
	AsyScale[{1},h] * BigO[AsyScale[{1,2},h],h]
	,
	BigO[AsyScale[{2,2},h],h]
]


VerificationTest[
	5 BigO[AsyScale[{1,2},h],h]
	,
	BigO[AsyScale[{1,2},h],h]
]


(* ::Section::Closed:: *)
(*Exponentiation*)


VerificationTest[
	BigO[AsyScale[{1},h],h]^Pi
	,
	BigO[AsyScale[{Pi},h],h]
]


(* ::Section::Closed:: *)
(*Distributivity*)


VerificationTest[
	BigO[AsyScale[{1},h]  +  AsyScale[{0,7,2},h],h]
	,
	BigO[AsyScale[{1},h],h]
]


(* ::Section::Closed:: *)
(*GetExponents*)


VerificationTest[
	GetExponents[BigO[AsyScale[{0,0,1},z],z]]
	,
	{0,0,1}
]


VerificationTest[
	GetExponents[BigO[1,z]]
	,
	{0}
]


(* ::Section::Closed:: *)
(*GetVariable*)


VerificationTest[
	GetVariable[BigO[AsyScale[{0,0,1},z],z]]
	,
	z
]


VerificationTest[
	GetVariable[BigO[10,z]]
	,
	z
]


(* ::Title::Closed:: *)
(*DropErrorTerms*)


(* ::Section::Closed:: *)
(*No errors*)


VerificationTest[
	DropErrorTerms[10]
	,
	10
]


VerificationTest[
	DropErrorTerms[10 AsyScale[{0,0,1},z]]
	,
	10 AsyScale[{0,0,1},z]
]


(* ::Section::Closed:: *)
(*With errors*)


VerificationTest[
	DropErrorTerms[BigO[10,z]]
	,
	0
]


VerificationTest[
	DropErrorTerms[10 BigO[AsyScale[{0,0,1},z],z]]
	,
	0
]


(* ::Section::Closed:: *)
(*Distributivity*)


VerificationTest[
	DropErrorTerms[ AsyScale[{1},z]+ 10 BigO[AsyScale[{0,0,1},z],z]]
	,
	AsyScale[{1},z]
]


VerificationTest[
	DropErrorTerms[ AsyScale[{1},z]+ 10 BigO[AsyScale[{0,0,1},z],z] + Pi BigO[AsyScale[{10},h],h]]
	,
	AsyScale[{1},z]
]


(* ::Title::Closed:: *)
(*GetErrorTerms*)


(* ::Section::Closed:: *)
(*No errors*)


VerificationTest[
	GetErrorTerms[10]
	,
	0
]


VerificationTest[
	GetErrorTerms[10 AsyScale[{0,0,1},z]]
	,
	0
]


(* ::Section:: *)
(*With errors*)


VerificationTest[
	GetErrorTerms[BigO[10,z]]
	,
	1
]


VerificationTest[
	GetErrorTerms[10 BigO[AsyScale[{0,0,1},z],z]]
	,
	AsyScale[{0,0,1},z]
]


(* ::Section:: *)
(*Distributivity*)


VerificationTest[
	GetErrorTerms[ AsyScale[{1},z]+ 10 BigO[AsyScale[{0,0,1},z],z]]
	,
	AsyScale[{0,0,1},z]
]


VerificationTest[
	GetErrorTerms[ BigO[AsyScale[{1},h],h]+ 10 BigO[AsyScale[{0,0,1},z],z]]
	,
	AsyScale[{1},h] + AsyScale[{0,0,1},z]
]


VerificationTest[
	GetErrorTerms[ BigO[AsyScale[{1},h],h]+ 10 BigO[AsyScale[{0,0,1},z],z]]
	,
	AsyScale[{1},h] + AsyScale[{0,0,1},z]
]


(* ::Title:: *)
(*DropCorrections*)


(* ::Section:: *)
(*Single term action*)


VerificationTest[
	DropCorrections[ 3 AsyScale[{1},z] ,z]
	,
	3 AsyScale[{1},z]
]
VerificationTest[
	DropCorrections[ 3 AsyScale[{1},z] ,h]
	,
	3 AsyScale[{1},z]
]
VerificationTest[
	DropCorrections[ BigO[AsyScale[{1},z],z] ,z]
	,
	BigO[AsyScale[{1},z],z]
]
VerificationTest[
	DropCorrections[ 10 ,h]
	,
	10
]


(* ::Section:: *)
(*Multiple term action*)


VerificationTest[
	DropCorrections[ 3 AsyScale[{1},z] +7+ AsyScale[{1/2,1},z] + BigO[AsyScale[ {0,0,1},z] ,z],z]
	,
	3 AsyScale[{1},z] + BigO[AsyScale[{1/2,1},z],z]
]
VerificationTest[
	DropCorrections[ 3 AsyScale[{1},h] + AsyScale[{1/2,1},z] + BigO[AsyScale[ {0,0,1},z] ,z],z]
	,
	3 AsyScale[{1},h] + AsyScale[{1/2,1},z]+ BigO[AsyScale[{0,0,1},z],z]
]
VerificationTest[
	DropCorrections[ 3 AsyScale[{1},z] + 7+ AsyScale[{-1/2,1},z] + BigO[AsyScale[ {-1/3,0,1},z],z] ,z]
	,
	3 AsyScale[{1},z] + BigO[1,z]
]
VerificationTest[
	DropCorrections[ 3 AsyScale[{1},h] + AsyScale[{-1/2,1},z] + BigO[AsyScale[ {0,0,1},z],z] ,z]
	,
	3 AsyScale[{1},h] + BigO[AsyScale[{0,0,1},z],z]
]


(* ::Section:: *)
(*LeadingOrder*)


VerificationTest[
	LeadingOrder[ 3 AsyScale[{1},z] + AsyScale[{-1/2,1},z] + BigO[AsyScale[ {0,0,1},z],z] ,z]
	,
	3 AsyScale[{1},z] 
]


VerificationTest[
	LeadingOrder[ 3 AsyScale[{1},h] + AsyScale[{-1/2,1},z] + BigO[AsyScale[ {0,0,1},z],z] ,z]
	,
	3 AsyScale[{1},h] 
]


(* ::Title:: *)
(*FullSimplify*)


VerificationTest[
	 (1/3-EulerGamma-Log[2]-1/2 PolyGamma[0,5/2])AsyScale[{1},z]//FullSimplify
	,
	(-1-EulerGamma/2) AsyScale[{1},z]
]


(* ::Title:: *)
(*ClassicForm*)


VerificationTest[
	AsyScale[{1,2,0,0},k]+AsyScale[{Pi,3,0,0,4},l]+5 // ClassicForm
	,
	k Log[k]^2 + l^Pi Log[l]^3 (Log@Log@Log@Log@l)^4 + 5
]


VerificationTest[
	AsyScale[{1,0,3},z] + BigO[AsyScale[{1/2},z],z] // ClassicForm
	,
	z Log[Log[z]]^3 + BigO[ Sqrt@z ,z]
]


(* ::Title:: *)
(*AsyForm*)


VerificationTest[
	k Log[k]^2 + l^Pi Log[l]^3 (Log@Log@Log@Log@l)^4 + 5 // AsyForm
	,
	AsyScale[{1,2,0,0},k]+AsyScale[{Pi,3,0,0,4},l]+5 
]


VerificationTest[
	z Log[Log[z]]^3 + BigO[ Sqrt@z ,z] // AsyForm
	,
	AsyScale[{1,0,3},z] + BigO[AsyScale[{1/2},z],z]
]
