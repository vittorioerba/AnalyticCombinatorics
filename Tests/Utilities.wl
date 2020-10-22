(* ::Package:: *)

VerificationTest[
	ACDev`PadLists[{1}]
	,
	{1}
]


VerificationTest[
	ACDev`PadLists[{2,3,4},{"a"}]
	,
	{{2,3,4},{"a",0,0}}
]


VerificationTest[
	ACDev`LexiLess[{1,2},{0,0,15}]
	,
	False
]


VerificationTest[
	ACDev`LexiLess[{0,0},{1}]
	,
	True
]


VerificationTest[
	ACDev`LexiLess[{1,0},{1}]
	,
	False
]


VerificationTest[
	ACDev`LexiLessEq[{1,2},{0,0,15}]
	,
	False
]


VerificationTest[
	ACDev`LexiLessEq[{0,0},{1}]
	,
	True
]


VerificationTest[
	ACDev`LexiLessEq[{1,0},{1}]
	,
	True
]


VerificationTest[
	NonNegativeIntegerQ[Pi]
	,
	False
]


VerificationTest[
	NonNegativeIntegerQ[GoldenRatio-1/GoldenRatio]
	,
	True
]


VerificationTest[
	NonNegativeIntegerQ[-1]
	,
	False
]


VerificationTest[
	ACDev`OrdersArray[4,2,5]
	,
	{{4,2},{4,1},{4,0},{3,2},{3,1}}
]
