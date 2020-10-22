(* ::Package:: *)

BeginPackage["AsyScale`"];

prova2::usage = "ciao"

Begin["`Private`"]; (* Begin Private Context *)

prova2[xs___]:=ACDeveloper`PadLists[xs]

End[]; (* End Private Context *)

EndPackage[];
