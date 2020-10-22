(* 
    Thanks to
    -   https://github.com/szhorvat/MaTeX/blob/master/MaTeX/Kernel/init.m
    for the structure of this init file
*)


(* Unprotect package symbols in case package is double-loaded *)
Unprotect["AnalyticCombinatorics`*"];

(* Load the package *)
Get["AnalyticCombinatorics`AnalyticCombinatorics`"]

(* Protect all package symbols *)
SetAttributes[
  Evaluate@Flatten[Names /@ {"AnalyticCombinatorics`*"}],
  {Protected, ReadProtected}
]
