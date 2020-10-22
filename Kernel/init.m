(* 
    Thanks to
    -   https://github.com/szhorvat/MaTeX/blob/master/MaTeX/Kernel/init.m
    for the structure of this init file
*)

(* Unprotect package symbols in case package is double-loaded *)
Unprotect@{"Utilities`*", "AsyScale`*", "TransferTheorem`*"};

(* Load the package *)
Get["AnalyticCombinatorics`Utilities`"]
Get["AnalyticCombinatorics`AsyScale`"]
Get["AnalyticCombinatorics`TransferTheorem`"]

(* Protect all package symbols *)
SetAttributes[
  Evaluate@Flatten[Names /@ {"Utilities`", "AsyScale`*", "TransferTheorem`*"}],
  {Protected, ReadProtected}
]
