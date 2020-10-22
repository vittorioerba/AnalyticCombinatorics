(* 
    Thanks to
    -   https://github.com/szhorvat/MaTeX/blob/master/MaTeX/Kernel/init.m
    for the structure of this init file
*)

AnalyticCombinatorics`Private`Submodules = {"Utilities", "AsyScale", "TransferTheorem"};

AnalyticCombinatorics`Private`Contexts = # <> "`*" & /@ Append[ AnalyticCombinatorics`Private`Submodules, "ACDev"];

AnalyticCombinatorics`Private`Files = # <> ".m" & /@ AnalyticCombinatorics`Private`Submodules;

(* Unprotect package symbols in case package is double-loaded *)
Unprotect/@AnalyticCombinatorics`Private`Contexts;

(* Load the submodules *)
Get[FileNameJoin[{NotebookDirectory[], "Src", #}]]& /@ AnalyticCombinatorics`Private`Files;

(* Protect all package symbols *)
SetAttributes[
  Evaluate@Flatten[Names /@ AnalyticCombinatorics`Private`Contexts],
  {Protected, ReadProtected}
];
