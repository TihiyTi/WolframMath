(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Test *)
(* :Context: Test` *)
(* :Author: Aleksey *)
(* :Date: 2017-03-19 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Aleksey *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["Test`"]
(* Exported symbols added here with SymbolName::usage *)

MyPlus::usage = "Plus some values";

Begin["`Private`"]

MyPlus[x_,y_]:=x+y;

End[] (* `Private` *)

EndPackage[]