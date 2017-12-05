(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: DspLib *)
(* :Context: DspLib` *)
(* :Author: Aleksey *)
(* :Date: 2017-11-24 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Aleksey *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["DspLib`"]
(* Exported symbols added here with SymbolName::usage *)

FirFilter::usage = "";

Begin["`Private`"]

(* signal - исходный массим сигнала,
kernel - импульсная хар-ка фильтра, для нерекурсивного фильтра это \
коэффициенты фильтра задом наперед, посчитать можно как в семинаре и \
для рекурсивного фильтра
ret(возвращаемое значение) - массив выходного сигнала *)
FirFilter[signal_, kernel_] := Module[{size, t},
  size = Round[Length[kernel]/2];
  t = ListConvolve[kernel, signal];
  Join[Table[t[[1]], size], t, Table[t[[-1]], size]]
];

End[] (* `Private` *)

EndPackage[]