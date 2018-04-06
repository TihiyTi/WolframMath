(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: MRIdata *)
(* :Context: MRIdata` *)
(* :Author: Alex *)
(* :Date: 2017-10-30 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Alex *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["MRIdata`"]
(* Exported symbols added here with SymbolName::usage *)

ValveMove::usage = "Data of valve moving by MRI data"
RbyMRIbyVolume::usage = ""
HeartContoursWithAtrial::usage = ""

Begin["`Private`"]

ValveMove[person_]:=Module[{},
  Switch[person,
    "Alex",  5,
    "Ivan",  5,
    "Artem",  5,
    "", "Alex, Ivan, Artem"
  ]
];
RbyMRIbyVolume[person_]:=Module[{},
  Switch[person,
    "Alex",  50,
    "Ivan",  50,
    "Artem", 50,
    "", "Alex, Ivan, Artem"
  ]
];

(*HeartContours[name_] := Module[{},*)
  (*Switch[name,*)
    (*"Ivan", ivan7pointContour,*)
    (*"Alex", alex7pointContour,*)
    (*"Artem", artem7pointContour,*)
    (*"all", Print["Choose one of those names: Ivan, Alex, Artem"]]];*)
(**)

HeartContoursWithAtrial[name_] := Module[{},
  Switch[name,
    "Ivan", ivan7contourWithAtrial,
    "Alex", alex7contourWithAtrial,
    "Artem", artem7contourWithAtrial,
    "all", Print["Choose one of those names: Ivan, Alex, Artem"]]];

HeartEdgeMoveWithAtrial


artem7contourWithAtrial = {{-39,-22},{-37,-27},{12,-47},{67,-42},{76,-11},{41,29},{31,32},
  {-2,35},{-31,32},{-38,0},{-39,-22}};

(*ivan7pointContour = {{-33, -8}, {-32, -24}, {11, -51}, {58, -37}, {63, -11}, {36, 27}, {27, 29}};*)
(*alex7pointContour = {{-39,12},{-40,-10},{-16,-63},{42,-63},{65,-40},{42,10},{35,13}};*)



End[] (* `Private` *)

EndPackage[]