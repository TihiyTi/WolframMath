(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: IMPdata *)
(* :Context: IMPdata` *)
(* :Author: Alex *)
(* :Date: 2018-09-11 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2018 Alex *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["IMPdata`"]
(* Exported symbols added here with SymbolName::usage *)
RadialExpData::usage = "Data of valve moving by MRI data"
artemSphereMoving::usage = "Data of valve moving by MRI data"

Begin["`Private`"]

RadialExpData::boole = "Support only Artem yet";
RadialExpData[name_, time_]:= Switch[name,
  (*"Ivan", GetIvanRadial[],*)
  (*"Alex", GetAlexRadial[],*)
  "Artem", RadialExpDataArtem[time],
  _, Message[GetRadial::boole, name]; Null];
RadialExpData[name_]:= RadialExpData[name, 8];

RadialExpData::boole = "Support only Artem and Artem02 yet";
RadialExpData[name_] := Switch[name,
(*"Ivan", GetIvanRadial[],*)
(*"Alex", GetAlexRadial[],*)
  "Artem", RadialExpDataArtem[time],
  "Artem02", RadialExpDataArtem[time],
  _, Message[GetRadial::boole, name]; Null];

RadialExpDataArtem[time_]:= Association[
  "dZRad" -> N[artemRadialArrayFix[[time,2]]/1000],
  "zBase" -> {70, 67, 58, 67, 76},
  "flDZ"-> artemRadialArrayFix[[time,1]]/1000//N,
  "flBase"->79
];
RadialExpDataArtem[]:=RadialExpDataArtem[8];

(*artemRadialArray = {*)
  (*{13,  {0,0,0,-7,-3}},*)
  (*{17,  {9,1,-6,-24,-2}},*)
  (*{36,  {28,3,-13,-57,-17}},*)
  (*{63,  {34,13,15,-44,-20}},*)
  (*{78,  {62,46,80,10,2}},*)
  (*{87,  {100,71,120,45,25}},*)
  (*{95,  {120,84,138,69,42}},*)
  (*{90,  {136,84,144,82,60}}*)
(*}*)
(*artemRadialArray = {*)
  (*{13,  {0,     0*0.65,    0*0.76,    -7,   -3}},*)
  (*{17,  {9/4,   1*0.65,   -6*0.76,     -24,    -2}},*)
  (*{36,  {28/4,  3*0.65,  -13*0.76,    -57,    -17}},*)
  (*{63,  {34/4,  13*0.65,   15*0.76,   -44,    -20}},*)
  (*{78,  {62/4,  46*0.65,   80*0.76,   10,     2}},*)
  (*{87,  {100/4, 71*0.65,   120*0.76,   45,    25}},*)
  (*{95,  {120/4, 84*0.65,   138*0.76,    69,   42}},*)
  (*{100,  {136/4,  84*0.65,   144*0.76,   82,   60}}*)
(*}*)


artemRadialArray = {
  {6,  {2,    5,    10,  9,   -4}},
  {24,  {5,   10,   13,   6,    0}},
  {26,  {7,   6,    -5,   -30,  -15}},
  {39,  {10,  7,    -17,  -48,  -23}},
  {65,  {13,  21,   10,   -26,  -29}},
  {85,  {44,  49,   71,   31,   -6}},
  {105, {77,  77,   116,  72,   19}},
  {120, {116, 98,   142,  102,  35}},
  {131, {140, 109,  156,  114,  46}},
  {113, {141, 100,  147,  109,  51}},
  {112, {132, 83,   115,  81,   57}},
  {98,  {114, 59,   70,   42,   58}},
  {78,  {79,  37,   38,   20,   47}},
  {80,  {29,  17,   6,    -2,   16}},
  {73,  {17,  10,   -6,   -11,  1}},
  {72,  {0,   5,    -16,  -17,  -10}},
  {56,  {0,   0,    -20,  -18,  -9}},
  {65,  {-2,  -4,   -25,  -22,  -5}},
  {60,  {-1,  -4,   -27,  -24,  -5}},
  {43,  {6,   -2,   -24,  -19,  -4}},
  {30,  {10,  3,    -19,  -13,  2}},
  {22,  {1,   3,    -17,  -14,  -3}},
  {32,  {-4,  2,    -18,  -14,  -4}},
  {29,  {-11, 4,    -15,  -13,  -9}},
  {10,  {-13,  2,   -20,  -14,  -14}}
}

artemRadialArrayFix =  Table[ {el[[1]]*100/130, el[[2]]*{40, 55, 110, 83, 76}/{140, 109,  156,  114,  46}}, {el, artemRadialArray}];

(*artemSphereMoving = {*)
  (*{1, -1, 0,  -1.2},*)
  (*{1, -1, 0,  -1},*)
  (*{1, -1, 0,  -1.1},*)
  (*{1, -1, 0,  -1.2},*)
  (*{1, -1, 0,  -0.8},*)

  (*{3, -2, 2,  -2.7},*)
  (*{4, -3, 0,  -3.6},*)
  (*{5, -4, 0,  -4.4},*)
  (*{5, -4, 0,  -4.1},*)
  (*{5, -4, 0,  -4.2},*)

  (*{4, -3, 0,  -2.8},*)
  (*{3, -2, 0,  -1.9},*)
  (*{3, -2, 0,  -2.3},*)
  (*{3, -2, 0,  -2.6},*)
  (*{3, -2, 0,  -3},*)

  (*{1, -1, 1,  -0.5},*)
  (*{1, -1, 0,  -0.8},*)
  (*{1, -1, 0,  -0.7},*)
  (*{1, -1,-1,  -0.9},*)
  (*{1, -1, 0,  -0.9},*)

  (*{1, -1, 0,  -0.9},*)
  (*{1, -1, 0,  -1.1},*)
  (*{1, -1, 0,  -1},*)
  (*{1, -1, 1,  -1},*)
  (*{1, -1, 0,  -1.2}*)

(*}*)

artemSphereMoving = {
  {1, -0.6, 0.4, -1.2},
  {1, -0.6, 0  , -1 },
  {1, -0.6, 0.4, -1.1},
  {1, -0.8, 1  , -1},
  {1, -1  , 0.8, -1.6},

  {3.2,   -2,   1,   -3},
  {4.6,   -3,   2,   -4.2},
  {5.2,   -3.6, 1.8, -4.4},
  {5.8,   -4,   1.8, -5},
  {5.4,   -3.8, 1.2, -4.7},

  {4.8,   -3.4, 0.6, -3.9},
  {3.8,   -2.6, 0.2, -2.9},
  {2.6,   -1.6, 0.2, -1.9},
  {2,     -1.2, 0.4, -1.3},
  {1.4,   -0.8, -0.6,-1.1},

  {1.2,   -0.6, -0.4,-1},
  {1.2,   -0.6, -0.6,-1.2},
  {1.2,   -0.6, -0.4,-1},
  {1.2,   -0.6, -0.6,-1.1},
  {1.2,   -0.6, -0.4,-1.2},

  {1.2,   -0.6, -0.2,-1.2},
  {1,     -0.6, -0.4,-1},
  {1,     -0.6, -0.4,-1},
  {1,     -0.6, -0.4,-1},
  {1,     -0.6, 0.4,-1.2}
}

artemSphereMoving02mm


End[] (* `Private` *)

EndPackage[]