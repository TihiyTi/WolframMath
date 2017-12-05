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
End[] (* `Private` *)

EndPackage[]