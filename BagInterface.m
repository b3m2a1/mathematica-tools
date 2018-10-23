(* ::Package:: *)



BeginPackage["BagInterface`"];


ExprBag::usage=
  "Top-level interface to Bag";


BeginPackage["`Package`"];


ConstructExprBag::usage="";


ExprBagQ::usage="";


BagAppendTo::usage="";
BagLength::usage="";
BagPart::usage="";


BagSetPart::usage="";
BagMap::usage="";
BagCopy::usage="";
BagToList::usage="";
BagExtend::usage="";
BagTake::usage="";


EndPackage[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Object*)



ExprBag//Clear


(* ::Subsubsection::Closed:: *)
(*BagQ*)



ExprBagQ//Clear


ExprBagQ[e_ExprBag?System`Private`HoldNoEntryQ]:=True;
ExprBagQ[e_]:=False


(* ::Subsubsection::Closed:: *)
(*Constructor*)



ExprBag[bag_Internal`Bag]?System`Private`HoldEntryQ:=
  ConstructExprBag[bag];
ExprBag[e_List]:=
  With[{res=ConstructExprBag[e]},
    res/;ExprBagQ[res]
    ];
ExprBag[]:=
  ConstructExprBag@Internal`Bag[];


ConstructExprBag[bag_Internal`Bag]:=
  System`Private`HoldSetNoEntry[ExprBag[bag]];
ConstructExprBag[e_List]:=
  ConstructExprBag@Internal`Bag[e]


(* ::Subsubsection::Closed:: *)
(*Format*)



Format[e_ExprBag?ExprBagQ]:=
  RawBoxes@
    BoxForm`ArrangeSummaryBox[
      ExprBag,
      e,
      None,
      {
        BoxForm`MakeSummaryItem[{"Items: ", Length@e}, StandardForm]
        },
      {},
      StandardForm
      ]


(* ::Subsection:: *)
(*Package Functions*)



(* ::Subsubsection::Closed:: *)
(*BagAppendTo*)



BagAppendTo[bag_Internal`Bag, o_]:=
  Internal`StuffBag[bag, o];
BagAppendTo[e:ExprBag[bag_Internal`Bag], o_]:=
  (BagAppendTo[bag, o];e)


(* ::Subsubsection::Closed:: *)
(*BagLength*)



BagLength[bag_Internal`Bag]:=
  Internal`BagLength[bag];
BagLength[ExprBag[bag_Internal`Bag]]:=
  BagLength[bag]


(* ::Subsubsection::Closed:: *)
(*BagPart*)



BagPart[bag_Internal`Bag, p_]:=
  Internal`BagPart[bag, p];
BagPart[ExprBag[bag_Internal`Bag], p_]:=
  BagPart[bag, p]


(* ::Subsubsection::Closed:: *)
(*BagSetPart*)



BagSetPart[bag_Internal`Bag, p_, v_]:=
  Set[Internal`BagPart[bag, p], v];
BagSetPart[ExprBag[bag_Internal`Bag], p_, v_]:=
  BagSetPart[bag, p, v]


(* ::Subsubsection::Closed:: *)
(*BagCopy*)



BagCopy[bag_Internal`Bag]:=
  Internal`Bag@
    Internal`BagPart[bag, All];
BagCopy[ExprBag[bag_Internal`Bag]]:=
  ExprBag[BagCopy[bag]];


(* ::Subsubsection::Closed:: *)
(*BagMap*)



BagMap[f_, bag_Internal`Bag]:=
  Do[
    Internal`BagPart[bag, i]=f[Internal`BagPart[bag, i]], 
    {i, Internal`BagLength[bag]}
    ];
BagMap[f_, e:ExprBag[bag_Internal`Bag]]:=
  (BagMap[f, bag];e)


(* ::Subsubsection::Closed:: *)
(*BagToList*)



BagToList[bag_Internal`Bag]:=
  Internal`BagPart[bag, All];
BagToList[ExprBag[bag_Internal`Bag]]:=
  Internal`BagPart[bag, All];


(* ::Subsubsection::Closed:: *)
(*BagExtend*)



BagExtend[bag_Internal`Bag, iterable_]:=
  Scan[Internal`StuffBag[bag, #]&, iterable];
BagExtend[e:ExprBag[bag_Internal`Bag], iterable_]:=
  (BagExtend[bag, iterable];e)


(* ::Subsubsection::Closed:: *)
(*BagTake*)



BagTake//Clear


iBagTake[bag_Internal`Bag, {i_Integer}]:=
  Internal`BagPart[bag, i];
iBagTake[bag_Internal`Bag, i_Integer]:=
  Internal`BagPart[bag, ;;i];
iBagTake[bag_Internal`Bag, {i_Integer, e_Integer}]:=
  Internal`BagPart[bag, i;;e];
iBagTake[bag_Internal`Bag, {i_Integer, e_Integer, s_Integer}]:=
  Internal`BagPart[bag, i;;e;;s];


BagTake[bag_Internal`Bag, r_]:=
  Module[
    {
      e=r/.UpTo[n_]:>Min@{n, Internal`BagLength[bag]},
      res
      },
    res=iBagTake[bag, e];
    res/;Head[res]=!=iBagTake
    ];
BagTake[ExprBag[bag_Internal`Bag], r_]:=
  With[{b=BagTake[bag, r]},
    b/;Head[b]=!=BagTake
    ]


(* ::Subsection:: *)
(*Overrides*)



(* ::Subsubsection::Closed:: *)
(*UpValues*)



ExprBag/:Length[e_ExprBag?ExprBagQ]:=
  BagLength[e];
ExprBag/:Normal[e_ExprBag?ExprBagQ]:=
  BagToList[e];
ExprBag/:Part[e_ExprBag?ExprBagQ, i_]:=
  BagPart[e, i];
ExprBag/:Take[e_ExprBag?ExprBagQ, s_]:=
  BagTake[e, s];


ExprBag/:Internal`BagPart[ExprBag[bag_Internal`Bag]?ExprBagQ, b_]:=
  Internal`BagPart[bag, b];
ExprBag/:Internal`BagLength[ExprBag[bag_Internal`Bag]?ExprBagQ]:=
  Internal`BagLength[bag]
ExprBag/:Internal`StuffBag[ExprBag[bag_Internal`Bag]?ExprBagQ, b_]:=
  Internal`StuffBag[bag, b];


(* ::Subsubsection::Closed:: *)
(*Methods*)



ExprBag/:e_ExprBag?ExprBagQ["Map"[f_]]:=
  BagMap[f];
ExprBag/:e_ExprBag?ExprBagQ["Copy"[]]:=
  BagCopy[];
ExprBag/:e_ExprBag?ExprBagQ["Extend"[iterable_]]:=
  BagCopy[];


(* ::Subsubsection::Closed:: *)
(*Mutations*)



bagSymQ[s_]:=
  MatchQ[OwnValues[s], {_:>_ExprBag?ExprBagQ}];
bagSymQ~SetAttributes~HoldFirst;


BagMutationHandler//ClearAll
BagMutationHandler~SetAttributes~HoldAllComplete;
BagMutationHandler[(Set|SetDelayed)[Part[s:_ExprBag?ExprBagQ|_Symbol?bagSymQ, p_], v_]]:=
  BagSetPart[s, p, v];
BagMutationHandler[AppendTo[s:_ExprBag?ExprBagQ|_Symbol?bagSymQ, v_]]:=
  BagAppendTo[s, v];
BagMutationHandler[___]:=
  Language`MutationFallthrough
Language`SetMutationHandler[ExprBag, BagMutationHandler];


End[];


EndPackage[];



