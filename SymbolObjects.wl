(* ::Package:: *)

(* ::Input:: *)
(*<<BTools`FrontEnd`*)


(* ::Input:: *)
(*MakeIndentable["IndentCharacter"->"  "]*)


(* ::Input:: *)
(*CurrentValue[EvaluationNotebook[],{TaggingRules, "IndentCharacter"}]="  ";*)


(* ::Input:: *)
(*BatchIndentationEvent["Restore", *)
(*	CellStyle->"Code"*)
(*	]*)


(* ::Input:: *)
(*CurrentValue[EvaluationNotebook[],{TaggingRules, "IndentCharacter"}]="\t";*)


(* ::Input:: *)
(*BatchIndentationEvent["Replace", *)
(*	CellStyle->"Code"*)
(*	]*)


(* ::Section:: *)
(*SymbolObjects*)


BeginPackage["SymbolObjects`"];


SObj::usage="SObj[name, templates, a] builds a new SObj
SObj[s][props...] gets properties
";


(* ::Subsubsection::Closed:: *)
(*Private Declarations*)


AppendTo[$ContextPath, $Context<>"Package`" ];


Begin["`Package`"];


(*Package Declarations*)
$SObjTemplates::usage="$SObjTemplates";
$SObjGetDecorate::usage="$SObjGetDecorate";
$SObjFormat::usage="$SObjFormat";


(*Package Declarations*)
SObjAssociationQ::usage="SObjAssociationQ[a]
SObjAssociationQ[___]";
SObjSymbolQ::usage="SObjSymbolQ[s]
SObjSymbolQ[___]";
SObjQ::usage="SObjQ[o]
SObjQ[___]";
SObjNew::usage="SObjNew[name, templates, a]";
SObjInstantiate::usage="SObjInstantiate[obj, args]";
SObjRemove::usage="SObjRemove[obj]";
SObjClear::usage="SObjClear[obj]";
SObjSymbol::usage="SObjSymbol[SObj[s], h]";
SObjKeys::usage="SObjKeys[SObj[s], pat]";
SObjMethod::usage="SObjMethod[obj, f]
SObjMethod[obj, f, a]
SObjMethod[obj, vars, f, a]
SObjMethod[...][obj]";
SObjProperty::usage="SObjProperty[...][obj]";
SObjGetDecorate::usage="SObjGetDecorate[o, res]";
SObjPart::usage="SObjPart[o, k]";
SObjExtract::usage="SObjExtract[SObj[s], k]
SObjExtract[SObj[s], k, h]";
SObjLookup::usage="SObjLookup[SObj[s], k]
SObjLookup[SObj[s], k, d]
SObjLookup[o, k]
SObjLookup[o, k, d]";
SObjSet::usage="SObjSet[SObj[s], prop, val]";
SObjSetDelayed::usage="SObjSetDelayed[SObj[s], prop, val]";
SObjSetPart::usage="SObjSetPart[SObj[s], part, val]";
SObjSetPartDelayed::usage="SObjSetPartDelayed[SObj[s], part, val]";
SObjAssociateTo::usage="SObjAssociateTo[SObj[s], parts]";
SObjUnset::usage="SObjUnset[o, key]";
SObjKeyDropFrom::usage="SObjKeyDropFrom[o, keys]";
SObjFormat::usage="SObjFormat[o]";
SObjMutationHandler::usage="SObjMutationHandler[sym[prop] = newvalue]
SObjMutationHandler[sym[prop] := newvalue]
SObjMutationHandler[sym[[p]] = newvalue]
SObjMutationHandler[sym[[p]] := newvalue]";


End[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


(* ::Subsubsection::Closed:: *)
(*Test for object validity*)


(*
	SObjAssociationQ:
		tests whether an Association represents object state
*)


SObjAssociationQ[a_Association?AssociationQ] :=
	KeyMemberQ[a, "ObjectType"];
SObjAssociationQ[___] := False;


(*
	SObjQ:
		tests whether an SObj is a real object
*)


SObjSymbolQ[s_Symbol] :=
	MemberQ[Attributes[s], Temporary] &&
		SObjAssociationQ@s;
SObjSymbolQ[___] :=
	False;
SObjSymbolQ~SetAttributes~HoldFirst;
SObjQ[o : SObj[s_Symbol]] :=
	(
		System`Private`NoEntryQ[Unevaluated@o] ||
		If[SObjSymbolQ[s],
			System`Private`SetNoEntry[Unevaluated@o];
			True,
			False
			]
		);
SObjQ[___] :=
 False


(* ::Subsubsection::Closed:: *)
(*Create new objects*)


(*
	$SObjTemplates:
		a set of templates for building SObjs
*)
If[!AssociationQ@$SObjTemplates,
	$SObjTemplates=
		<|
			"Class"->
				<|
					"New"->
						SObjMethod[SObjInstantiate]
					|>
				|>
	];


(*
	SObjNew[___]:
		construct a new SObj
*)
SObjNew//Clear
With[{c=$Context},
SObjNew[
	name : _String | Automatic : Automatic,
	templates : {___String} : {},
	a : _Association : <||>
	] :=
	With[
		{
			symbol =
			Unique[
				c<>"$Objects`" <>
				If[StringQ@name, name, "SObj"] <>
				"$"
				],
			state =
				Merge[
					{
						"ObjectID"->CreateUUID["sobj-"],
						a,
						Lookup[$SObjTemplates,
							Prepend[
								templates,
								If[StringQ@name, name, "Object"]
								], 
							{}
							],
						"ObjectType" -> 
							If[StringQ@name, name, "Object"],
						"ObjectTemplates" -> templates
						},
					First
					]
			},
		SetAttributes[symbol, Temporary];
		symbol = state;
		SObj[symbol]
		];
];
SObjNew[
	SObj[s_Symbol]
	]:=
	With[{new=SObjNew[s]},
		new/;SObjQ@s
		]


(* ::Subsubsection::Closed:: *)
(*Instantiate object*)


SObj::noinit="Instance failed to initialize, the \"ObjectInitialize\" function must return the SObj"; 
SObj::nonew="New object failed to build, the \"ObjectNew\" function must return a new SObj";
Clear[SObjInstantiate];
SObjInstantiate[o_SObj, args___]:=
	Catch@
	Module[
		{
			newO=
				SObjLookup[
					o, 
					"ObjectNew", 
					SObjNew
					][SObjSymbol[o, Identity]],
			init=
				SObjLookup[o, "ObjectInitialize", 
					(AssociateTo[#, Select[Flatten@{##}, OptionQ]];#)&
					],
			instanceProps=
				SObjLookup[o, "ObjectInstanceProperties", <||>]
			},
		If[!SObjQ@newO, Message[SObj::nonew];Throw[$Failed]];
		newO["ObjectTemplates"]=
			DeleteCases[newO["ObjectTemplates"], 
				"Class"
				];
		SObjAssociateTo[
			newO,
			instanceProps
			];
		newO=init[newO, args];
		SObjKeyDropFrom[
			newO,
			Join[
				{
					"ObjectNew",
					"ObjectInstanceProperties"
					},
				Keys@$SObjTemplates["Class"]
				]
			];
		If[!SObjQ@newO, Message[SObj::nonint];Throw[$Failed]];
		newO
		];


(* ::Subsubsection::Closed:: *)
(*Delete objects*)


SObjRemove[SObj[s_Symbol]?SObjQ]:=
	Remove@s;
SObjClear[SObj[s_Symbol]?SObjQ]:=
	Clear@s;


(* ::Subsubsection::Closed:: *)
(*Create basic structural extractors*)


(*
	SObjSymbol[o, h]:
		extracts the Symbol from o and wraps with h
*)
SObjSymbol[SObj[s_Symbol], h_: Identity] := h@s;


(*
	SObjKeys[o]:
		extracts the Keys from o
*)
SObjKeys[SObj[s_Symbol]] :=
 Keys@s


(*
	SObjValues[o]:
		extracts the Values from o
*)
SObjValues[SObj[s_Symbol]] :=
 Values@s


(* ::Subsubsection::Closed:: *)
(*Accessor decorators and methods*)


(*
	SObjMethod[___]:
		decorator to build a function over an object
*)
SObjMethod[obj_SObj, f_] :=
	Function[f[obj, ##]];
SObjMethod[obj_SObj, f_,  a_] :=
	Function[Null, f[obj, ##], a];
SObjMethod[obj_SObj, vars_, f_,  a_] :=
	Function[vars, f[obj, ##], a];
SObjMethod[f__][obj_SObj] :=
	SObjMethod[obj, f];


(*
	SObjProperty[___]:
		decorator to apply a function to an object on lookup
*)
SObjProperty[f__][obj_SObj] :=
	SObjMethod[obj, f][];


(*
	SObjGetDecorate[___]:=
		decorates Part, Extract, and Lookup calls
*)
$SObjGetDecorate = True;
SObjGetDecorate//Clear
SObjGetDecorate[o_SObj, m:_SObjMethod | _SObjProperty] :=
	m[o]
SObjGetDecorate[o_SObj, l_List] :=
	SObjGetDecorate[o, #]&/@l;
SObjGetDecorate[o_SObj, r_]:=r;(*
SObjGetDecorate[e_]:=e*)


$SObjGetWrap=True;


(*
	SObjAccess[___]:
		Basic [___] wrapper for SObj
*)
SObjAccess//Clear
SObjAccess[
	o:SObj[s_Symbol],
	k__
	]:=
	If[$SObjGetDecorate, SObjGetDecorate[o, #] &, Identity]@
		s[k]


(*
	SObjPart[___]:
		Part wrapper for SObj
*)
SObjPart//Clear
SObjPart[
	o : SObj[s_Symbol], 
	k:_Span|_List
	] :=
	If[$SObjGetWrap, SObj, Identity]@Evaluate@
		s[[k]];
SObjPart[
	o : SObj[s_Symbol], 
	k__
	] :=
If[$SObjGetDecorate, SObjGetDecorate[o, #] &, Identity]@
	s[[k]];


(*
	SObjExtract[___]:
		Extract wrapper for SObj
*)
SObjExtract//Clear
SObjExtract[
	o:SObj[s_Symbol], 
	k:{_List}|Except[_List]
	] :=
	If[$SObjGetDecorate, SObjGetDecorate[o, #] &, Identity]@
		Extract[s, k];
SObjExtract[
	o:SObj[s_Symbol], 
	k_
	] :=
	If[$SObjGetWrap, SObj, Identity]@Evaluate@
		Extract[s, k];
SObjExtract[
	o:SObj[s_Symbol], 
	k_,
	h_
	] :=
	If[$SObjGetDecorate, SObjGetDecorate[o, #] &, Identity]@
		Extract[s, k, h];


(*
	SObjLookup[___]:
		Lookup wrapper for SObj
*)
SObjLookup[
	o:SObj[s_Symbol], 
	k_
	] :=
If[$SObjGetDecorate, SObjGetDecorate[o, #] &, Identity]@
	Lookup[s, k];
SObjLookup[
	o:SObj[s_Symbol], 
	k_,
	d_
	] :=
If[$SObjGetDecorate, SObjGetDecorate[o, #] &, Identity]@
	Lookup[s, k, d];
SObjLookup[
	o : {__SObj},
	k_
	] :=
If[$SObjGetDecorate, SObjGetDecorate[o, #] &, Identity]@
	Lookup[SObjSymbol /@ o, k];
SObjLookup[
	o : {__SObj},
	k_,
	d_
	] :=
If[$SObjGetDecorate, SObjGetDecorate[o, #] &, Identity]@
	Lookup[SObjSymbol /@ o, k, d];
SetAttributes[SObjLookup, HoldRest]


(*
	SObjAccessWrapper[___]:
		General purpose accessor wrapper
*)
SObjAccessWrapper[o:SObj[s_Symbol], 
	fn_, 
	k___
	]:=
	If[$SObjGetWrap, SObj, Identity]@Evaluate@
		fn[s, k];


(* ::Subsubsection::Closed:: *)
(*Define property setters*)


(*
	SObjSet[___]:
		Set wrapper for SObj
*)
SObjSet[SObj[s_], prop_, val_] :=
	s[prop] = val;


(*
	SObjSetDelayed[___]:
		SetDelayed wrapper for SObj
*)
SObjSetDelayed[SObj[s_], prop_, val_] :=
		s[prop] := val;
SObjSetDelayed~SetAttributes~HoldRest


(*
	SObjSetPart[___]:
		Set Part wrapper for SObj
*)
SObjSetPart[SObj[s_], part__, val_] :=
	s[[part]] = val;


(*
	SObjSetPartDelayed[___]:
		SetDelayed Part wrapper for SObj
*)
SObjSetPartDelayed[SObj[s_], part__, val_] :=
	s[[part]] := val;
SObjSetDelayed~SetAttributes~HoldRest


(*
	SObjAssociateTo[___]:
		AssociateTo wrapper for SObj
*)
SObjAssociateTo[SObj[s_], parts_] :=
	(AssociateTo[s, parts];);


SObjPropMutator[func_][SObj[s_], prop__, val_]:=
	(func[s[prop], val];);
SObjPartMutator[func_][SObj[s_], part__, val_]:=
	(func[s[[part]], val];);


(* ::Subsubsection::Closed:: *)
(*Define property clearers*)


(*
	SObjUnset[___]:
		Unset wrapper for SObj
*)
SObjUnset[SObj[s_], prop_] :=
	s[prop] =.;


(*
	SObjKeyDropFrom[___]:
		KeyDropFrom wrapper for SObj
*)
SObjKeyDropFrom[SObj[s_], keys_] :=
	KeyDropFrom[s, keys]


(* ::Subsubsection::Closed:: *)
(*Define Format*)


(*
	SObjFormat[___]:
		Formatting wrapper for SObj
*)
$SObjFormat = True;
SObjFormat[o : SObj[s_]] :=
 Block[
	{
		$SObjGetWrap = False,
		$SObjGetDecorate = False,
		$SObjFormat = False
		},
	Module[
		{
			head = SObjLookup[o, "DisplayHead", "SObj"],
			icon = SObjLookup[o, "DisplayIcon", None],
			items = SObjLookup[o, "DisplaySummaryItems", {"ObjectType", "ObjectTemplates"}],
			toggle = SObjLookup[o, "DisplayToggleItems", {}],
			id = SObjLookup[o, "ObjectID", "\[SadSmiley]"]
			},
		RawBoxes@
			TemplateBox[
				{
					BoxForm`ArrangeSummaryBox[
						head,
						o,
						icon,
						KeyValueMap[
							BoxForm`MakeSummaryItem[{Row@{#, ": "}, #2}, StandardForm] &,
							SObjPart[o, items]
							],
						KeyValueMap[
							BoxForm`MakeSummaryItem[{Row@{#, ": "}, #2}, StandardForm] &,
							SObjPart[o, toggle]
							],
						StandardForm
						],
					ToBoxes[id]
					},
				"SObj",
				InterpretationFunction ->
				Function[#],
				DisplayFunction ->
				Function[TooltipBox[#, #2]]
				]
		]
	]


(* ::Subsubsection::Closed:: *)
(*Object constructor*)


SObj//Clear


(*
	SObj[_]:
		construct a new SObj
*)
SObj[
	name : _String | Automatic : Automatic,
	templates : {___String} : {},
	a : _Association : <||>
	] :=
	SObjNew[name, templates, a];
o:SObj[s_Symbol]/;(
	System`Private`EntryQ[Unevaluated@o]&&
		SObjSymbolQ[s]
	):=
	(System`Private`SetNoEntry[Unevaluated@o];o)


(* ::Subsubsection::Closed:: *)
(*Property access*)


(*
	SObj[_][___]:
		access properties
*)
(o : SObj[s_Symbol]?SObjQ)[
	i__
	] :=
	(SObjAccess[o, i]);
SObj/:Lookup[SObj[s_Symbol]?SObjQ, i__] :=
	(SObjLookup[s, i]);
SObj/:Part[o:SObj[s_], p___]:=
	(SObjPart[o, p]/;SObjQ@o);
SObj/:Extract[o:SObj[s_], e___]:=
	(SObjExtract[o, e]/;SObjQ@o);


(* ::Subsubsection:: *)
(*Property setting*)


(*
	SObj[_][___] = :
		set properties on the SObj
*)
SetAttributes[SObjMutationHandler, HoldAllComplete];
SObj /: Set[o_SObj?SObjQ[prop_], newvalue_] :=
	SObjSet[o, prop, newvalue];
SObjMutationHandler[
	Set[(sym : (_SObj | _Symbol)?SObjQ)[prop_], newvalue_]
	] := SObjSet[sym, prop, newvalue];
SObj /: SetDelayed[o_SObj?SObjQ[prop_], newvalue_] :=
	SObjSetDelayed[o, prop, newvalue];
SObjMutationHandler[
	Unset[(sym:(_SObj|_Symbol)?SObjQ), stuff_]
	] := SObjUnset[sym, stuff];
SObj /: Unset[o_SObj?SObjQ[prop_]] :=
	SObjUnset[o, prop];
SObjMutationHandler[
	SetDelayed[(sym : (_SObj | _Symbol)?SObjQ)[prop_], newvalue_]
	] := SObjSetDelayed[sym, prop, newvalue];
SObjMutationHandler[
	Set[Part[(sym : (_SObj | _Symbol)?SObjQ), p__], newvalue_]
	] := SObjSetPart[sym, p, newvalue];
SObjMutationHandler[
	SetDelayed[Part[(sym : (_SObj | _Symbol)?SObjQ), p__], newvalue_]
	] := SObjSetPartDelayed[sym, p, newvalue];
SObjMutationHandler[
	AssociateTo[(sym:(_SObj|_Symbol)?SObjQ),stuff_]
	] := SObjAssociateTo[sym, stuff];
SObjMutationHandler[
	KeyDropFrom[(sym:(_SObj|_Symbol)?SObjQ), stuff_]
	] := SObjKeyDropFrom[sym, stuff];


Map[
	Function[
		SObjMutationHandler[
			#[(sym:(_SObj|_Symbol)?SObjQ)[prop__], stuff_]
			] := SObjPropMutator[#][sym, prop, stuff];
		SObjMutationHandler[
			#[(sym:(_SObj|_Symbol)?SObjQ)[[part__]], stuff_]
			] := SObjPartMutator[#][sym, part, stuff];
		],
	{
		AddTo, SubtractFrom, TimesBy, DivideBy,
		AppendTo, PrependTo,
		AssociateTo, KeyDropFrom
		}
	];
Map[
	Function[
		SObjMutationHandler[
			#[(sym:(_SObj|_Symbol)?SObjQ)[prop__]]
			] := SObjPropMutator[#][sym, prop];
		SObjMutationHandler[
			#[(sym:(_SObj|_Symbol)?SObjQ)[[part__]]]
			] := SObjPartMutator[#][sym, part];
		],
	{
		Increment, Decrement
		}
	];


Language`SetMutationHandler[SObj, SObjMutationHandler];


(* ::Subsubsection::Closed:: *)
(*UpValues interface*)


(*
	Various UpValues
*)
SObj /: Keys[o_SObj?SObjQ] := SObjKeys@o;
SObj /: Values[o_SObj?SObjQ] := SObjValues@o;
SObj /: Normal[o_SObj?SObjQ] := SObjSymbol@o;
SObj /: Dataset[o_SObj?SObjQ] := SObjSymbol[o, Dataset];
Map[
	Function[
		SObj /: #[o_SObj?SObjQ] :=
			Block[{$SObjGetWrap=False}, SObjAccessWrapper[o, #]];
		],
	{
		First, Last, Length
		}
	];
Map[
	Function[
		SObj /: #[o_SObj?SObjQ, a__] :=
			Block[{$SObjGetWrap=False}, SObjAccessWrapper[o, #, a]];
		],
	{
		KeyExistsQ, KeyMemberQ
		}
	];
Map[
	Function[
		SObj /: #[o_SObj?SObjQ, a__] :=
			SObjAccessWrapper[o, #, a];
		],
	{
		Most, Rest, Take, 
		Select, Cases, Pick,
		Drop, Delete, DeleteCases,
		Insert, 
		KeyTake, KeySelect, KeyDrop
		}
	];


(* ::Subsubsection::Closed:: *)
(*Format*)


(* 
	Format
*)


Format[o_SObj?SObjQ, StandardForm] :=
 SObjFormat[o] /; TrueQ@$SObjFormat


(* ::Subsubsection::Closed:: *)
(*Attributes*)


(* 
	Handle the necessary HoldFirst
*)
SObj~SetAttributes~HoldFirst


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
