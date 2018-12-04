(* ::Package:: *)

(* ::Section:: *)
(*Grep*)


BeginPackage["Grep`"];


Grep::usage=
	"Call in to grep";


(* ::Subsection::Closed:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*grepOpsMap*)


grepOpsMap=
	<|
		"TrailingLines"->"A",
		"LeadingLines"->"B",
		"DirectoryAction"->"d",
		"SearchPattern"->"e",
		"ExcludedFiles"->"exclude",
		"IncludedFiles"->"include",
		"EcludedDirectories"->"exclude-dir",
		"IncludedDirectories"->"include-dir",
		"UseRegex"->"E",
		"UseFixedStrings"->"F",
		"UseBasicGrep"->"G",
		"PrintFileNames"->"H",
		"SuppressFileNames"->"h",
		"PrintLineNumbers"->"n",
		"ReturnMatchCount"->"c",
		"IgnoreBinaries"->"I",
		"IgnoreCase"->"i",
		"ReturnNotContaining"->"L",
		"ReturnContaining"->"l",
		"MaxNumberOfMatches"->"m",
		"Quiet"->"q",
		"Recursive"->"R",
		"OppositeDay"->"v"
		|>;
grepOpsExceptions=
	<|
		"IgnoreBinaries"->True
		|>;
grepTrueOps=
	AssociationMap[
		Lookup[grepOpsExceptions, #, Automatic]&,
		Keys@grepOpsMap
		];


getGrepKeyString[k_]:=
	Lookup[grepOpsMap, k];
getGrepNotKeyString[k_]:=
	If[StringLength[#]===1,
		If[UpperCaseQ[#], ToLowerCase[#], ToUpperCase[#]],
		StringReplace[#, {"exclude"->"include", "include"->"exclude"}]
		]&@
		Lookup[grepOpsMap, k]


formatGrepKey[k_]:=
	If[StringLength[k]===1, "-"<>k, "--"<>k]
formatGrepKeyPair[{k_, v_}]:=
	If[StringLength[k]===1, 
		"-"<>k<>" "<>ToString[v], 
		"--"<>k<>"="<>ToString[v]
		]


(* ::Subsubsection::Closed:: *)
(*parseGrepOps*)


parseGrepOps[ops:OptionsPattern[]]:=
	Sequence@@
		Module[
			{
				fullOps=
					FilterRules[
						Normal@Merge[{ops, Options[Grep]}, First],
						Normal@grepOpsMap
						]
				},
			Replace[
				fullOps,
				{
					(k_->Automatic):>
						Nothing,
					(k_->True):>
						formatGrepKey@getGrepKeyString[k],
					(k_->False):>
						formatGrepKey@getGrepNotKeyString[k],
					(k_->l_List):>
						Apply[Sequence,
							Map[formatGrepKeyPair, Thread[{getGrepKeyString@k, l}]]
							],
					(k_->v:_?NumericQ|_String):>
						formatGrepKeyPair[{getGrepKeyString@k, v}],
					_->Nothing
					},
				1
				]
			]


(* ::Subsubsection::Closed:: *)
(*getGrepPat*)


getGrepPat[pat_, useFixed_]:=
	If[TrueQ@useFixed&&StringQ@pat,
		pat,
		If[StringPattern`StringPatternQ[pat],
			StringTrim[
				StringPattern`PatternConvert[pat][[1]], 
				"(?ms)"
				],
			Nothing
			]
		]


(* ::Subsubsection::Closed:: *)
(*grepPostProcessor*)


nTrue=Except[True];


grepPostProcessor//Clear
grepPostProcessor[ops:OptionsPattern[]]:=
	grepPostProcessor@@
		Lookup[Flatten@{ops}, 
			{
				"ReturnContaining",
				"ReturnNotContaining",
				"ReturnMatchCount",
				"PrintFileNames",
				"SuppressFileNames",
				"PrintLineNumbers"
				},
			Automatic
			];
grepPostProcessor[_, _, True, True, _, _]:=
	Floor@Map[
		Internal`StringToDouble,
		StringSplit[#, "\n"]
		]&
grepPostProcessor[_, _, True, nTrue, _, _]:=
	Floor@Map[
		Internal`StringToDouble,
		AssociationThread@@
			Transpose@
				StringSplit[
					StringSplit[#, "\n"],
					":",
					2
					]
		]&;
grepPostProcessor[True, _, _, _, _, _]:=
	StringSplit[#, "\n"]&
grepPostProcessor[_, True, _, _, _, _]:=
	StringSplit[#, "\n"]&
grepPostProcessor[nTrue, nTrue, nTrue, _, nTrue, True]:=
	GroupBy[
		StringSplit[
			StringSplit[#, "\n"],
			":",
			3
			],
		First->(Rule@@#[[2;;]]&),
		KeyMap[Floor[Internal`StringToDouble[#]]&]@*Association
		]&;
grepPostProcessor[nTrue, nTrue, nTrue, _, nTrue, nTrue]:=
	GroupBy[
		StringSplit[
			StringSplit[#, "\n"],
			":",
			2
			],
		First->(#[[2]]&)
		]&;
grepPostProcessor[nTrue, nTrue, nTrue, _, True, nTrue]:=
	StringSplit[#, "\n"]&
grepPostProcessor[nTrue, nTrue, nTrue, _, True, True]:=
	Map[
		Floor@Internal`StringToDouble@First[#]->#[[2]]&,
		StringSplit[
			StringSplit[#, "\n"],
			":",
			2
			]
		]&;
grepPostProcessor[___]:=
	Identity;


(* ::Subsubsection::Closed:: *)
(*Grep*)


Options[Grep]=
	Join[
		Normal@grepTrueOps,
		Options[RunProcess],
		{
			"PostProcess"->True
			}
		];
Grep[files_, pat_, ops:OptionsPattern[]]:=
	Module[{res},
		res=
			RunProcess[
				{
					"grep",
					parseGrepOps[ops],
					getGrepPat[pat, OptionValue["UseFixedStrings"]],
					Sequence@@Flatten@{files}
					},
				FilterRules[{ops}, Options[RunProcess]]
				];
		If[
			StringQ@res["StandardError"]&&
				StringLength@StringTrim@res["StandardError"]>0,
			PackageThrowMessage[
				"Grep",
				StringTrim@res["StandardError"]
				]
			];
		If[OptionValue["PostProcess"], grepPostProcessor[ops], Identity]@
			StringTrim[res["StandardOutput"], "\n"~~EndOfString]
		]


(* ::Subsubsection::Closed:: *)
(*End*)


End[];


(* ::Subsection:: *)
(*EndPackage*)


EndPackage[];
