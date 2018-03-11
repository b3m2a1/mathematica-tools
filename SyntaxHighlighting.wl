(* ::Package:: *)

(* ::Section:: *)
(*Syntax Highlighting*)


BeginPackage["SyntaxHighlighting`"]


SyntaxHiglightingApplyStyling::usage=
	"Applies syntax coloring to a cell or cell style";
SyntaxHiglightingClearStyling::usage=
	"Removes syntax coloring from a cell or cell style";


(* ::Subsection::Closed:: *)
(*Package Declarations*)


BeginPackage["`Package`"];


$SyntaxHighlightingStyles::usage="List of styles that can be used";
$SyntaxHiglightingPunctuation::usage="List of punctuation to color";
$SyntaxHiglightingFormattingHeads::usage="List of formatting heads to color";
$SyntaxHiglightingConstants::usage="List of constants to color";
$SyntaxHiglightingTypes::usage="List of types to color";
$SyntaxHiglightingCommands::usage="List of commands to color";
$SyntaxHiglightingFunctions::usage="List of functions to color";
$SyntaxHiglightingContextStyling::usage="List of contexts and styles to apply";
SyntaxHiglightingStylesheet::usage=
	"Uses StyleSheetTemplate to make a template for stylesheet editing";


EndPackage[];


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


Needs["StyleSheetEdit`", 
	"https://raw.githubusercontent.com/b3m2a1/mathematica-tools/master/StylesheetEdit.wl"
	]


(* ::Subsection:: *)
(*Symbol Configuration*)


(* ::Subsubsection::Closed:: *)
(*Config*)


$SyntaxHighlightingStyles={
	"CodeStyle","CommandStyle","TypeStyle",
	"FormattingHeadStyle","ConstantStyle","FunctionStyle",
	"StringStyle","CommentStyle","UndefinedSymbolStyle",
	"SystemStyle","GlobalStyle","PrivateStyle","OperatorStyle",
	"UnstyledStyle","EmphasizedSyntaxErrorStyle","ExcessArgumentStyle",
	"FormattingErrorStyle","GlobalToLocalScopeConflictStyle",
	"GraphicsCompatibilityProblemStyle","LocalScopeConflictStyle",
	"LocalVariableStyle","FunctionLocalVariableStyle","MissingArgumentStyle",
	"MissingArgumentTemplateStyle","SyntaxErrorStyle","MisspelledWordStyle",
	"NoKernelPresentStyle","PatternVariableStyle","SymbolShadowingStyle",
	"OrderOfEvaluationConflictStyle","UnknownOptionStyle",
	"UnwantedAssignmentStyle"
	};


SyntaxHiglightingStylesheet[notebookArg:None|_Notebook:None]:=
	StyleSheetTemplate[notebookArg,
		DefaultStyle[
			FrontEnd`FileName[Evaluate@{$PackageName},
				"SyntaxHighlighting.nb"]
			],
		Sequence@@$SyntaxStyles,
		Directive["Input","ColoredInput"]
		];


(* ::Subsubsection::Closed:: *)
(*Clear Stuff*)


Clear/@{
"$SyntaxHiglightingPunctuation",
"$SyntaxHiglightingFormattingHeads",
"$SyntaxHiglightingConstants",
"$SyntaxHiglightingTypes",
"$SyntaxHiglightingCommands",
"$SyntaxHiglightingFunctions",
"$SyntaxHiglightingContextStyling"};


(* ::Subsubsection::Closed:: *)
(*String*)


$SyntaxHiglightingStringSepDefault={"\""};
$SyntaxHiglightingStringSep=$SyntaxHiglightingStringSepDefault;


(* ::Subsubsection::Closed:: *)
(*Punctuation*)


$SyntaxHiglightingPunctuationDefault={
"@","//","~",";",
",","-","+",
"/","*","(",")",
"[","]","{","}",
"[[","]]","<",">",
":","::",
".","..","...",
"_","__","___",
"?","&","&&",
"~~","\[RuleDelayed]","->",":>",
"|",":=","^:=",
"/:","/;"
};
$SyntaxHiglightingPunctuation=$SyntaxHiglightingPunctuationDefault;


(* ::Subsubsection::Closed:: *)
(*FormattingHeads*)


$SyntaxHiglightingFormattingHeadsDefault={
"Graphics","Graphics3D",
"Point","Line",
"Disk","Sphere",
"Tube","Arrow"
};
$SyntaxHiglightingFormattingHeads=$SyntaxHiglightingFormattingHeadsDefault;


(* ::Subsubsection::Closed:: *)
(*Constants*)


$SyntaxHiglightingConstantsDefault={
"$Failed","False","True","$Canceled",
"All"
};
$SyntaxHiglightingConstants=$SyntaxHiglightingConstantsDefault;


(* ::Subsubsection::Closed:: *)
(*Types*)


$SyntaxHiglightingTypesDefault={
	"Entity","Quantity",
	"ChannelObject","CloudObject",
	"CloudExpression","CellObject",
	"Notebook","Cell",
	"ResourceObject","LocalObject",
	"FormObject"};
$SyntaxHiglightingTypes=$SyntaxHiglightingTypesDefault;


(* ::Subsubsection::Closed:: *)
(*Commands*)


$SyntaxHiglightingCommandsDefault={
"Return","Break","DialogReturn",
"Goto","Throw","Catch",
"Message","Print",
"Assert",
"Begin","BeginPackage",
"End","EndPackage"
};
$SyntaxHiglightingCommands=$SyntaxHiglightingCommandsDefault;


(* ::Subsubsection::Closed:: *)
(*Functions*)


$SyntaxHiglightingFunctionsDefault={
	"With","Module","Block",
	"If","Switch","Which",
	"Do","Table",
	"For","While",
	"Replace","ReplaceAll",
	"ReplaceRepeated","ReplacePart",
	"Map","MapIndexed","MapThread"
	};
$SyntaxHiglightingFunctions=$SyntaxHiglightingFunctionsDefault;


(* ::Subsubsection::Closed:: *)
(*Contexts*)


$SyntaxHiglightingContextStylingDefault={
	"System`"->"SystemStyle",
	"Global`"->"GlobalStyle",
	Automatic->"UndefinedStyle"
	};
$SyntaxHiglightingContextStyling=$SyntaxHiglightingContextStylingDefault;


(* ::Subsubsection::Closed:: *)
(*UpValues*)


Map[
	Replace[#,{
		Hold[s_]:>
			(s/:HoldPattern[(h:Set|SetDelayed)[s,v_]]/;!TrueQ@$SyntaxHiglightingApplyingSyntaxStyling:=
				Block[{$SyntaxHiglightingApplyingSyntaxStyling=True},
					h[s,v];
					Block[{$SyntaxHiglightingForceCells=False},SyntaxHiglightingApplyStyling[]];
					If[h==Set,v];
					])
		}]&,
	Thread@Hold@{
		$SyntaxHiglightingPunctuation,
		$SyntaxHiglightingFormattingHeads,
		$SyntaxHiglightingContextStyling,
		$SyntaxHiglightingTypes,
		$SyntaxHiglightingFunctions,
		$SyntaxHiglightingConstants,
		$SyntaxHiglightingCommands,
		$SyntaxHiglightingContextStyling
		}
	];


MapThread[
	Replace[#,{
		Hold[s_]:>
			(s/:HoldPattern[Unset[s]]:=Block[{$SyntaxHiglightingForceCells=False},s=#2])
		}]&,{
		Thread@Hold@{
			$SyntaxHiglightingPunctuation,
			$SyntaxHiglightingFormattingHeads,
			$SyntaxHiglightingContextStyling,
			$SyntaxHiglightingTypes,
			$SyntaxHiglightingFunctions,
			$SyntaxHiglightingConstants,
			$SyntaxHiglightingCommands,
			$SyntaxHiglightingContextStyling
			},
		{
			$SyntaxHiglightingPunctuationDefault,
			$SyntaxHiglightingFormattingHeadsDefault,
			$SyntaxHiglightingContextStylingDefault,
			$SyntaxHiglightingTypesDefault,
			$SyntaxHiglightingFunctionsDefault,
			$SyntaxHiglightingConstantsDefault,
			$SyntaxHiglightingCommandsDefault,
			$SyntaxHiglightingContextStylingDefault
			}
		}]


(* ::Subsection:: *)
(*Coloring Function*)


$SyntaxHiglightingForceCells=True;


(* ::Subsubsection::Closed:: *)
(*SetSyntaxHighlighting*)


SyntaxHiglightingSetHighlightingStyle[nb_,override:True|False:True]:=
	Block[{$SyntaxHiglightingForceCells=False},
		NotebookDelete@
			StyleSheetCells[nb,
				DefaultStyle[
					Except@
						FrontEnd`FileName[
							Evaluate@{$PackageName},
							"SyntaxHighlighting.nb"
							]
					]
				];
		StyleSheetCells[
			DefaultStyle[
				FrontEnd`FileName[Evaluate@{$PackageName},
				"SyntaxHighlighting.nb"]
				],
			True
			]
		];


(* ::Subsubsection::Closed:: *)
(*SyntaxHiglightingConfigureSyntaxColoring*)


Options[SyntaxHiglightingConfigureSyntaxColoring]={
	"Punctuation"->Automatic,
	"Commands"->Automatic,
	"Functions"->Automatic,
	"Types"->Automatic,
	"Constants"->Automatic,
	"Contexts"->Automatic
	};
SyntaxHiglightingConfigureSyntaxColoring[
	objs:Except[_?OptionQ]..,
	ops:OptionsPattern[]
	]:=
	StyleSheetEdit[objs,{
		AutoStyleWords->
			Join[
				Thread[
					Replace[
						OptionValue["Punctuation"],
							Except[_List]:>$SyntaxHiglightingPunctuation
						]->"OperatorStyle"
						],
				Thread[$SyntaxHiglightingStringSep->"StringStyle"],
				Thread[
					Replace[
						OptionValue["Commands"],
							Except[_List]:>$SyntaxHiglightingCommands
						]->"CommandStyle"
						],
				Thread[
					Replace[
						OptionValue["Functions"],
							Except[_List]:>$SyntaxHiglightingFunctions
						]->"FunctionStyle"
					],
				Thread[
					Replace[
						OptionValue["Types"],
							Except[_List]:>$SyntaxHiglightingTypes
						]->"TypeStyle"
					],
				Thread[
					Replace[
						OptionValue["Constants"],
							Except[_List]:>$SyntaxHiglightingConstants
						]->"ConstantStyle"
					]
				],
		AutoStyleOptions->{
			"UndefinedSymbolStyle"->
				"UndefinedSymbolStyle",
			"CommentStyle"->
				"CommentStyle",
			"StringStyle"->
				"StringStyle",
			"SymbolContextStyles"->
				Replace[OptionValue@"Contexts",
					Except[_List]->$SyntaxHiglightingContextStyling
					],
			"EmphasizedSyntaxErrorStyle"->
				"EmphasizedSyntaxErrorStyle",
			"ExcessArgumentStyle"->
				"ExcessArgumentStyle",
			"FormattingErrorStyle"->
				"FormattingErrorStyle",
			"FunctionLocalVariableStyle"->
				"FunctionLocalVariableStyle",
			"GlobalToLocalScopeConflictStyle"->
				"GlobalToLocalScopeConflictStyle",
			"GraphicsCompatibilityProblemStyle"->
				"GraphicsCompatibilityProblemStyle",
			"LocalScopeConflictStyle"->
				"LocalScopeConflictStyle",
			"LocalVariableStyle"->
				"LocalVariableStyle",
			"MissingArgumentStyle"->
				"MissingArgumentStyle",
			"MissingArgumentTemplateStyle"->
				"MissingArgumentTemplateStyle",
			"MisspelledWordStyle"->
				"MisspelledWordStyle",
			"NoKernelPresentStyle"->
				"NoKernelPresentStyle",
			"OrderOfEvaluationConflictStyle"->
				"OrderOfEvaluationConflictStyle",
			"PatternVariableStyle"->
				"PatternVariableStyle",
			"SymbolContextStyles"->
				"SymbolContextStyles",
			"SymbolShadowingStyle"->
				"SymbolShadowingStyle",
			"SyntaxErrorStyle"->
				"SyntaxErrorStyle",
			"UndefinedSymbolStyle"->
				"UndefinedSymbolStyle",
			"UnknownOptionStyle"->
				"UnknownOptionStyle",
			"UnwantedAssignmentStyle"->
				"UnwantedAssignmentStyle"
				}
			}];


(* ::Subsubsection::Closed:: *)
(*SyntaxHiglightingRemoveSyntaxColoring*)


SyntaxHiglightingRemoveSyntaxColoring[objs_]:=
	StyleSheetEdit[objs,
		AutoStyleWords->
			Inherited,
		AutoStyleOptions->
			{
				"UndefinedSymbolStyle"->Inherited,
				"CommentStyle"->Inherited,
				"StringStyle"->Inherited,
				"SymbolContextStyles"->Inherited,
				"EmphasizedSyntaxErrorStyle"->Inherited,
				"ExcessArgumentStyle"->Inherited,
				"FormattingErrorStyle"->Inherited,
				"FunctionLocalVariableStyle"->Inherited,
				"GlobalToLocalScopeConflictStyle"->Inherited,
				"GraphicsCompatibilityProblemStyle"->Inherited,
				"LocalScopeConflictStyle"->Inherited,
				"LocalVariableStyle"->Inherited,
				"MissingArgumentStyle"->Inherited,
				"MissingArgumentTemplateStyle"->Inherited,
				"MisspelledWordStyle"->Inherited,
				"NoKernelPresentStyle"->Inherited,
				"OrderOfEvaluationConflictStyle"->Inherited,
				"PatternVariableStyle"->Inherited,
				"SymbolContextStyles"->Inherited,
				"SymbolShadowingStyle"->Inherited,
				"SyntaxErrorStyle"->Inherited,
				"UndefinedSymbolStyle"->Inherited,
				"UnknownOptionStyle"->Inherited,
				"UnwantedAssignmentStyle"->Inherited
				}
		];


(* ::Subsubsection::Closed:: *)
(*SyntaxHiglightingApplyStyling*)


Clear@SyntaxHiglightingApplyStyling;
Options[SyntaxHiglightingApplyStyling]=
	Options@SyntaxHiglightingConfigureSyntaxColoring;
SyntaxHiglightingApplyStyling[cell:_CellObject|{__CellObject},ops:OptionsPattern[]]:=
	SyntaxHiglightingConfigureSyntaxColoring[cell,ops];
SyntaxHiglightingApplyStyling[
	nb:_NotebookObject|Automatic:Automatic,
	style_:All,
	ops:OptionsPattern[]
	]:=
	(
		SyntaxHiglightingSetHighlightingStyle[nb,$SyntaxHiglightingForceCells];
		With[{s=
			Replace[StyleSheetCells[nb,style,$SyntaxHiglightingForceCells],{
				{}:>
					If[TrueQ@$SyntaxHiglightingForceCells,
						StyleSheetCells[nb,style,$SyntaxHiglightingForceCells],
						{}
						]
				}]
			},
			SyntaxHiglightingConfigureSyntaxColoring[s,ops]
			]
		);


(* ::Subsubsection::Closed:: *)
(*SyntaxHiglightingClearStyling*)


SyntaxHiglightingClearStyling[cell:_CellObject|{__CellObject}]:=
	SyntaxHiglightingRemoveSyntaxColoring[cell];
SyntaxHiglightingClearStyling[
	nb:_NotebookObject|Automatic:Automatic,
	style_:All
	]:=
	With[{s=StyleSheetCells[nb,style]},
		If[Length@s>0,
			SyntaxHiglightingClearStyling[s]
			]
		]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];


(* ::Section:: *)
(*End Package*)


EndPackage[]
