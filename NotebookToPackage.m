(* ::Package:: *)

(* ::Section:: *)
(*NotebookToPackage*)


BeginPackage["NotebookToPackage`"];


(*Package Declarations*)
NotebookToPackage::usage=
"NotebookToPackage[nb] turns NotebookObject nb into a package
NotebookToPackage[file] silently opens file and builds a package from it";


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*Private Declarations*)


(*Package Declarations*)
$codeExportStyles::usage=
	"$codeExportStyles are the styles exported to Code cells";
$hiddenPackageExportContext::usage=
	"$hiddenPackageExportContext is the context in which the notebook-to-package operation is performed";
makeUsageRule::usage=
	"makeUsageRule[sym, usage] creates a usage template rule for sym based on usage";
patternSanitize::usage=
	"patternSanitize[usage] cleans usage so it can be fed into a template";
ownPatternToUsage::usage=
	"ownPatternToUsage[sym :> u] creates an OwnValues usage template";
downPatternToUsage::usage=
	"downPatternToUsage[sym[args] :> u] creates a DownValues usage template";
subPatternPullHead::usage=
	"subPatternPullHead[pat] pulls the Head off pat until it's a Symbol";
subPatternToUsage::usage=
	"subPatternToUsage[subPat :> u] creates a SubValues usage template";
upPatternToUsage::usage=
	"upPatternToUsage[{sym, p} :> u] creates an UpValues usage template";
exprDefinitionToUsage::usage=
	"exprDefinitionToUsage[{sym, p} :> v] calls upPatternToUsage
exprDefinitionToUsage[own :> v] calls ownPatternToUsage
exprDefinitionToUsage[down :> v] calls downPatternToUsage
exprDefinitionToUsage[sub :> v] call subPatternToUsage";
definitionSanitize::usage=
	"definitionSanitize[expr] cleans expr to its usage may be extracted";
exprFindDefinitions::usage=
	"exprFindDefinitions[expr] finds definitions in expr";
makeUsageBoxes::usage=
	"makeUsageBoxes[name, strings] makes usage boxes for name from strings";
defsUsagesCell::usage=
	"defsUsagesCell[defList] converts defList into a usage definition cell";
cellsBuildPackageCore::usage=
	"cellsBuildPackageCore[c] formats the package core and Sows definitions";
notebookExtractPackageName::usage=
	"notebookExtractPackageName[nb] gets the package name from nb";


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


$codeExportStyles = "Code" | "Input";


$hiddenPackageExportContext = "Dooooop`Doop`Doop`Doop`";


(* ::Subsubsection::Closed:: *)
(*makeUsageRule*)


Clear[makeUsageRule, patternSanitize];
makeUsageRule[sym_Symbol, usage_] :=
  StringReplace[
   ToString[Unevaluated[sym],InputForm],
   $hiddenPackageExportContext->""
   ]->
   Replace[patternSanitize[usage],
    HoldComplete[s_] :>
     StringReplace[
      ToString[Unevaluated[s], InputForm],
      $hiddenPackageExportContext -> ""
      ]
    ];
makeUsageRule~SetAttributes~HoldAllComplete;
patternSanitize[usage_] :=
  ReplaceAll[
   ReplaceRepeated[
    HoldComplete[usage], {
     (Verbatim[Pattern] |
         Verbatim[Optional] |
         Verbatim[PatternTest] |
         Verbatim[Condition]
        )[p_, _] :> p
     }],
   (s_Symbol?(Function[Null, Quiet[Context[#]] =!= "System`",
        HoldAllComplete]) :> RuleCondition[
      ToExpression[$hiddenPackageExportContext <>
        SymbolName[Unevaluated@s]],
      True
      ])
   ];
patternSanitize~SetAttributes~HoldAllComplete


(* ::Subsubsection::Closed:: *)
(*exprDefinitionToUsage*)


Clear[ownPatternToUsage, downPatternToUsage, subPatternToUsage,
  upPatternToUsage];
ownPatternToUsage[sym_Symbol :> u_] :=
  makeUsageRule[sym, sym];
ownPatternToUsage~SetAttributes~HoldAllComplete;
downPatternToUsage[(sym_Symbol)[args___] :> u_] :=

  makeUsageRule[sym, sym[args]];
downPatternToUsage~SetAttributes~HoldAllComplete;
subPatternPullHead[pat_] :=
  NestWhile[
   Extract[#, {1, 0}, HoldComplete] &,
   HoldComplete[pat],
   ! MatchQ[#, HoldComplete[_Symbol]] &
   ];
subPatternPullHead~SetAttributes~HoldAllComplete;
subPatternToUsage[subPat_ :> u_] :=

  With[{sym = Extract[subPatternPullHead[subPat], 1, Unevaluated]},
   makeUsageRule[sym, subPat]
   ];
subPatternToUsage~SetAttributes~HoldAllComplete;
upPatternToUsage[{sym_, p_} :> u_] :=
  makeUsageRule[sym, p];
upPatternToUsage~SetAttributes~HoldAllComplete;


Clear[exprDefinitionToUsage];
exprDefinitionToUsage[
   ({Verbatim[HoldPattern][sym_], Verbatim[HoldPattern][p_]} :> v_)
   ] :=
  upPatternToUsage[{sym, p} :> v];
exprDefinitionToUsage[
   (Verbatim[HoldPattern][own_Symbol] :> v_)
   ] :=
  ownPatternToUsage[own :> v];
exprDefinitionToUsage[
   (Verbatim[HoldPattern][down : (_Symbol)[___]] :> v_)
   ] :=
  downPatternToUsage[down :> v];
exprDefinitionToUsage[
   (Verbatim[HoldPattern][sub_] :> v_)
   ] :=
  subPatternToUsage[sub :> v];


(* ::Subsubsection::Closed:: *)
(*exprFindDefinitions*)


definitionSanitize[expr_] :=
  HoldComplete[expr] //. {
     Verbatim[Verbatim][e_] :> e,
     Verbatim[HoldPattern][e_] :> e
     } // Apply[HoldPattern];
definitionSanitize~SetAttributes~HoldAllComplete


exprFindDefinitions[expr_] :=
  With[{eheld =
     Replace[HoldComplete[expr],
      HoldComplete[CompoundExpression[e___]] :>
       HoldComplete[e]
      ]},
   Flatten@{
     Cases[eheld,
      (SetDelayed | Set)[lhs_, rhs_] :>
       (definitionSanitize[lhs] :> rhs)
      ],
     Cases[eheld,
      (TagSetDelayed | TagSet)[tag_, lhs_, rhs_] :>
       ({definitionSanitize[tag], definitionSanitize[lhs]} :> rhs)
      ],
     Cases[eheld,
      (UpSetDelayed | UpSet)[lhs : _[e__], rhs_] :>
       Replace[
        {(subPatternPullHead /@ HoldComplete[e] // ReleaseHold)},
        HoldComplete[s_Symbol] :>
         ({definitionSanitize[s], definitionSanitize[lhs]} :> rhs),
        1
        ]
      ]
     }
   ];
exprFindDefinitions~SetAttributes~HoldAllComplete


(* ::Subsubsection::Closed:: *)
(*defsUsagesCell*)


Clear[makeUsageBoxes];
makeUsageBoxes[name_, strings : {__}] :=
  RowBox[{
    RowBox[{
      RowBox[{name, "::", "usage"}],
      "=",
      "\"" <> StringRiffle[strings, "\n"] <> "\""
      }],
    ";"
    }];
defsUsagesCell[defList_] :=
 With[{chunks =
    GroupBy[
     exprDefinitionToUsage /@ defList,
     First -> Last
     ]
   },
  If[Length[#] > 0,
     Cell[
      BoxData@
       RowBox@
        Riffle[
         Prepend[
           RowBox@{"(*", RowBox@{"Package", " ", "Declarations"},
             "*)"}]@
          KeyValueMap[makeUsageBoxes, #],
         "\n"
         ],
      "Code"
      ],
     {}
     ] & /@ {
    KeySelect[chunks,
     And @@ Through[{Not@*LowerCaseQ, LetterQ}@StringTake[#, 1]] &],
    KeySelect[chunks,
     Or @@ Through[{LowerCaseQ, Not@*LetterQ}@StringTake[#, 1]] &]
    }
  ]


(* ::Subsubsection::Closed:: *)
(*cellsBuildPackageCore*)


cellsBuildPackageCore[c : {___Cell}] :=
  (
   Begin[$hiddenPackageExportContext];
   (End[]; #) &@Replace[c,
     {
      cell : Cell[BoxData[b_], $codeExportStyles, ___] :>
       (
        Sow@
         ToExpression[b, StandardForm, exprFindDefinitions];
        Cell[
         BoxData@
          FrontEndExecute@FrontEnd`ReparseBoxStructurePacket[
            First@FrontEndExecute@
              FrontEnd`ExportPacket[cell, "InputText"]
            ],
         "Code"
         ]
        ),
      Cell[CellGroupData[cells_, state1___], state2___] :>
       Cell[CellGroupData[cellsBuildPackageCore[cells], state1],
        state2]
      },
     1
     ]
   );


(* ::Subsubsection::Closed:: *)
(*notebookExtractPackageName*)


notebookExtractPackageName[nb_NotebookObject] :=
  StringReplace[
   Replace[
    Quiet[NotebookFileName[nb]],
    {
     s_String :>
      FileBaseName[s],
     $Failed :>
      AbsoluteCurrentValue[nb, WindowTitle]
     }
    ],
   Except[WordCharacter] -> ""
   ];


(* ::Subsubsection::Closed:: *)
(*NotebookToPackage*)


NotebookToPackage[nb_NotebookObject] :=
  With[{cells =
     Replace[NotebookRead[nb], {
       c_Cell :> {c},
       {} :> First@NotebookGet[nb]
       }]},
   With[{data = Reap[cellsBuildPackageCore@cells]},
    With[{usagecells = defsUsagesCell[Flatten@Last@data]},
     Notebook[
      Flatten@{
        Cell[notebookExtractPackageName[nb], "Section"],
        Cell[
         BoxData@
          RowBox[{
            RowBox[{"BeginPackage", "[",
              "\"" <> notebookExtractPackageName[nb] <> "`\"",
              "]"}],
            ";"}],
         "Code"
         ],
        If[Length@usagecells[[1]] > 0,
         usagecells[[1]],
         {}
         ],
        Cell[
         BoxData@RowBox[{RowBox[{"Begin", "[", "\"`Private`\"", "]"}],
            ";"}],
         "Code"
         ],
        If[Length@usagecells[[2]] > 0,
         Cell[
          CellGroupData[
           Flatten@{
             Cell["Private Declarations", "Subsubsection"],
             usagecells[[2]]
             },
           Closed
           ]
          ],
         Nothing
         ],
        Cell[
         CellGroupData[
          Flatten@{
            Cell["Implementation", "Subsection"],
            Cell[BoxData@
              RowBox@{"(*",
                RowBox@{"Package", " ", "Implementation"},
                "*)"},
             "Code"
             ],
            First@data
            }
          ]
         ],
        Cell[
         CellGroupData[{
           Cell["End", "Subsection"],
           Cell[BoxData@RowBox[{RowBox[{"End", "[", "]"}], ";"}],
            "Code"
            ],

           Cell[BoxData@
             RowBox[{RowBox[{"EndPackage", "[", "]"}], ";"}],
            "Code"
            ]
           }]
         ]
        },
      StyleDefinitions -> "Package.nb"
      ]
     ]
    ]
   ];


NotebookToPackage[file_String?FileExistsQ] :=

 With[{nb = NotebookOpen[file, Visible -> False]},
  (NotebookClose[nb]; #) &@NotebookToPackage[nb]
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
