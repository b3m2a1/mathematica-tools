(* ::Package:: *)

(* ::Section:: *)
(*FEInfoExtractor*)


BeginPackage["FEInfoExtractor`"];


(*Package Declarations*)
AutoFrontEndInfo::usage="AutoFrontEndInfo[f, o]";


(* ::Subsubsection::Closed:: *)
(*Private Declarations*)


AppendTo[$ContextPath,$Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
getCodeValues::usage="getCodeValues[f, vs]";
extractorLocalized::usage="extractorLocalized[s]";
$usageTypeReplacements::usage="$usageTypeReplacements";
$usageSymNames::usage="$usageSymNames";
symbolUsageReplacementPattern::usage="symbolUsageReplacementPattern[names, conts]";
usagePatternReplace::usage="usagePatternReplace[vals, reps]";
generateSymbolUsage::usage="generateSymbolUsage[f, defaultMessages]";
autoCompletionsExtractSeeder::usage="autoCompletionsExtractSeeder[_ | (Blank | BlankSequence)[Hue | RGBColor | XYZColor | LABColor], n]
autoCompletionsExtractSeeder[_, n]
autoCompletionsExtractSeeder[_ | (Blank | BlankSequence)[File] | _File, n]
autoCompletionsExtractSeeder[Alternatives[s], n]
autoCompletionsExtractSeeder[_, n]
autoCompletionsExtractSeeder[a, n]";
autoCompletionsExtract::usage="autoCompletionsExtract[_[a]]
autoCompletionsExtract[f]";
generateAutocompletions::usage="generateAutocompletions[f, otherAutos]";
$autoCompletionFormats::usage="$autoCompletionFormats";
$autocompletionAliases::usage="$autocompletionAliases";
$autocompletionTable::usage="$autocompletionTable";
autocompletionPreCompile::usage="autocompletionPreCompile[v]
autocompletionPreCompile[o]
autocompletionPreCompile[s, v]
autocompletionPreCompile[l, v]";
addAutocompletions::usage="addAutocompletions[pats]
addAutocompletions[pat]";
reducePatterns::usage="reducePatterns[p]";
reconstructPatterns::usage="reconstructPatterns[p]";
argPatPartLens::usage="argPatPartLens[patList]";
mergeArgPats::usage="mergeArgPats[pats, returnNum]";
generateSIArgPat::usage="generateSIArgPat[f]";
generateSILocVars::usage="generateSILocVars[f]";
generateSIColEq::usage="generateSIColEq[f]";
generateSIOpNames::usage="generateSIOpNames[f]";
generateSyntaxInformation::usage="generateSyntaxInformation[f, ops]";
generateArgCount::usage="generateArgCount[f]";
setArgCount::usage="setArgCount[f, minA, maxA, noo]";
generateAutoFrontEndInfo::usage="generateAutoFrontEndInfo[f, ops]";


End[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


(* ::Subsection:: *)
(*CodeValues*)


(* ::Subsubsection::Closed:: *)
(*getCodeValues*)


getCodeValues[f_Symbol,
   vs :
    {Repeated[
      OwnValues | DownValues | SubValues | UpValues]} : {OwnValues, 
     DownValues, SubValues, UpValues}
   ] :=
  
  If[Intersection[Attributes@f, { ReadProtected, Locked}] === { 
     Locked, ReadProtected},
   {},
   Join @@ Map[#[f] &, vs]
   ];
getCodeValues~SetAttributes~HoldFirst


(* ::Subsection:: *)
(*Usage*)


(* ::Subsubsection::Closed:: *)
(*usagePatternReplace*)


extractorLocalized[s_] :=
  
  Block[{$ContextPath = {"System`"}, $Context = 
     "FEInfoExtractor`Private`"},
   Internal`WithLocalSettings[
    BeginPackage["FEInfoExtractor`Private`"];
    Off[General::shdw];,
    ToExpression[s],
    On[General::shdw];
    EndPackage[]
    ]
   ];
$usageTypeReplacements =
  {
   Integer -> extractorLocalized["int"],
   Real -> extractorLocalized["float"],
   String -> extractorLocalized["str"],
   List -> extractorLocalized["list"],
   Association -> extractorLocalized["assoc"],
   Symbol -> extractorLocalized["sym"]
   };
$usageSymNames =
  {
   Alternatives -> extractorLocalized["alt"],
   PatternTest -> extractorLocalized["test"],
   Condition -> extractorLocalized["cond"],
   s_Symbol :>
    RuleCondition[
     extractorLocalized@
      ToLowerCase[StringTake[SymbolName[Unevaluated@s], UpTo[3]]],
      True
     ]
   };
symbolUsageReplacementPattern[names_, conts_] :=
  s_Symbol?(
     GeneralUtilities`HoldFunction[
      ! MatchQ[Context[#], conts] &&
       ! 
        MemberQ[$ContextPath, Context[#]] &&
       ! 
        KeyMemberQ[names, SymbolName@Unevaluated[#]]
      ]
     ) :>
   RuleCondition[
    ToExpression@
     Evaluate[$Context <>
       
       With[{name = SymbolName@Unevaluated[s]},
        If[StringLength@StringTrim[name, "$"] > 0,
         StringTrim[name, "$"],
         name
         ]
        ]
      ],
    True];
usagePatternReplace[
   vals_,
   reps_: {}
   ] :=
  With[{
    names = AssociationMap[Null &, {}(*Names[]*)],
    conts = 
     Alternatives @@ {"System`", "FrontEnd`", "PacletManager`", 
       "Internal`"}
    },
   Replace[
      Replace[
       #,
        {
        Verbatim[HoldPattern][a___] :> a
        },
       {2, 10}
       ],
      Join[$usageTypeReplacements, $usageSymNames],
       Depth[#]
      ] &@
    ReplaceRepeated[
     FixedPoint[
      Replace[
        #,
        {
         Verbatim[Pattern][_, e_] :>
          e,
         Verbatim[HoldPattern][Verbatim[Pattern][_, e_]] :>
          
          HoldPattern[e],
         Verbatim[HoldPattern][Verbatim[HoldPattern][e_]] :>
         
           HoldPattern[e]
         },
        1
        ] &,
      vals
      ],
     Flatten@{
       reps,
       Verbatim[PatternTest][_, ColorQ] :>
        extractorLocalized@"color",
       Verbatim[PatternTest][_, ImageQ] :>
        extractorLocalized@"img",
       Verbatim[Optional][name_, _] :>
        name,
       Verbatim[Pattern][_, _OptionsPattern] :>
        Sequence[],
       Verbatim[Pattern][name_, _] :>
        name,
       Verbatim[PatternTest][p_, _] :>
        p,
       Verbatim[Condition][p_, _] :>
        p,
       Verbatim[Alternatives][a_, ___][___] |
         Verbatim[Alternatives][a_, ___][___][___] |
         Verbatim[Alternatives][a_, ___][___][___][___] |
         Verbatim[Alternatives][a_, ___][___][___][___][___] |
         Verbatim[Alternatives][a_, ___][___][___][___][___][___] :>
        a,
       Verbatim[Alternatives][a_, ___] :>
        RuleCondition[
         Blank[
          Replace[Hold@a,
           {
            Hold[p : Verbatim[HoldPattern][_]] :>
             p,
            Hold[e_[___]] :> e,
            _ :> a
            }
           ]
          ],
         True
         ],
       Verbatim[Verbatim][p_][a___] :>
        p,
       Verbatim[Blank][] :>
        extractorLocalized@"expr",
       Verbatim[Blank][
         t : Alternatives @@ Keys[$usageTypeReplacements]] :>
        
        RuleCondition[
         Replace[t,
          $usageTypeReplacements
          ],
         True
         ],
       Verbatim[Blank][t_] :>
        t,
       Verbatim[BlankSequence][] :>
        
        Sequence @@ extractorLocalized[{"expr1", "expr2"}],
       Verbatim[BlankNullSequence][] :>
        Sequence[],
       symbolUsageReplacementPattern[names, conts],
       h_[a___, Verbatim[Sequence][b___], c___] :> h[a, b, c]
       }
     ]
   ];


(* ::Subsubsection::Closed:: *)
(*generateSymbolUsage*)


generateSymbolUsage[f_, 
   defaultMessages : {(_Rule | _RuleDelayed) ...} : {}] :=
  With[
   {
    uml =
     Replace[defaultMessages,
      {
       (h : Rule | RuleDelayed)[Verbatim[HoldPattern][pat___], m_] :>
        h[HoldPattern[Verbatim[HoldPattern][pat]], m],
       (h : Rule | RuleDelayed)[pat___, m_] :>
        h[Verbatim[HoldPattern][pat], m],
       _ -> Nothing
       },
      {1}
      ]
    },
   Replace[
    DeleteDuplicates@usagePatternReplace[Keys@getCodeValues[f]],
    {
     Verbatim[HoldPattern][s_[a___]]:>
      With[
       {
        uu =
         StringTrim@
          Replace[HoldPattern[s[a]] /. uml,
           
           Except[_String] :>
            
            Replace[Quiet@s::usage, Except[_String] -> ""]
           ],
        sn = ToString[Unevaluated@s],
        meuu = ToString[Unevaluated[s[a]], InputForm]
        },
       StringReplace["FEInfoExtractor`Private`" -> ""]@
        If[! StringContainsQ[uu, meuu],
         meuu <> " " <>
          Which[
           uu == "",
           
           "has no usage message", ! 
            StringStartsQ[uu, 
             sn | (Except[WordCharacter] .. ~~ "RowBox[{" ~~ 
                Except[WordCharacter] ... ~~ sn)],
           uu,
           True,
           ""
           ],
         StringCases[uu, 
           (StartOfLine | StartOfString) ~~ Except["\n"] ... ~~ meuu ~~
             Except["\n"] ... ~~ EndOfLine,
           1
           ][[1]]
         ]
       ],
     _ -> Nothing
     },
    {1}
    ]
   ];
generateSymbolUsage~SetAttributes~HoldFirst;


(* ::Subsection:: *)
(*AutoComplete*)


(* ::Subsubsection::Closed:: *)
(*autoCompletionsExtractSeeder*)


Attributes[autoCompletionsExtractSeeder] =
  {
   HoldFirst
   };
autoCompletionsExtractSeeder[
   HoldPattern[Verbatim[PatternTest][_, ColorQ]] |
    (Blank | BlankSequence)[Hue | RGBColor | XYZColor | LABColor],
   n_
   ] := Sow[n -> "Color"];
autoCompletionsExtractSeeder[
   HoldPattern[Verbatim[PatternTest][_, DirectoryQ]],
   n_
   ] := Sow[n -> "Directory"];
autoCompletionsExtractSeeder[
   HoldPattern[Verbatim[PatternTest][_, FileExistsQ]] |
    (Blank | BlankSequence)[File] | _File,
   n_
   ] := Sow[n -> "FileName"];
autoCompletionsExtractSeeder[
   Verbatim[Alternatives][s__String],
   n_
   ] :=
  Sow[n -> {s}];
autoCompletionsExtractSeeder[
   Verbatim[Pattern][_, b_],
   n_
   ] := autoCompletionsExtractSeeder[b, n];
autoCompletionsExtractSeeder[
   Verbatim[Optional][a_, ___],
   n_
   ] := autoCompletionsExtractSeeder[a, n];


(* ::Subsubsection::Closed:: *)
(*autoCompletionsExtract*)


Attributes[autoCompletionsExtract] =
  {
   HoldFirst
   };
autoCompletionsExtract[Verbatim[HoldPattern][_[a___]]] :=
  {ReleaseHold@
    MapIndexed[
     Function[Null, autoCompletionsExtractSeeder[#, #2[[1]]], 
      HoldAllComplete],
     Hold[a]
     ]
   };
autoCompletionsExtract[f_Symbol] :=
 Flatten@
  Reap[
    autoCompletionsExtract /@
     Replace[
      Keys@getCodeValues[f, {DownValues, SubValues, UpValues}],
      {
        Verbatim[HoldPattern][
         HoldPattern[
          f[a___][___]|
          f[a___][___][___]|
          f[a___][___][___][___]|
          f[a___][___][___][___][___]|
          f[a___][___][___][___][___][___]
          ]
         ]:>HoldPattern[f[a]],
        Verbatim[HoldPattern][
         HoldPattern[
          _[___, f[a___], ___]|
          _[___, f[a___][___], ___]|
          _[___, f[a___][___][___], ___]|
          _[___, f[a___][___][___][___], ___]|
          _[___, f[a___][___][___][___][___], ___]|
          _[___, f[a___][___][___][___][___][___],  ___]
          ]
         ]:>HoldPattern[f[a]]
        },
      {1}
      ]
    ][[2]]


(* ::Subsubsection::Closed:: *)
(*generateAutocompletions*)


generateAutocompletions[
   f : _Symbol : None, 
   otherAutos : {(_Integer -> _) ...} : {}
   ] :=
  With[
   {
    gg =
     Join[
      GroupBy[
       If[Unevaluated[f] =!= None,
        autoCompletionsExtract[f],
        {}
        ],
       First -> Last,
       Replace[{s_, ___} :> s]
       ],
      GroupBy[
       otherAutos,
       First -> Last
       ]
      ]
    },
   With[{km = Max@Append[Keys@gg, 0]},
    Table[
     Lookup[gg, i, None],
     {i, km}
     ]
    ]
   ];
SetAttributes[generateAutocompletions, HoldFirst];


(* ::Subsection:: *)
(*Set Autocompletions*)


(* ::Subsubsection::Closed:: *)
(*$autoCompletionFormats*)


$autoCompletionFormats =
  Alternatives @@ Join @@ {
     Range[0, 9],
     {
      _String?(FileExtension[#] === "trie" &),
      {
       _String | (Alternatives @@ Range[0, 9]) | {__String},
       (("URI" | "DependsOnArgument") -> _) ...
       },
      {
       _String | (Alternatives @@ Range[0, 9]) | {__String},
       (("URI" | "DependsOnArgument") -> _) ...,
       (_String | (Alternatives @@ Range[0, 9]) | {__String})
       },
      {
       __String
       }
      },
     {
      "codingNoteFontCom",
      "ConvertersPath",
      "ExternalDataCharacterEncoding",
      "MenuListCellTags",
      "MenuListConvertFormatTypes",
      "MenuListDisplayAsFormatTypes",
      "MenuListFonts",
      "MenuListGlobalEvaluators",
      "MenuListHelpWindows",
      "MenuListNotebookEvaluators",
      "MenuListNotebooksMenu",
      "MenuListPackageWindows",
      "MenuListPalettesMenu",
      "MenuListPaletteWindows",
      "MenuListPlayerWindows",
      "MenuListPrintingStyleEnvironments",
      "MenuListQuitEvaluators",
      "MenuListScreenStyleEnvironments",
      "MenuListStartEvaluators",
      "MenuListStyleDefinitions",
      "MenuListStyles",
      "MenuListStylesheetWindows",
      "MenuListTextWindows",
      "MenuListWindows",
      "PrintingStyleEnvironment",
      "ScreenStyleEnvironment",
      "Style"
      }
     };


(* ::Subsubsection::Closed:: *)
(*$autocompletionAliases*)


$autocompletionAliases =
  {
   "None" | None | Normal -> 0,
   "AbsoluteFileName" | AbsoluteFileName -> 2,
   "FileName" | File | FileName -> 3,
   "Color" | RGBColor | Hue | XYZColor -> 4,
   "Package" | Package -> 7,
   "Directory" | Directory -> 8,
   "Interpreter" | Interpreter -> 9,
   "Notebook" | Notebook -> "MenuListNotebooksMenu",
   "StyleSheet" -> "MenuListStylesheetMenu",
   "Palette" -> "MenuListPalettesMenu",
   "Evaluator" | Evaluator -> "MenuListGlobalEvaluators",
   "FontFamily" | FontFamily -> "MenuListFonts",
   "CellTag" | CellTags -> "MenuListCellTags",
   "FormatType" | FormatType -> "MenuListDisplayAsFormatTypes",
   "ConvertFormatType" -> "MenuListConvertFormatTypes",
   "DisplayAsFormatType" -> "MenuListDisplayAsFormatTypes",
   "GlobalEvaluator" -> "MenuListGlobalEvaluators",
   "HelpWindow" -> "MenuListHelpWindows",
   "NotebookEvaluator" -> "MenuListNotebookEvaluators",
   "PrintingStyleEnvironment" | PrintingStyleEnvironment ->
    
    "PrintingStyleEnvironment",
   "ScreenStyleEnvironment" | ScreenStyleEnvironment ->
    
    ScreenStyleEnvironment,
   "QuitEvaluator" -> "MenuListQuitEvaluators",
   "StartEvaluator" -> "MenuListStartEvaluators",
   "StyleDefinitions" | StyleDefinitions ->
    
    "MenuListStyleDefinitions",
   "CharacterEncoding" | CharacterEncoding |
     ExternalDataCharacterEncoding ->
    
    "ExternalDataCharacterEncoding",
   "Style" | Style -> "Style",
   "Window" -> "MenuListWindows"
   };


(* ::Subsubsection::Closed:: *)
(*$autocompletionTable*)


$autocompletionTable = {
   f : $autoCompletionFormats :> f,
   Sequence @@ $autocompletionAliases,
   s_String :> {s}
   };


(* ::Subsubsection::Closed:: *)
(*autocompletionPreCompile*)


autocompletionPreCompile[v : Except[{__Rule}, _List | _?AtomQ]] :=
  
  Replace[
    Flatten[{v}, 1],
   $autocompletionTable,
   {1}
   ];
autocompletionPreCompile[o : {__Rule}] :=
  Replace[o,
   (s_ -> v_) :>
    (
     Replace[s, _Symbol :> SymbolName[s]] ->
      
      autocompletionPreCompile[v]
     ),
   1
   ];
autocompletionPreCompile[s : Except[_List], v_] :=
  
  autocompletionPreCompile[{s -> v}];
autocompletionPreCompile[l_, v_] :=
  autocompletionPreCompile@
   Flatten@{
     Quiet@
      Check[
       Thread[l -> v],
       Map[l -> # &, v]
       ]
     };


(* ::Subsubsection::Closed:: *)
(*addAutocompletions*)


addAutocompletions[
   pats : {(_String -> {$autoCompletionFormats ..}) ..}] :=
  
  If[$Notebooks &&
    
    Internal`CachedSystemInformation["FrontEnd", "VersionNumber"] > 
     10.0,
   Scan[
    FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]] &,
    pats
    ];
   pats,
   $Failed
   ];
addAutocompletions[pat : (_String -> {$autoCompletionFormats ..})] :=

    addAutocompletions[{pat}];


addAutocompletions[a__] /; (! TrueQ@$recursionProtect) :=
  
  Block[{$recursionProtect = True},
   Replace[
    addAutocompletions@autocompletionPreCompile[a],
    _addAutocompletions -> $Failed
    ]
   ];


(*addAutocompletions[
addAutocompletions,
{
None,
Replace[Keys[$autocompletionAliases],
Verbatim[Alternatives][s_,___]:>s,
1
]
}
];*)


(* ::Subsection:: *)
(*Pattern Parsing*)


(* ::Subsubsection::Closed:: *)
(*reducePatterns*)


reducePatterns[p_] :=
  Replace[
    p,
    {
     Except[
       _Pattern | _Optional | _Blank | _BlankSequence | 
        _BlankNullSequence | _PatternSequence | _OptionsPattern |
        _Repeated | _RepeatedNull | _Default | _PatternTest | _Condition
       ] -> _
     },
    {2, Infinity}
    ] //. {
    _Blank -> "Blank",
    _BlankSequence -> "BlankSequence",
    _BlankNullSequence -> "BlankNullSequence",
    _OptionsPattern :> "OptionsPattern",
    Verbatim[HoldPattern][
       Verbatim[Pattern][a_, b_]
       ] | Verbatim[Pattern][a_, b_] :> b,
    (PatternTest | Condition)[a_, b_] :> a,
    Verbatim[Optional][a_, ___] :> "Optional"[a],
    _Default -> "Default",
    Verbatim[Repeated][_] -> "Repeated"[\[Infinity]],
    Verbatim[RepeatedNull][_] -> "RepeatedNull"[\[Infinity]],
    Verbatim[Repeated][_, s_] :> "Repeated"[s],
    Verbatim[RepeatedNull][_, s_] :> "RepeatedNull"[s]
    };


(* ::Subsubsection::Closed:: *)
(*reconstructPatterns*)


reconstructPatterns[p_] :=
  p //. {
     "Optional"[a_] :> Optional[a],
     "Default" -> _.,
     "OptionsPattern" -> OptionsPattern[],
     "Blank" -> _,
     "BlankSequence" -> __,
     "BlankNullSequence" -> ___,
     "Repeated"[\[Infinity]] -> Repeated[_],
     "RepeatedNull"[\[Infinity]] -> RepeatedNull[_],
     "Repeated"[s_] :> Repeated[_, s],
     "RepeatedNull"[s_] :> RepeatedNull[_, s]
     } // Flatten;


(* ::Subsubsection::Closed:: *)
(*argPatPartLens*)


argPatPartLens[patList_] :=
  Thread[
   patList ->
    Replace[
     patList,
     {
      _Blank -> 1 ;; 1,
      _BlankSequence -> 1 ;; \[Infinity],
      _BlankNullSequence -> 0 ;; \[Infinity],
      Verbatim[Repeated][_] -> 1 ;; \[Infinity],
      Verbatim[RepeatedNull][_] -> 0 ;; \[Infinity],
      Verbatim[Repeated][_, {M_}] :> 1 ;; M,
      Verbatim[RepeatedNull][_, {M_}] :> 0 ;; M,
      (Repeated | RepeatedNull)[_, {M_}] :> ;; M,
      (Repeated | RepeatedNull)[_, {m_, M_}] :> m ;; M,
      _Optional -> 0 ;; 1,
      _Default -> 0 ;; 1,
      _OptionsPattern -> 0 ;; \[Infinity],
      _ -> 1 ;; 1
      },
     {1}
     ]
   ];


(* ::Subsubsection::Closed:: *)
(*mergeArgPats*)


mergeArgPats[pats_, returnNum : False | True : False] :=
 Module[
  {
   reppedPats = argPatPartLens /@ pats,
   mlen,
   paddies,
   werkit = True,
   patMaxes,
   patMins,
   patChoices,
   patNs,
   patCho
   },
  mlen = Max[Length /@ reppedPats];
  paddies = PadRight[#, mlen, _. -> 0 ;; 1] & /@ reppedPats;
  MapThread[
   Function[
    patMins = MinimalBy[{##}, #[[2, 1]] &];
    patMaxes = MaximalBy[{##}, #[[2, 2]] &];
    patChoices = Intersection[patMins, patMaxes];
    patNs = {patMins[[1, 2, 1]], patMaxes[[1, 2, 2]]};
    patCho =
     If[Length@patChoices > 0,
      SortBy[patChoices,
        Switch[#[[1]],
          _OptionsPattern,
          0,
          _RepeatedNull | _Repeated,
          1,
          _Optional | _Default,
          2,
          _,
          3
          ] &
        ][[1, 1]],
      Replace[ patNs,
       {
        {0, 1} -> _.,
        {1, \[Infinity]} -> __,
        {0, \[Infinity]} -> ___,
        {m_, n_} :> Repeated[_, {m, n}]
        }
       ]
      ];
    If[returnNum, patCho -> patNs, patCho]
    ],
   paddies
   ]
  ]


(* ::Subsection:: *)
(*SyntaxInfo*)


(* ::Subsubsection::Closed:: *)
(*generateSIArgPat*)


generateSIArgPat[f_] :=
  With[{dvs = Keys@getCodeValues[f, {DownValues, SubValues, UpValues}]},
   mergeArgPats@
    DeleteDuplicates[
     reconstructPatterns /@
      ReplaceAll[
       reducePatterns /@ 
         Replace[dvs,
           {
           Verbatim[HoldPattern][
             HoldPattern[
              f[a___][___]|
              f[a___][___][___]|
              f[a___][___][___][___]|
              f[a___][___][___][___][___]|
              f[a___][___][___][___][___][___]
              ]
             ]:>HoldPattern[f[a]],
            Verbatim[HoldPattern][
             HoldPattern[
              _[___, f[a___], ___]|
              _[___, f[a___][___], ___]|
              _[___, f[a___][___][___], ___]|
              _[___, f[a___][___][___][___], ___]|
              _[___, f[a___][___][___][___][___], ___]|
              _[___, f[a___][___][___][___][___][___],  ___]
              ]
             ]:>HoldPattern[f[a]]
            },
           1
           ],
       {
        (f | HoldPattern) -> List
        }
       ]
     ]
   ];
generateSIArgPat~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*generateSILocVars*)


generateSILocVars[f_] :=
  
  With[{att = Attributes[f], 
    dvs = Keys@getCodeValues[f, {DownValues}]},
   Which[
    MemberQ[att, HoldAll],
    {1, Infinity},
    MemberQ[att, HoldRest],
    {2, Infinity},
    MemberQ[att, HoldFirst],
    {1}
    ]
   ];
generateSILocVars~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*generateSIColEq*)


generateSIColEq[f_] :=
  
  With[{dvs = Keys@getCodeValues[f, {DownValues}]},
   Replace[{a_, ___} :> a]@
    MaximalBy[
     MinMax@Flatten@Position[#, _Equal, 1] & /@ dvs,
     Apply[Subtract]@*Reverse
     ]
   ];
generateSIColEq~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*generateSIOpNames*)


generateSIOpNames[f_] :=
  ToString[#, InputForm] & /@ Keys@Options[f];
generateSIOpNames~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*generateSyntaxInformation*)


Options[generateSyntaxInformation] =
  {
   "ArgumentsPattern" -> Automatic,
   "LocalVariables" -> None,
   "ColorEqualSigns" -> None,
   "OptionNames" -> Automatic
   };
Attributes[generateSyntaxInformation] =
  {
   HoldFirst
   };
generateSyntaxInformation[
   f_,
   ops : OptionsPattern[]
   ] :=
  {
   "ArgumentsPattern" ->
    Replace[OptionValue["ArgumentsPattern"],
     Automatic :> generateSIArgPat[f]
     ],
   "LocalVariables" ->
    Replace[OptionValue["LocalVariables"],
     Automatic :> generateSILocVars[f]
     ],
   "ColorEqualSigns" ->
    Replace[OptionValue["LocalVariables"],
     Automatic :> generateSIColEq[f]
     ],
   "OptionNames" ->
    Replace[OptionValue["OptionNames"],
     Automatic :> generateSIOpNames[f]
     ]
   };


(* ::Subsection:: *)
(*ArgCount*)


(* ::Subsubsection::Closed:: *)
(*generateArgCount*)


generateArgCount[f_] :=
  Module[
   {
    dvs = Keys@getCodeValues[f, {DownValues, SubValues, UpValues}],
    patsNums,
    patsMax,
    patsMin,
    patsTypes,
    doNonOp = False
    },
   dvs=
     Replace[dvs,
       {
         Verbatim[HoldPattern][
           HoldPattern[
            f[a___][___]|
            f[a___][___][___]|
            f[a___][___][___][___]|
            f[a___][___][___][___][___]|
            f[a___][___][___][___][___][___]
            ]
           ]:>HoldPattern[f[a]],
          Verbatim[HoldPattern][
           HoldPattern[
            _[___, f[a___], ___]|
            _[___, f[a___][___], ___]|
            _[___, f[a___][___][___], ___]|
            _[___, f[a___][___][___][___], ___]|
            _[___, f[a___][___][___][___][___], ___]|
            _[___, f[a___][___][___][___][___][___],  ___]
            ]
           ]:>HoldPattern[f[a]]
          },
       1
       ];
   patsNums =
    mergeArgPats[
     DeleteDuplicates[
      reconstructPatterns /@
       ReplaceAll[
        reducePatterns /@ dvs,
        {
         (f | HoldPattern) -> List
         }
        ]
      ],
     True
     ];
   patsTypes = patsNums[[All, 1]];
   patsMin =
    Block[{noopnoop = False},
     MapThread[
      If[noopnoop,
        0,
        If[MatchQ[#, _OptionsPattern],
         doNonOp = True;
         noopnoop = True;
         0,
         #2
         ]
        ] &,
      {
       patsTypes,
       patsNums[[All, 2, 1]]
       }
      ]
     ];
   patsMax =
    Block[{noopnoop = False},
     MapThread[
      If[noopnoop,
        0,
        If[MatchQ[#, _OptionsPattern],
         doNonOp = True;
         noopnoop = True;
         0,
         #2
         ]
        ] &,
      {
       patsTypes,
       patsNums[[All, 2, 2]]
       }
      ]
     ];
   {"MinArgs" -> Total[patsMin], "MaxArgs" -> Total[patsMax], 
    "OptionsPattern" -> doNonOp}
   ];
generateArgCount~SetAttributes~HoldFirst


(* ::Subsubsection:: *)
(*setArgCount*)


setArgCount[f_Symbol, minA : _Integer, maxA : _Integer|Infinity, 
   noo : True | False] :=
  f[argPatLongToNotDupe___] :=
   (
    1 /; (ArgumentCountQ[f,
        Length@If[noo,
          Replace[Hold[argPatLongToNotDupe], 
            Hold[argPatLongToNotDupe2___, 
              (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ..}) ...
              ] :> Hold[argPatLongToNotDupe2]
            ], Hold[argPatLongToNotDupe]], minA, maxA]; False)
    );
setArgCount~SetAttributes~HoldFirst


(* ::Subsection:: *)
(*AutoFrontEndInfo*)


(* ::Subsubsection::Closed:: *)
(*generateAutoFrontEndInfo*)


Options[generateAutoFrontEndInfo] =
  {
   "SyntaxInformation" -> {},
   "Autocompletions" -> {},
   "UsageMessages" -> {},
   "ArgCount" -> Automatic
   };
generateAutoFrontEndInfo[
   f_Symbol,
   ops : OptionsPattern[]
   ] :=
  {
   "UsageMessages" ->
    generateSymbolUsage[f,
     Cases[
      Flatten@List[OptionsPattern["UsageMessages"]],
      _Rule | _RuleDelayed
      ]
     ],
   "SyntaxInformation" ->
    generateSyntaxInformation[f,
     OptionValue["SyntaxInformation"]
     ],
   "Autocompletions" ->
    generateAutocompletions[f,
     OptionValue["Autocompletions"]
     ],
   "ArgCount" ->
    Replace[OptionValue@"ArgCount",
     Except[
       KeyValuePattern[
        {"MinArgs" -> _Integer, "MaxArgs" -> _Integer | Infinity, 
         "OptionsPattern" -> (True | False)}
        ]
       ] :> generateArgCount[f]
     ]
   };
generateAutoFrontEndInfo~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*AutoFrontEndInfo*)


Options[AutoFrontEndInfo] =
  {
   "SyntaxInformation" -> {},
   "Autocompletions" -> {},
   "UsageMessages" -> {},
   "SetInfo" -> False,
   "GatherInfo" -> True
   };
AutoFrontEndInfo[f_Symbol, o : OptionsPattern[]] :=
 Module[
  {
   sinfBase =
    If[OptionValue["GatherInfo"] =!= False,
     generateAutoFrontEndInfo[f, 
      FilterRules[{o}, Options[generateAutoFrontEndInfo]]], 
     Flatten[{o, Options[AutoFrontEndInfo]}]
     ],
   sops = FilterRules[{o}, Options[generateAutoFrontEndInfo]],
   as = {},
   si,
   um,
   ac,
   argX,
   set = TrueQ@OptionValue["SetInfo"]
   },
  si =
   Replace[
    Replace[Lookup[sinfBase, "SyntaxInformation"],
     Except[{(_String -> _) ..}] :>
      Lookup[as, "SyntaxInformation",
       Lookup[Set[as, generateAutoFrontEndInfo[f, sops]], 
        "SyntaxInformation"]
       ]
     ],
    {
     (Except[_String] -> _) -> Nothing,
     (k_ -> None) :> k -> {}
     },
    {1}
    ];
  um =
   Replace[Lookup[sinfBase, "UsageMessages"],
    Except[{__String}] :>
     Lookup[as, "UsageMessages",
      Lookup[Set[as, generateAutoFrontEndInfo[f, sops]], 
       "UsageMessages"]
      ]
    ];
  um = StringRiffle[um, "\n"];
  ac =
   Replace[Lookup[sinfBase, "Autocompletions"],
    Except[_List] :>
     Lookup[as, "Autocompletions",
      Lookup[Set[as, generateAutoFrontEndInfo[f, sops]], 
       "Autocompletions"]
      ]
    ];
  argX =
   Association@
    Replace[Lookup[sinfBase, "ArgCount"],
     Except[{"MinArgs" -> _Integer, 
        "MaxArgs" -> _Integer | \[Infinity], 
        "OptionsPattern" -> True | False}] :>
      Lookup[as, "ArgCount",
       Lookup[Set[as, generateAutoFrontEndInfo[f, sops]], "ArgCount"]
       ]
     ];
  If[set,
   SyntaxInformation[Unevaluated@f] = si;
   If[StringLength@um > 0, f::usage = um];
   If[Length@ac > 0, addAutocompletions[f, ac]];
   setArgCount[f, argX["MinArgs"], argX["MaxArgs"], 
    argX["OptionsPattern"]],
   (* dump to held expression for writing to file *)
   With[
    {
     si2 = si,
     um2 = um,
     acpat = autocompletionPreCompile[ac],
     minA = argX["MinArgs"],
     maxA = argX["MaxArgs"],
     noo = argX["OptionsPattern"]
     },
    Hold[
     SyntaxInformation[Unevaluated[f]] = si2;
     If[StringLength@um2 > 0, Set[f::usage, um2]];
     If[Length@acpat > 0,
      If[$Notebooks &&
        
        Internal`CachedSystemInformation["FrontEnd", 
          "VersionNumber"] > 10.0,
       FE`Evaluate[
        FEPrivate`AddSpecialArgCompletion[
         ToString[Unevaluated[f]] -> acpat]
        ]
       ];
      SetDelayed[
       f[argPatLongToNotDupe___],
       (
        1 /; (ArgumentCountQ[f,
            Length@If[noo,
              Replace[Hold[argPatLongToNotDupe], 
              
              Hold[argPatLongToNotDupe2___, (_Rule | _RuleDelayed | \
{(_Rule | _RuleDelayed) ..}) ...] :> Hold[argPatLongToNotDupe2]
              ], Hold[argPatLongToNotDupe]
             ], minA, maxA]; False)
        )
       ]
      ]
     ]
    ]
   ]
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
