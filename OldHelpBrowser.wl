(* ::Package:: *)

(* ::Section:: *)
(*OldHelpBrowser*)


BeginPackage["OldHelpBrowser`"];


(*Package Declarations*)
OpenHelpBrowser::usage="
OpenHelpBrowser[] opens an old-style help browser
OpenHelpBrowser[path] opens an old-style help browser intialized with path
OpenHelpBrowser[crit] opens for the first hit for search criterion crit
";
HelpPagesSearch::usage="
HelpPagesSearch[test] implements a search over documentation pages
HelpPagesSearch[test, True] has the pages open in the HelpBrowser
"


(* ::Subsubsection:: *)
(*Private Declarations*)


AppendTo[$ContextPath,$Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
$versionNumber::usage="$versionNumber";
loadDocumentationData::usage="loadDocumentationData[]";
loadDocumentationMetadata::usage="loadDocumentationMetadata[]";
ensureLoadedDocumentationMetadata::usage="ensureLoadedDocumentationMetadata[]";
preLoadDocumentationMetadata::usage="preLoadDocumentationMetadata[]";
loadCachedDocumentationData::usage="loadCachedDocumentationData[]";
cacheDocumentationData::usage="cacheDocumentationData[]";
clearCachedDocumentationData::usage="clearCachedDocumentationData[]";
$helpSearcherDocData::usage="The core doc data Association";
$helpSearcherDocMetadataDS::usage="$helpSearcherDocMetadataDS";
$helpBrowserCoreDS::usage="$helpBrowserCoreDS";
helpBrowserDSButton::usage="helpBrowserDSButton[entry, onClick]";
helpBrowserDS::usage="helpBrowserDS[formatFunction, onClick]";
helpBrowserPacletGetPath::usage="helpBrowserPacletGetPath[uri]";
helpBrowserPacletLookup::usage="helpBrowserPacletGetPath[browser, uri]";
helpSearcherDSNameSearch::usage="helpSearcherDSNameSearch[name, type]";
helpBrowserNameSearch::usage="helpBrowserNameSearch[browser, name, type]";
helpBrowserAutocomplete::usage="helpBrowserAutocomplete[s]";
helpBrowserSearch::usage="helpBrowserSearch[browser, name, type]";
helpBrowserDockedCell::usage="helpBrowserDockedCell[path]";
helpBrowserSetNotebook::usage="helpBrowserSetNotebook[browser, ...]";
helpBrowserNotebook::usage="helpBrowserNotebook[path]";


End[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


$versionNumber="1.1.7";


(* ::Subsubsection::Closed:: *)
(*Load Data*)


loadDocumentationData[] :=
 $helpSearcherDocData =
  Append[#,
     "Pages" ->
      Apply[Join]@
       Map[FileNames["*.nb", Last[#], \[Infinity]] &, #["Directories"]]
     ] &@
   <|
    "Directories" ->
     Join[
      DeleteDuplicatesBy[#[[1]]["Name"] &]@
       Select[DirectoryQ@*Last]@
        Map[
         # -> FileNameJoin[{#["Location"], "Documentation"}] &,
         PacletManager`PacletFind["*"]
         ],
       {"System"->FileNameJoin[{$InstallationDirectory, "Documentation"}]}
      ]
    |>


loadDocumentationMetadata[] :=
 (
  If[! AssociationQ@$helpSearcherDocData, loadDocumentationData[]];
  $helpSearcherDocData["Metadata"] =
   Association@
    Map[
     # :>
       Set[
        $helpSearcherDocData["Metadata", #],
        Append[
         Fold[Association@Lookup[#, #2, {}] &, 
          Options[Get[#]], {TaggingRules, "Metadata"}],
         "File" -> #
         ]
        ] &,
     $helpSearcherDocData["Pages"]
     ]
  )


ensureLoadedDocumentationMetadata[] :=
 
 If[! AssociationQ@$helpSearcherDocData || ! 
    KeyMemberQ[$helpSearcherDocData, "Metadata"],
  loadDocumentationMetadata[]
  ]


preLoadDocumentationMetadata[] :=
(ensureLoadedDocumentationMetadata[]; 
  Scan[Identity, $helpSearcherDocData["Metadata"]])


(* ::Subsubsection::Closed:: *)
(*Cache Data*)


loadCachedDocumentationData::corrupt="Cache has been corrupted. It will be ignored.";
loadCachedDocumentationData[] :=
  Catch[
   $helpSearcherDocData = 
     Replace[Get[LocalObject["docsDataCache"]],
      a_Association?(
       !AssociationQ[#["Metadata"]]||Length[#["Metadata"]]===0
       &):>
       (Message[loadCachedDocumentationData::corrupt];Throw@$Failed)
      ]
   ];
cacheDocumentationData[] :=
  Put[$helpSearcherDocData, LocalObject["docsDataCache"]];
clearCachedDocumentationData[] :=
  Quiet@DeleteFile@LocalObject["docsDataCache"];


(* ::Subsubsection::Closed:: *)
(*Docs Metadata Dataset*)


If[!MatchQ[OwnValues@$helpSearcherDocMetadataDS,{_:>_Association?AssociationQ}],
$helpSearcherDocMetadataDS :=
   (
    If[
     !AssociationQ@$helpSearcherDocData["Metadata"]||
      MatchQ[Normal@Take[$helpSearcherDocData["Metadata"],3],{___RuleDelayed}],
     preLoadDocumentationMetadata[]
     ];
    Dataset@
     Select[Values@$helpSearcherDocData["Metadata"], KeyMemberQ["uri"]]
    );
];


(* ::Subsubsection::Closed:: *)
(*Help Browser Tree*)


If[!MatchQ[OwnValues@$helpBrowserCoreDS,{_:>_Association?AssociationQ}],
$helpBrowserCoreDS :=
 $helpBrowserCoreDS=
  Map[
   Association@*
    Map[
     Lookup[#, "title"] -> #
      &
     ]
   ] /@  
   GroupBy[
    Normal@$helpSearcherDocMetadataDS,
    Key["type"] -> KeyDrop["type"],
    GroupBy[Key["context"] -> KeyDrop["context"]]
    ];
];


helpBrowserDSButton[entry_, onClick_] :=
  Column[{
    Button[#,
       onClick["paclet:" <> #2, #],
       BaseStyle -> "Hyperlink",
       Appearance -> None,
       Method -> "Queued"
       ] & @@ Lookup[entry, {"title", "uri"}],
    If[StringQ@#,
       Pane[#, {400, UpTo[250]}, ImageSizeAction -> "Scrollable"],
       #
       ] &@Lookup[entry, "summary", Nothing]
    }];
helpBrowserDS[
  formatFunction_: helpBrowserDSButton,
  onClick_: Documentation`HelpLookup
  ] :=
 Map[
   Association@*
    Map[
     Lookup[#, "title"] ->
       formatFunction[#, onClick]
      &
     ]
   ] /@ 
   GroupBy[
    $helpSearcherDocMetadataDS,
    Key["type"] -> KeyDrop["type"],
    GroupBy[Key["context"] -> KeyDrop["context"]]
    ]


(* ::Subsubsection::Closed:: *)
(*Autocomplete*)


If[!MatchQ[OwnValues@$helpBrowserAutocomplete,{_:>_AutocompletionFunction}],
$helpBrowserAutocomplete:=
 $helpBrowserAutocomplete = 
  Autocomplete[
   Keys@
    $helpBrowserCoreDS[
     "Symbol",
     "System`"
     ]
   ]
 ];
helpBrowserAutocomplete[e___]:=
 (
  $helpBrowserAutocomplete[e]
  )


(* ::Subsubsection::Closed:: *)
(*Constants*)


$helpBrowserTaggingRulesPath={TaggingRules,"OldHelpBrowser","Path"};


(* ::Subsubsection::Closed:: *)
(*SetBrowserPath*)


helpBrowserSetPath[browser_, path_List]:=
 CurrentValue[browser, $helpBrowserTaggingRulesPath] = path;


(* ::Subsubsection::Closed:: *)
(*Search*)


helpSearcherDSNameSearch[name_, type_:"Symbol"]:=
 SortBy[
  Normal@Select[$helpSearcherDocMetadataDS,
   #type===type&&StringContainsQ[#title, name]&
   ],
  StringLength[#["title"]]&
  ]


helpBrowserNameSearch[browser_, name_, type_:"Symbol"]:=
  With[{
    result=Replace[helpSearcherDSNameSearch[name,type], {a_,___}:>a]
    },
    If[AssociationQ@result,
      helpBrowserSetPath[browser, 
       Lookup[result, {"type", "context", "title"}]]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*PacletLookup*)


helpBrowserPathGetURI[path_]:=
 Replace[
  $helpBrowserCoreDS @@ path, {
   a_Association?(KeyMemberQ["uri"]):>
    (a["uri"])
   }]


helpBrowserPacletGetPath[pacletURI_]:=
 With[{
   baseFile=Documentation`ResolveLink[pacletURI]
   },
   Lookup[
      Lookup[$helpSearcherDocData["Metadata"],baseFile,
       <|"type"->Nothing,"context"->Nothing,"title"->Nothing|>
       ],
      {"type","context","title"}
      ]
   ]


helpBrowserPacletLookup[browser_ ,pacletURI_]:=
  With[{
    new=helpBrowserPacletGetPath[pacletURI],
    current=CurrentValue[browser, $helpBrowserTaggingRulesPath]
    },
    If[current===new,
     If[StringContainsQ[pacletURI,"#"],
      NotebookFind[$helpBrowserTaggingRulesPath, 
       StringSplit[pacletURI,"#"][[-1]], Next, CellID, AutoScroll -> Top]
      ],
     helpBrowserSetPath[browser, new]
     ]
    ];


(* ::Subsubsection::Closed:: *)
(*HelpBrowserSearch*)


helpBrowserSearch[browser_, name_, type_:"Symbol"]:=
 With[{pl=Documentation`ResolveLink[name]},
  If[pl=!=Null&&!
   StringStartsQ[pl,
    FileNameJoin@{PacletManager`$UserBasePacletsDirectory,"Temporary"}
    ],
   helpBrowserPacletLookup[browser,name],
   helpBrowserNameSearch[browser,name,type]
   ]
  ]


(* ::Subsubsection::Closed:: *)
(*Styles*)


$helpBrowserStyleDefinitions=
Notebook[{
 Cell[
  StyleData[
   StyleDefinitions->
    FrontEnd`FileName[{"Wolfram"}, "Reference.nb",
     CharacterEncoding -> "UTF-8"
     ]
   ]
  ],
 Cell[StyleData["Notebook"],
  NotebookEventActions->
   {
    {"MenuCommand","OpenHelpLink"}:>
     Apply[helpBrowserPacletLookup,
      Reverse@Last@CurrentValue["EventData"]
      ]
    }
  ],
 Cell[StyleData["Link"],
  ButtonBoxOptions->{
   ButtonFunction:>
    Function[
     KernelExecute[
      helpBrowserPacletLookup[EvaluationNotebook[],#]
      ]
     ],
   Evaluator->"Local"
   }
  ]
 },
 StyleDefinitions->"PrivateStylesheetFormatting.nb"
 ];


(* ::Subsubsection::Closed:: *)
(*PanePicker*)


helpBrowserPickerPane[
 nb_,
 choices_,
 idx_,
 pp_,
 lineHeight_
 ]:=
  ListPicker[
    Dynamic[
     If[Length@pp >= idx, pp[[{idx}]]],
     Function@
      With[{
       p=$helpBrowserTaggingRulesPath,
       newPath=
        ReplacePart[
         If[Length@pp >= idx,
          Take[pp, idx],
          PadRight[pp, idx, ""]
          ], 
         idx -> First@#
         ]
       },
       If[newPath=!=pp,
        Set[ CurrentValue[nb, p], newPath];
        ]
       ]
     ],
    Map[#->
     Pane[#,{Full,lineHeight}]&, 
     choices
     ],
    With[{wlpp=If[Length@pp===3, 2, Length@pp]},
     ImageSize -> 
      {
       Scaled[1 / (wlpp + 1)], 
       Full
       }
     ],
    Background -> {{GrayLevel[.98], White}},
    Appearance -> "Frameless",
    If[Length@pp >= idx&&MemberQ[choices, pp[[idx]]],
     ScrollPosition ->
      {
       0, 
       With[
        {
         p1=FirstPosition[choices, pp[[idx]]][[1]]
         },
        Max@{(1+p1)*lineHeight-100,0}
        ]
       },
     Sequence@@{}
     ],
   Spacings->0
   ];


(* ::Subsubsection::Closed:: *)
(*SetNotebook*)


helpBrowserSetNotebook[
 nb_,
 uri_,
 currentLoadedPath_,
 panePathCached_,
 searchString_,
 preserveHistory_:False
 ]:=
 If[currentLoadedPath =!= panePathCached,
  If[currentLoadedPath=!=None,
   currentLoadedPath = panePathCached
   ];
  With[{
   tr= 
    Join[
     {
      "Path" -> panePathCached,
      "LoadedPath" -> panePathCached,
      "SearchString" -> uri,
      "LinkHistory" -> 
       If[preserveHistory || 
         Length[Lookup[#, "LoadedPath"]]<3,
        Lookup[#, "LinkHistory", {}],
        Append[Lookup[#, "LinkHistory", {}], 
         {
          Lookup[#, "LoadedPath", {}],
          helpBrowserPathGetURI@Lookup[#, "LoadedPath", {}]
          }
         ]
        ],
      "LinkFuture" -> 
       If[preserveHistory, 
        Lookup[#, "LinkFuture", {}], 
        {}
        ]
      },
     DeleteCases[#,
      "LoadedPath" | "Path" | "SearchString" |
       "LinkHistory" | "LinkFuture" ->_
      ]
     ]&@CurrentValue[EvaluationNotebook[], {TaggingRules, "OldHelpBrowser"}],
   sd=$helpBrowserStyleDefinitions,
   cleanURI="paclet:"<>StringTrim[uri,"paclet:"],
   enb=nb
   },
   Replace[Documentation`ResolveLink[cleanURI],{
    f_String:>
     With[{put=Get[f]},
     With[{
       mcells=
        Sequence@@Flatten@
         Map[
          {
           FrontEnd`SetOptions[#, Deletable->True],
           FrontEnd`NotebookDelete[#]
           }&,
          Cells[enb]
          ],
       ops=
        Join[
         DeleteCases[Options[put],
          StyleDefinitions|TaggingRules|
           WindowSize|WindowMargins->_
          ],
         {
          Background->Inherited,
          StyleDefinitions->sd,
          TaggingRules->
           Append[
            Lookup[Options[put],TaggingRules, {}], 
            "OldHelpBrowser"->tr
            ]
          }
         ]
      },
      MathLink`CallFrontEnd@{
       FrontEnd`SetOptions[enb, ops],
       FrontEnd`NotebookSuspendScreenUpdates[enb],
       mcells,
       FrontEnd`SelectionMove[enb, Before, Notebook],
       FrontEnd`NotebookWrite[enb, put, None,
        AutoScroll->False
        ],
       FrontEnd`NotebookResumeScreenUpdates[enb]
       };
      SetOptions[enb,
       Selectable->True
       ];
      nb
     ]
     ],
   _:>$Failed
   }]
  ]
 ];
helpBrowserSetNotebook~SetAttributes~HoldRest


(* ::Subsubsection::Closed:: *)
(*Old Set*)


helpBrowserSetNotebookOld[
 nb_,
 nbPath_,
 currentLoadedPath_,
 panePathCached_,
 searchString_
 ]:=
 If[currentLoadedPath =!= panePathCached,
  CheckAll[
   FrontEndExecute@
    FrontEnd`NotebookSuspendScreenUpdates[nb];
   With[{
    c=Cells[nb],
    nbExpr =
     DeleteCases[
      WindowSize | WindowMargins | DockedCells | 
      StyleDefinitions -> _]@
     Replace[Documentation`ResolveLink[nbPath], {
      f_String?FileExistsQ :> 
        ReplaceAll[Get[f],{
          HoldPattern[Documentation`HelpLookup[e_]]:>
           (helpBrowserPacletLookup[nb, e]),
          HoldPattern[Documentation`HelpLookup[e_,n_]]:>
           (helpBrowserPacletLookup[n, e])
          }],
      _ -> Notebook[{}]
      }]
    },
    Replace[
     Fold[
      Lookup[#,#2,<||>]&,
      Options[nbExpr],
      {TaggingRules,"Metadata","uri"}
      ],
     s_String:>Set[searchString,s]
     ];
    FrontEndExecute@
    Join[
     Map[
      FrontEnd`SetOptions[#,{
       Deletable -> True,
       Editable->True
       }]&,
      c
      ],
     {
      FrontEnd`NotebookDelete[c],
      FrontEnd`SetOptions[nb,{
       Selectable->True,
       Editable->True,
       Deployed->False
       }],
      FrontEnd`NotebookWrite[
       nb,
       First@nbExpr
       ],
      FrontEnd`SetOptions[nb,
       Join[
        {
         StyleDefinitions -> $helpBrowserStyleDefinitions,
         TaggingRules -> 
          Join[
           Lookup[Options[nbExpr],TaggingRules,{}],
           CurrentValue[nb, TaggingRules]
           ]
         },
        Cases[ DeleteCases[TaggingRules->_]@Rest@nb, _?OptionQ]
        ]
       ],
     FrontEnd`SelectionMove[nb, Before, Notebook],
     FrontEnd`NotebookResumeScreenUpdates[nb]
     }
    ]
   ],
   FrontEndExecute@
    FrontEnd`NotebookResumeScreenUpdates[nb]
   ];
   currentLoadedPath = panePathCached
   ];
helpBrowserSetNotebookOld~SetAttributes~HoldRest


(* ::Subsubsection::Closed:: *)
(*ResizeBar*)


helpBrowserResizeBar[
 panePickerHeight_,
 resizeDragBase_,
 panePickerHeightBase_
 ]:=
 EventHandler[
           MouseAppearance[#,"FrameTBResize"]&@
             Graphics[
              {},
              Background->GrayLevel[.7],
              ImageSize->{Full,2},
              AspectRatio->Full,
              ImagePadding->None,
              Method->{"ShrinkWrap"->True},
              ImageMargins->0
              ],{
            "MouseDown":>
              (
               If[!NumericQ@resizeDragBase,
                Replace[MousePosition["ScreenAbsolute"],
                 {_,y_}:>Set[resizeDragBase,y]
                 ]
                ];
               If[!NumericQ@panePickerHeightBase,
                panePickerHeightBase=panePickerHeight
                ]
               ),
            "MouseUp":>
              Clear[resizeDragBase,panePickerHeightBase],
            "MouseDragged":>
             (
              Replace[MousePosition["ScreenAbsolute"],
               {_,m_}:>
                If[!NumericQ@resizeDragBase,
                 Set[resizeDragBase,m];
                 panePickerHeightBase=panePickerHeight,
                 With[{
                  new = panePickerHeightBase +  m - resizeDragBase,
                  old = panePickerHeight
                  },
                  panePickerHeight = new
                  ]
                 ]
               ]
              ),
            PassEventsDown->True
            }];
helpBrowserResizeBar~SetAttributes~HoldAll;


(* ::Subsubsection::Closed:: *)
(*SearchIcon*)


$helpBrowserSearchIcon=
 Function[{
               "Default"->#,
               "Hover"->
                 Image[Darker[#,.5],
                 "Byte",
                 "ColorSpace"->"RGB",
                 Interleaving->True],
               "Pressed"->
                Image[Lighter[#,.5],
                 "Byte",
                 "ColorSpace"->"RGB",
                 Interleaving->True]
               }]@ToExpression@FrontEndResource["FEBitmaps","SearchIcon"];


(* ::Subsubsection::Closed:: *)
(*XIcon*)


$helpBrowserXIcon={
                  "Default"->
                   ToExpression@FrontEndResource["FEBitmaps","CircleXIcon"],
                  "Hover"->
                   ToExpression@FrontEndResource["FEBitmaps","CircleXIconHighlight"],
                  "Pressed"->
                   ToExpression@FrontEndResource["FEBitmaps","CircleXIconPressed"]
                  }


(* ::Subsubsection:: *)
(*Docked Cell*)


Off[General::shdw];
System`WholeCellGroupOpener;
On[General::shdw];


helpBrowserDockedCell[path : _List : {}] :=
  DynamicModule[
   {
    serialNo,
    browserLocked,
    loadPath,
    loadDMVars,
    setDMVars,
    panePathCached,
    currentLoadedPath,
    linkHistory,
    linkFuture,
    paneCoreCached,
    searchString,
    panePicker,
    panePickerHeight,
    setNB,
    setNBPrevious,
    setNBFuture,
    nbSetFlag,
    showBrowser,
    showBrowserToggled = False,
    advancedSearch = False,
    advancedSearchValues,
    advancedSearchResults,
    listPickerLineHeight = 20,
    panePickerHeightBase,
    resizeDragBase,
    autoCompleteFilling,
    autoCompleteLastFill
    },
   (* Total display *)
   Dynamic[
    If[!browserLocked[]//TrueQ,
     Replace[
      CurrentValue[ EvaluationNotebook[], 
       {TaggingRules, "OldHelpBrowser", "Path"}],{
      p_List:>Set[panePathCached, p],
      e_:>Set[panePathCached, path]
      }];
    loadDMVars[];
    If[showBrowser//TrueQ,
     Grid[{
       (* Search bar *)
       List@
        Dynamic[
         Panel[
          Grid[{
           {"",
            Item[#,Alignment->{Right,Top}]&@
            Button[
             Row[
              {
               "",
               "Hide Browser"
               },
               Spacer[2]
              ],
              showBrowser = False;
              setDMVars[],
              Appearance -> None,
              BaseStyle -> "Message"
              ]},
           {
            #,
            SpanFromLeft
            }
           },
           ItemSize->{{0,Scaled[1]},Automatic}
           ],
          ImageSize->{Scaled[1], Automatic},
          Appearance->
           Image[CompressedData["1:eJztyDkKAkEUhOFGE0OvIHgIU0PTEQ8ww7SDSQs9gnhXl3HBfbmC7Q0qeWDwf1CPqtcr5tm05ZyrO+lk+XIYY74ad9OYhHpWBV+OwsJXPg6Kdnr2U8qUX/8AAAAA+HtvyF6AgSdg4AHZHbIbYOAKGLhAdobsBBg4QnaAbA9ZAxjYAQa2gIENZGvIvp8pRMU="], 
            "Byte", ColorSpace -> "RGB", Interleaving -> True],
          Alignment->Center
          ], 
         TrackedSymbols:>{advancedSearch}
         ]&@
        If[!advancedSearch,
         (* Standard search interface *)
         Grid[{
          {
           Row[{
            Button["",
             setNBPrevious[],
             Appearance->
             {
              "Default"->
               ToExpression@FrontEndResource["FEBitmaps","BackIcon"],
              "Hover" -> 
               ToExpression@FrontEndResource["FEBitmaps","BackIconHot"],
              "Disabled" -> 
               ToExpression@FrontEndResource["FEBitmaps","DisabledBackIcon"]
              },
             ImageSize->{21,25},
             Enabled -> Dynamic[Length[linkHistory] > 0]
             ],
            Button["",
             setNBFuture[],
             Appearance->
             {
              "Default"->ToExpression@FrontEndResource["FEBitmaps","ForwardIcon"],
              "Hover" -> ToExpression@FrontEndResource["FEBitmaps","ForwardIconHot"],
              "Disabled" ->
               ToExpression@FrontEndResource["FEBitmaps","DisabledForwardIcon"]
              },
             ImageSize -> {21,25},
             Enabled -> Dynamic[Length[linkFuture] > 0]
             ]
            },
            Spacer[2]
            ],
           autoCompleteFilling=.;
           EventHandler[
            InputField[
             Dynamic[searchString],(*,
              (
              If[
               (
                QuantityMagnitude[
                 UnitConvert[Now-autoCompleteLastFill, "Seconds"]
                 ]>1
                 )&&
               KeyMemberQ[
                $helpBrowserCoreDS["Symbol", "System`"],
                #],
               searchString=#;
               autoCompleteLastFill=Now;
               setDMVars[];
               helpBrowserSearch[EvaluationNotebook[], #];,
               searchString=#
               ];
              )&
              ]*)
             String, 
             FieldSize->35(*,
             MenuList->
              Sort@Keys@$helpBrowserCoreDS["Symbol","System`"]*)(*,
             FieldCompletionFunction\[Rule]helpBrowserAutocomplete*)
             ],{
            "ReturnKeyDown":>
              helpBrowserSearch[EvaluationNotebook[], searchString](*,
            PassEventsDown\[Rule]True*)
            }],
           Button["",
            helpBrowserSearch[EvaluationNotebook[], searchString],
            Appearance->$helpBrowserSearchIcon,
            ImageSize->{15,14}
            ]
           },
          {
           "",
           Item[#, Alignment->Right]&@
           Button["Advanced search",
            advancedSearch = True;
            setDMVars[],
            Appearance -> None,
            BaseStyle -> "Message"
            ]
           }
          },
         Alignment->Scaled[.8]
         ],
        (* Advanced search system *)
        Grid[
         Join[
          Insert[
           Map[
            {#, 
             EventHandler[
              With[{f=#},
              InputField[
               Dynamic[
                advancedSearchValues[[f]],
                Function[
                 advancedSearchValues[[f]] = #;
                 setDMVars[]
                 ]
                ],
               Hold[Expression],
               FieldSize->30
               ]
               ],
              "ReturnKeyDown":>
               Set[advancedSearchResults, 
                HelpPagesSearch[
                 Normal@DeleteCases[Null]@
                  Map[
                   Replace[
                    s_String:>
                     Function[StringContainsQ[#,s,IgnoreCase->True]]
                    ]@*
                   Replace[{
                    Verbatim[RawBoxes][s_String]:>s,
                    Hold[a_Symbol]:>ToString[Unevaluated[a]],
                    Hold[e_]:>e
                    }],
                   advancedSearchValues
                   ],
                 True
                 ]
                ]
              ]
             }&,
            Keys@advancedSearchValues
            ],
          Button["",
            Set[advancedSearchResults, 
              HelpPagesSearch[
                Normal@DeleteCases[Null]@
                 Map[
                  Replace[
                   s_String:>
                    Function[StringContainsQ[#,s,IgnoreCase->True]]
                   ]@*
                  Replace[{
                   Verbatim[RawBoxes][s_String]:>s,
                   Hold[a_Symbol]:>ToString[Unevaluated[a]],
                   Hold[e_]:>e
                   }],
                  advancedSearchValues
                  ],
                True
                ]
               ],
            Appearance->$helpBrowserSearchIcon,
            ImageSize->{15,14}
            ],
          {-1,-1}
          ],
          {
           {
            Item[#, Alignment->Left]&@
            Row[{
            Button["",
             setNBPrevious[],
             Appearance->
             {
              "Default"->ToExpression@FrontEndResource["FEBitmaps","BackIcon"],
              "Hover" -> ToExpression@FrontEndResource["FEBitmaps","BackIconHot"],
              "Disabled" -> ToExpression@FrontEndResource["FEBitmaps","DisabledBackIcon"]
              },
             ImageSize->{21,25},
             Enabled -> Dynamic[Length[linkHistory] > 0]
             ],
            Button["",
             setNBFuture[],
             Appearance->
             {
              "Default"->ToExpression@FrontEndResource["FEBitmaps","ForwardIcon"],
              "Hover" -> ToExpression@FrontEndResource["FEBitmaps","ForwardIconHot"],
              "Disabled" ->
               ToExpression@FrontEndResource["FEBitmaps","DisabledForwardIcon"]
              },
             ImageSize -> {21,25},
             Enabled -> Dynamic[Length[linkFuture] > 0]
             ]
            },
            Spacer[2]
            ],
            Item[#,Alignment->Right]&@
            Button["Standard search",
             advancedSearch = False;
             setDMVars[],
             Appearance -> None,
             BaseStyle -> "Message"
             ]
            },
          {
           Item[#,Alignment->Center]&@
           Dynamic@
             Replace[advancedSearchResults,{
              _Dataset?(Length[#]===0&)->"No results...",
              Except[_Dataset]->"",
              e_:>
               Row[{
                Framed[
                 Pane[
                  With[{grid=Partition[Normal@e,UpTo[5]]},
                   TextGrid[
                    grid,
                    Alignment->Left,
                    ItemSize->Full,
                    Dividers->
                     {
                      Table[
                       j->GrayLevel[.8],
                       {j, 2, Length[grid[[1]]]}
                       ],
                      Table[
                       i->GrayLevel[.8],
                       {i, 2, Length[grid]}
                       ]
                      }
                    ]
                   ],
                  {475, 100}, 
                  ImageSizeAction->"Scrollable",
                  Scrollbars->Automatic
                  ],
                 Background->White,
                 FrameStyle->Gray,
                 FrameMargins->None
                 ],
                Button["",
                 advancedSearchResults = Null,
                 Appearance->$helpBrowserXIcon,
                 ImageSize->{14,14}
                 ]
               },
               Spacer[2]
               ]
             }],
            SpanFromLeft
            }
          }
          ],
         Alignment->Left
         ]
        ],
        (* Click pane *)
        List@
        With[{paneCore=
         If[
           !MatchQ[paneCoreCached, _Framed]||
           (
            (showBrowserToggled || (currentLoadedPath =!= panePathCached )) &&
            !TrueQ[browserLocked[]]
            ),
           showBrowserToggled = False;
           paneCoreCached = 
            Framed[
             Row@
              Prepend[
               MapIndexed[
                Replace[
                 $helpBrowserCoreDS @@ Take[panePathCached, #2[[1]]], {
                  a_Association?(KeyMemberQ["uri"]):>
                   (setNB[a["uri"]]),
                  a_Association :> 
                   panePicker[
                    SortBy[a // Keys // Sort, #=!="System`"&], 
                    #2[[1]] + 1
                    ]
                 }] &,
                panePathCached
                ],
              panePicker[ 
               SortBy[
                Sort@Keys[$helpBrowserCoreDS],
                Switch[#,
                 "Root Guide", 0,
                 "Symbol",1,
                 "Guide", 2,
                 "Tutorial",3,
                 _,4
                 ]&
                ],
               1
               ]
              ],
            ImageSize->Full,
            Background->White,
            FrameStyle->Gray,
            FrameMargins->None
            ],
           paneCoreCached
           ]
          },
           Column[{
            Dynamic[
             Pane[
              paneCore,
              ImageSize->{Full, panePickerHeight},
              AppearanceElements -> None
              ],
             TrackedSymbols:>{panePickerHeight}
             ],
            helpBrowserResizeBar[
             panePickerHeight,
             resizeDragBase,
             panePickerHeightBase
             ]
            },
           Spacings->0
           ]
        ]
       },
       Spacings->{Automatic,0}
       ],
     If[
        currentLoadedPath =!= panePathCached && 
        !browserLocked[],
        Replace[
         $helpBrowserCoreDS @@ panePathCached, {
          a_Association?(KeyMemberQ["uri"]):>
           (setNB[a["uri"]])
         }]
        ];
     Button[
      Column[{
       Row[{"","Show Browser"}, Spacer[2]],
       Spacer[5]
       },
       Spacings->0
       ],
      showBrowser = True;
      showBrowserToggled = True;
      setDMVars[],
      Appearance -> None,
      BaseStyle -> "Message"
      ]
     ],
    Row@{
     "Couldn't display browser.",
     Button["Retry?",
      CurrentValue[
        EvaluationNotebook[],
        {TaggingRules, "OldHelpBrowser", ".lock"}
        ] = False,
      Appearance -> None,
      BaseStyle -> "Message"
      ]
     }
    ]
   ],
  (* INITIALIZATION PROCEDURES *)
   Initialization:>
    (
     browserLocked = 
      Function[
       TrueQ@
       CurrentValue[
        EvaluationNotebook[],
        {TaggingRules, "OldHelpBrowser", ".lock"}
        ]
       ];
     serialNo=
      MaximalBy[
      DeleteCases[""]@
       StringTrim[Names["FE`*Variable*"],
        "FE`DynamicModuleVariableList$"],
       ToExpression
      ][[1]];
     (* Initialize front-end variables *)
     loadPath=
      Function@
       Replace[
        CurrentValue[ EvaluationNotebook[], 
         {TaggingRules, "OldHelpBrowser", "Path"}],{
         p_List:>Set[panePathCached, p],
         _:>Set[panePathCached, path]
         }];
     loadDMVars=
     Function@
     With[{
      core=
      CurrentValue[ EvaluationNotebook[], {TaggingRules, "OldHelpBrowser"}]
      },
      Replace[Lookup[core, "Path"],{
       p_List:>Set[panePathCached, p],
       _:>(Set[panePathCached, path])
       }];
      Replace[Lookup[core, "LoadedPath"],{
       p_List:>Set[currentLoadedPath, p],
       _:>Set[currentLoadedPath,{}]
       }];
      Replace[Lookup[core, "LinkHistory"],{
       p_List:>Set[linkHistory, p],
       _:>Set[linkHistory,{}]
       }];
      Replace[Lookup[core, "LinkFuture"],{
       p_List:>Set[linkFuture, p],
       _:>Set[linkFuture,{}]
       }];
      Replace[Lookup[core, "SearchString"],{
       p_String:> Set[searchString, p],
       _:> Set[searchString, ""]
       }];
      Replace[
       Lookup[core, "BrowserVisible"],{
       b:True|False:>Set[showBrowser, b],
       _:>Set[showBrowser, True]
       }
       ];
      Replace[
       Lookup[core, "AdvancedSearchOn"],{
       b:True|False:>Set[advancedSearch, b],
       _:>Set[advancedSearch, False]
       }];
      Replace[
       Lookup[core, "AdvancedSearchValues"],{
       p_Association:>Set[advancedSearchValues, p],
       _:> 
        Set[advancedSearchValues, 
         AssociationMap[Null&, {"type","title","context","status","uri"} ]]
       }];
      Replace[
       Lookup[core, "BrowserHeight"],{
       p_?NumberQ:>Set[panePickerHeight, p],
       _:> 
        Set[panePickerHeight, 150]
       }];
      ];
     loadDMVars[];
     setDMVars=
      Function@
       With[{
        varMap=
         AssociationThread[
          {
           "Path",
           "LoadedPath",
           "SearchString", 
           "BrowserVisible", 
           "AdvancedSearchOn",
           "AdvancedSearchValues",
           "BrowserHeight",
           "LinkHistory",
           "LinkFuture",
           "DynamicModuleInfo"
           },
           {
            panePathCached,
            currentLoadedPath,
            searchString,
            showBrowser,
            advancedSearch,
            advancedSearchValues,
            panePickerHeight,
            linkHistory,
            linkFuture,
            <|
             "$ModuleNumber"->serialNo,
             "Context"->Context[serialNo]
             |>
            }
          ]
        },
        MathLink`CallFrontEndHeld@@
        Thread[
        KeyValueMap[
         Hold@
          FrontEnd`SetValue[
           FEPrivate`Set[
            CurrentValue[EvaluationNotebook[], 
             {TaggingRules, "OldHelpBrowser", #}
             ],
            #2
            ]
           ]&,
         varMap
         ],
        Hold
        ]
       ];
     setDMVars[];
     (* Initialize notebook setting procedure *)
     setNB = 
      Function[
       If[ currentLoadedPath =!= panePathCached,
        CurrentValue[ 
         EvaluationNotebook[],
         {TaggingRules, "OldHelpBrowser", ".lock"}
         ] = True;
       setDMVars[];
       helpBrowserSetNotebook[
        EvaluationNotebook[],
        #,
        currentLoadedPath,
        panePathCached,
        searchString
        ];
        CurrentValue[ 
         EvaluationNotebook[],
         {TaggingRules, "OldHelpBrowser", ".lock"}
         ] = False;
       ];
       Nothing
       ];
     setNBPrevious = 
      Function[
       With[{p=linkHistory[[-1]]},
       CurrentValue[ 
         EvaluationNotebook[],
         {TaggingRules, "OldHelpBrowser", ".lock"}
         ] = True;
       linkHistory = Drop[ linkHistory, -1];
       If[p[[1]]=!=currentLoadedPath&&p[[2]]=!=searchString,
       linkFuture = 
        Prepend[linkFuture, 
         {currentLoadedPath, helpBrowserPathGetURI[currentLoadedPath]}
         ];
       searchString = p[[2]];
       setDMVars[];
       helpBrowserSetNotebook[
        EvaluationNotebook[],
        searchString,
        currentLoadedPath,
        p[[1]],
        searchString,
        True
        ];
        CurrentValue[ 
         EvaluationNotebook[],
         {TaggingRules, "OldHelpBrowser", ".lock"}
         ] = False;
       ]
       ];
       Nothing
       ];
     setNBFuture = 
      Function[
       With[{p=linkFuture[[1]]},
       CurrentValue[ 
         EvaluationNotebook[],
         {TaggingRules, "OldHelpBrowser", ".lock"}
         ] = True;
       linkFuture = Drop[ linkFuture, 1];
       If[p[[1]]=!=currentLoadedPath&&p[[2]]=!=searchString,
       linkHistory = 
        Append[linkHistory, 
         {currentLoadedPath, helpBrowserPathGetURI[currentLoadedPath]}
         ];
       searchString = p[[2]];
       setDMVars[];
       helpBrowserSetNotebook[
        EvaluationNotebook[],
        searchString,
        currentLoadedPath,
        p[[1]],
        searchString,
        True
        ];
        CurrentValue[ 
         EvaluationNotebook[],
         {TaggingRules, "OldHelpBrowser", ".lock"}
         ] = False;
       ]
       ];
       Nothing
       ];
     panePicker =
      Function[
       helpBrowserPickerPane[
        EvaluationNotebook[],
        #,
        #2,
        panePathCached,
        listPickerLineHeight
        ]
       ]
     )
   ];


helpBrowserNotebook[
 path : {___String} : {},
 ops:OptionsPattern[]
 ] :=
  Notebook[{},
   DockedCells ->
    Cell[BoxData@ToBoxes@helpBrowserDockedCell[path],
     CellFrame -> {{0, 0}, {1, 0}},
     CellMargins -> None,
     CellFrameMargins -> {{0,0},{-6,0}},
     TextAlignment->Right
     ],
   ops,
   StyleDefinitions->$helpBrowserStyleDefinitions,
   System`ClosingSaveDialog -> False,
   Saveable -> False,
   WindowTitle -> "Help Browser",
   WindowSize->{808,755},
   WindowMargins->
    MapThread[
     {Floor[Abs[Subtract@@#]/2.-#2],Automatic}&,
     {
      AbsoluteCurrentValue[$FrontEndSession, "ScreenRectangle"],
      {404,377}
      }
     ]
   ];


(* ::Subsubsection:: *)
(*OpenHelpBrowser*)


helpBrowserOverrideDocumentationHelpLookup[]:=
 If[MatchQ[$helpBrowser, _NotebookObject?(NotebookRead[#] =!= $Failed &)],
  SelectedNotebook[]===$helpBrowser,
  Unprotect[Documentation`HelpLookup];
  (Documentation`HelpLookup[s_String]/;helpBrowserOverrideDocumentationHelpLookup[])=.;
  Protect[Documentation`HelpLookup];
  ]


OpenHelpBrowser[path : {___String} | Automatic : Automatic] :=
 If[
  MatchQ[$helpBrowser, _NotebookObject?(NotebookRead[#] =!= $Failed &)],
  SetOptions[$helpBrowser, {
    WindowFloating -> True,
    Visible -> True
    }];
  SetOptions[$helpBrowser, WindowFloating -> False];
  SetSelectedNotebook@$helpBrowser;
  If[ListQ@path, helpBrowserSetPath[$helpBrowser, path]];
  $helpBrowser,
  Quiet@
   Check[
    loadCachedDocumentationData[],
    preLoadDocumentationMetadata[];
    cacheDocumentationData[]
    ];
   Unprotect[Documentation`HelpLookup];
  (Documentation`HelpLookup[s_String]/;helpBrowserOverrideDocumentationHelpLookup[]):=
   helpBrowserPacletLookup[$helpBrowser, s];
  Protect[Documentation`HelpLookup];
  $helpBrowser = 
   CreateDocument@
    helpBrowserNotebook[
     Replace[path, 
      Automatic:>
       Replace[CurrentValue[$FrontEndSession,HomePage],{
        "paclet:guide/WolframRoot"->{"Symbol","System`"},
        e_:>helpBrowserPacletGetPath[e]
        }]
       ]
      ]
  ];


OpenHelpBrowser[selectionFunction:Except[_?OptionQ|_?StringPattern`StringPatternQ]]:=
 OpenHelpBrowser[helpBrowserPacletGetPath[#[[1]]]]&@
 MinimalBy[StringLength[#["title"]]&]@
  $helpSearcherDocMetadataDS[
   Select[selectionFunction],
   {"title", "uri"}
   ]


OpenHelpBrowser[selectionFunction:_?StringPattern`StringPatternQ]:=
 OpenHelpBrowser[helpBrowserPacletGetPath[#[[1]]]]&@
 MinimalBy[StringLength[#["title"]]&]@
  $helpSearcherDocMetadataDS[
   Select[selectionFunction],
   {"title", "uri"}
   ]


OpenHelpBrowser[
 p_?StringPattern`StringPatternQ
 ]:=
 OpenHelpBrowser[
  StringMatchQ[#["title"], p]&
  ]


OpenHelpBrowser[
 ops_?OptionQ
 ]:=
 HelpPagesSearch[
  With[{
  coreTest=
   Map[
    With[{
     prop=#[[1]],
     val=#[[2]]
     },
     If[StringPattern`StringPatternQ@val,
       StringMatchQ[#[prop],val,IgnoreCase->True]&,
       val@#[prop]&
       ]
     ]&,
    Flatten@{ops}
    ]
   },
   Replace[Thread[coreTest, Function],
    Function[{t__}]:>Function[And[t]]
    ]
   ]
  ]


(* ::Subsubsection::Closed:: *)
(*HelpPagesSearch*)


HelpPagesSearch[
 selectionFunction:Except[_?OptionQ|_?StringPattern`StringPatternQ], 
 openInBrowser:True|False:False
 ] :=
 If[openInBrowser,
   Hyperlink[#, "paclet:" <> #2,
    ButtonFunction->
     Function@OpenHelpBrowser[helpBrowserPacletGetPath[#]],
    Evaluator->"Local"
    ]&,
   Hyperlink[#, "paclet:" <> #2] &
   ] @@@
  SortBy[StringLength[#["title"]]&]@
  $helpSearcherDocMetadataDS[
   Select[selectionFunction],
   {"title", "uri"}
   ]


HelpPagesSearch[
 p_?StringPattern`StringPatternQ, 
 openInBrowser:True|False:False
 ]:=
 HelpPagesSearch[
  StringMatchQ[#["title"], p]&,
  openInBrowser
  ]


HelpPagesSearch[
 ops_?OptionQ, 
 openInBrowser:True|False:False
 ]:=
 HelpPagesSearch[
  With[{
  coreTest=
   Map[
    With[{
     prop=#[[1]],
     val=#[[2]]
     },
     If[StringPattern`StringPatternQ@val,
       StringMatchQ[#[prop],val,IgnoreCase->True]&,
       val@#[prop]&
       ]
     ]&,
    Flatten@{ops}
    ]
   },
   Replace[Thread[coreTest, Function],
    Function[{t__}]:>Function[And[t]]
    ]
   ],
  openInBrowser
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
