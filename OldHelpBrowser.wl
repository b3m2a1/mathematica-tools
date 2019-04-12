(* ::Package:: *)

(*MakeIndentable["IndentCharacter"\[Rule]"  "]*)


(* ::Section:: *)
(*OldHelpBrowser*)


BeginPackage["OldHelpBrowser`", {"PacletManager`"}];


(*Package Declarations*)
OpenHelpBrowser::usage="
OpenHelpBrowser[] opens an old-style help browser
OpenHelpBrowser[path] opens an old-style help browser intialized with path
OpenHelpBrowser[crit] opens for the first hit for search criterion crit
OpenHelpBrowser[\"Key\"->key, ...] opens based on the cached data for key
";
HelpPagesSearch::usage="
HelpPagesSearch[test] implements a search over documentation pages
HelpPagesSearch[test, True] has the pages open in the HelpBrowser
HelpPagesSearch[\"Key\"->key, ...] works with the cached data for key
";


(* ::Subsubsection:: *)
(*Private Declarations*)


AppendTo[$ContextPath, $Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
$versionNumber::usage="$versionNumber";
$documentationLoadMonitor::usage="If True documentation loading is done verbosely";
documentationLoadDirectories::usage="documentationLoadDirectories[]
documentationLoadDirectories[key]";
loadDocumentationData::usage="loadDocumentationData[]
loadDocumentationData[key]";
loadDocumentationMetadata::usage="loadDocumentationMetadata[]
loadDocumentationMetadata[key]";
ensureLoadedDocumentationMetadata::usage="ensureLoadedDocumentationMetadata[]
ensureLoadedDocumentationMetadata[key]";
preLoadDocumentationMetadata::usage="preLoadDocumentationMetadata[]
preLoadDocumentationMetadata[key]";
$cachePath::usage="The path for LocalObject doc data caches";
cacheFind::usage="Finds a cache LocalObject for a given key";
loadCachedDocumentationData::usage="loadCachedDocumentationData[]";
cacheDocumentationData::usage="cacheDocumentationData[]";
clearCachedDocumentationData::usage="clearCachedDocumentationData[]";
$documentationLoadDirectories::usage="The set of directories from which to load";
$helpSearcherDocData::usage="The core doc data Association";
$helpSearcherDocMetadataDS::usage="$helpSearcherDocMetadataDS";
helpSearcherDocMetadataDS::usage="helpSearcherDocMetadataDS[]
helpSearcherDocMetadataDS[key]";
$helpBrowserCoreDS::usage="$helpBrowserCoreDS";
helpBrowserCoreDS::usage="helpBrowserCoreDS[]
helpBrowserCoreDS[key]";
helpBrowserDSButton::usage="helpBrowserDSButton[entry, onClick]";
helpBrowserDS::usage="helpBrowserDS[formatFunction, onClick]";
helpBrowserPacletGetPath::usage="helpBrowserPacletGetPath[uri]";
helpBrowserPacletLookup::usage="helpBrowserPacletGetPath[browser, uri]";
helpSearcherDSNameSearch::usage="helpSearcherDSNameSearch[name, type]";
helpBrowserNameSearch::usage="helpBrowserNameSearch[browser, name, type]";
helpBrowserAutocomplete::usage="helpBrowserAutocomplete[s]";
helpBrowserAutocompleteFunction::usage="helpBrowserAutocomplete[s]";
helpBrowserSearch::usage="helpBrowserSearch[browser, name, type]";
helpBrowserDockedCell::usage="helpBrowserDockedCell[path]";
helpLookup::usage="helpLookup[s]
helpLookup[key, s]";
helpBrowserSetNotebook::usage="helpBrowserSetNotebook[browser, ...]";
helpBrowserNotebook::usage="helpBrowserNotebook[path]";


End[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


$versionNumber="1.2.0";


(* ::Subsubsection:: *)
(*FileName Utils*)


fileNameForm[None, {path___, name_}]:=
	FrontEnd`FileName[{path},
   name
   ];
fileNameForm[Hold[head_], {path___, name_}]:=
	FrontEnd`FileName[{head, path},
   name
   ];
fileNameForm[head_, {path___, name_}]:=
	FrontEnd`FileName[{head, path},
   name
   ];
fileNameForm~SetAttributes~HoldFirst


$fileNameSplitReps=
	Function[Null, #->Hold[#], HoldAllComplete]/@
	Hold[
		$UserBasePacletsDirectory,
		$UserBaseDirectory,
		$UserDocumentsDirectory,
		$BasePacletsDirectory,
		$BaseDirectory
		]//ReleaseHold//Association


fileNameSplit[f_]:=
  Replace[
    Catch@
    KeyValueMap[
      If[StringStartsQ[f, #],
        Throw@fileNameForm[
          #2,
          DeleteCases[""]@FileNameSplit@
            StringTrim[f, #]
          ]
        ]&,
      $fileNameSplitReps
      ],
   Except[_FrontEnd`FileName]:>
    fileNameForm[
      None, 
      DeleteCases[""]@FileNameSplit@f
      ]
   ]


(* ::Subsubsection:: *)
(*Load Data*)


If[!AssociationQ@$documentationLoadDirectories,
  $documentationLoadDirectories =
  	<|
    	"SystemDocs":>
    	  Set[
    	    $documentationLoadDirectories["SystemDocs"],
    	    Join[
           DeleteDuplicatesBy[#[[1]]["Name"] &]@
           Map[
             With[{ loc = FileNameJoin@{ #["Location"], "Documentation" }},
               If[DirectoryQ[loc],
                 # -> loc,
                 Nothing
                ]
             ] &,
             PacletManager`PacletFind["*"]
             ],
           {"System"->FileNameJoin[{$InstallationDirectory, "Documentation"}]}
           ]
          ]
      |>
  ];


documentationLoadDirectories[key:_String:"SystemDocs"]:=
	If[KeyMemberQ[$documentationLoadDirectories, key],
    $documentationLoadDirectories[key],
    $documentationLoadDirectories[key] =
      Map[
       With[{ loc = FileNameJoin@{ #["Location"], "Documentation" }},
         If[DirectoryQ[loc],
           # -> loc,
           Nothing
          ]
         ]&,
       PacletManager`PacletFind[key]
       ]
    ]


$documentationLoadMonitor = True;


If[!AssociationQ@$helpSearcherDocData,
  $helpSearcherDocData =
  	<||>
  ];


helpSearcherDocData[key:_String:"SystemDocs"]:=
	If[KeyMemberQ[$helpSearcherDocData, key],
	  $helpSearcherDocData[key],
	  loadDocumentationData[key]
	  ]


loadDocumentationData[
  key:_String:"SystemDocs",
	mon:True|False|Automatic:Automatic
	] :=
 If[Replace[mon, Automatic:>$documentationLoadMonitor],
   Function[
     Null,
     Monitor[#, Internal`LoadingPanel["Loading documentation pages"]],
     HoldAllComplete
     ],
   Identity
   ][
   $helpSearcherDocData[key] =
    Merge[
      {
        Replace[
          $helpSearcherDocData[key], 
          Except[_Association?AssociationQ]->
            <||>
          ],
        Append[#,
         "Pages" ->
          Apply[Join]@
            Map[FileNames["*.nb", Last[#], \[Infinity]] &, #["Directories"]]
          ] &@ 
        		<|
             "Directories" ->
               documentationLoadDirectories[key]
             |>
        },
      Last
      ]
    ];


mergeTag//Clear;
mergeTag[e:Except[_Function]]:=e


loadDocumentationMetadata[
  key:_String:"SystemDocs", 
	mon:True|False|Automatic:Automatic
	] :=
 If[Replace[mon, Automatic:>$documentationLoadMonitor], 
   Function[
     Null,
     Monitor[#, Internal`LoadingPanel["Prepping documentation metadata"]],
     HoldAllComplete
     ],
   Identity
   ][
  If[! AssociationQ@$helpSearcherDocData[key], loadDocumentationData[key]];
  (* Merge old metadata so index can quickly be updated *)
  $helpSearcherDocData[key, "Metadata"] =
    ReplaceAll[
      HoldPattern[mergeTag[f_][a_]]:>
        RuleCondition[f[a], True]
      ]@
    Merge[
       {
         Lookup[$helpSearcherDocData[key], "Metadata", <||>],
         Association@
          Map[
           Function[
             RuleDelayed[
               #,
               Set[
                $helpSearcherDocData[key, "Metadata", #],
                Append[
                 Fold[Association@Lookup[#, #2, {}] &, 
                  Options[Get[#]], {TaggingRules, "Metadata"}],
                 "File" -> #
                 ]
                ]
              ]
             ],
           $helpSearcherDocData[key, "Pages"]
           ]
         },
       mergeTag@
       Function[Null,
         Replace[
           Hold[#],
           {
             Hold[{___, a_Association, Except[_Association]}]:>a,
             Hold[{___, e_Set}]:>Hold[e],
             Hold[{___, e_}]:>Unevaluated[e]
             }
           ],
         HoldAllComplete
         ]
       ]
  ]


ensureLoadedDocumentationMetadata[key:_String:"SystemDocs", 
  mon:True|False|Automatic:Automatic
  ] :=
 If[
   !AssociationQ@$helpSearcherDocData[key] || 
     !KeyMemberQ[$helpSearcherDocData[key], "Metadata"],
  loadDocumentationMetadata[key, mon]
  ]


preLoadDocumentationMetadata[
  key:_String:"SystemDocs", 
  mon:True|False|Automatic:Automatic
  ] :=
 (
  ensureLoadedDocumentationMetadata[key, mon]; 
  If[Replace[mon, Automatic:>$documentationLoadMonitor], 
   Function[
     Null,
     Monitor[#, Internal`LoadingPanel["Extracting metadata"]],
     HoldAllComplete
     ],
   Identity
   ]@Scan[ReleaseHold, $helpSearcherDocData[key, "Metadata"]]
  )


(* ::Subsubsection:: *)
(*Cache Data*)


cachePathLoad[]:=
  Map[
		URLBuild[
			<|
				"Scheme"->"file",
				"Path"->Flatten@{#, "OldHelpBrowser", "Caches"}
				|>
			]&,
		Join[
		  Thread[
		    {
		      Select[
    		    Lookup[
      		    PacletInformation/@PacletManager`PacletFind["*"],
      		    "Location"
      		    ],
      		  DirectoryQ@FileNameJoin@{#, "OldHelpBrowser", "Caches"}&
           ],
         "Resources"
        }
      ],
			{
			  {$UserBaseDirectory, "ApplicationData"}
				}
			]
		];
$cachePath:=$cachePath=cachePathLoad[]


cacheFind[key:_String:"SystemDocs", localBase_:Automatic]:=
	With[{k=ToString[key]},
  	Replace[
  	  Catch@
  	    Map[
      		If[DirectoryQ@FileNameJoin@URLParse[LocalObject[k, #][[1]], "Path"],
      			Throw@LocalObject[k, #]
      			]&,
      		Flatten@List@
      		  Replace[localBase,
        			Automatic:>
        				$cachePath
        			]
      		],
    	Except[_LocalObject]:>
    	  If[key=="SystemDocs"&&
    	    DirectoryQ@FileNameJoin@URLParse[LocalObject["docsDataCache"][[1]], "Path"],
    	    LocalObject["docsDataCache"],
    	    LocalObject[k, URLBuild@{$LocalBase, "OldHelpBrowser", "Caches" }] 
    	    ]
    	]
    ];


prepExportCacheDocumentationData[baseData_]:=
	Module[
		{dats=baseData},
		dats =
		  MapAt[
  		  Map[ 
  		    DeleteCases[ #[[1]], "Location"->_] -> 
  		    If[StringQ[#[[2]]], fileNameSplit[#[[2]]], #[[2]]] & 
  		    ],
    		dats,
    		"Directories"
    		];
   dats = 
     MapAt[
  		  Map[
  		    MapAt[ If[StringQ[#], fileNameSplit[#], #]&, #, "File"]&,
  		    KeyMap[ If[StringQ[#], fileNameSplit[#], #]&, #]
  		    ]&,
    		dats,
    		"Metadata"
    		];
   dats = 
     MapAt[
  		  Map[If[StringQ[#], fileNameSplit[#], #]&],
    		dats,
    		"Pages"
    		];
   dats
   ];


cleanLoadedCacheDocumentationData[key:_String:"SystemDocs"]:=
	Module[
		{dats=$helpSearcherDocData[key]},
		dats =
		  MapAt[
  		  Map[ #[[1]] -> If[!StringQ[#[[2]]], ToFileName[#[[2]]], #[[2]]] & ],
    		dats,
    		"Directories"
    		];
   dats = 
     MapAt[
  		  Map[
  		    MapAt[ If[!StringQ[#], ToFileName[#], #]& , #, "File"]&,
  		    KeyMap[ If[!StringQ[#], ToFileName[#], #]&, #]
  		    ]&,
    		dats,
    		"Metadata"
    		];
   dats = 
     MapAt[
  		  Map[If[!StringQ[#], ToFileName[#], #]&],
    		dats,
    		"Pages"
    		];
   $helpSearcherDocData[key] = dats
   ];


loadCachedDocumentationData::corrupt="Cache has been corrupted. It will be ignored.";
loadCachedDocumentationData[key:_String:"SystemDocs", localBase_:Automatic] :=
  Catch[
   $helpSearcherDocData[key] = 
     Replace[
      Get[cacheFind[key, localBase]],
      Except[_Association|_Association]?(
       !AssociationQ[#["Metadata"]]||Length[#["Metadata"]]===0
       &):>
       (Message[loadCachedDocumentationData::corrupt];Throw@$Failed)
      ];
   cleanLoadedCacheDocumentationData[key]
   ];


cacheDocumentationData[key:_String:"SystemDocs", localBase_:Automatic] :=
  Put[prepExportCacheDocumentationData@$helpSearcherDocData[key], 
    cacheFind[key, localBase]
    ];


clearCachedDocumentationData[key:_String:"SystemDocs", localBase_:Automatic] :=
  Quiet@DeleteFile@cacheFind[key, localBase];


(* ::Subsubsection:: *)
(*Docs Metadata Dataset*)


helpSearcherDocMetadataDSLoad[key:_String:"SystemDocs"]:=
  Set[
    $helpSearcherDocMetadataDS[key],
    If[
     !AssociationQ@$helpSearcherDocData[key "Metadata"]||
      MatchQ[Normal@Take[$helpSearcherDocData[key, "Metadata"], 3], {___RuleDelayed}],
     preLoadDocumentationMetadata[key]
     ];
    Dataset@
     Select[Values@$helpSearcherDocData[key, "Metadata"], KeyMemberQ["uri"]]
    ];


If[!AssociationQ@$helpSearcherDocMetadataDS,
	$helpSearcherDocMetadataDS=<||>
	];


helpSearcherDocMetadataDS[key:_String:"SystemDocs"]:=
	If[!KeyMemberQ[$helpSearcherDocMetadataDS, key],
		helpSearcherDocMetadataDSLoad[key],
		$helpSearcherDocMetadataDS[key]
		]


(* ::Subsubsection:: *)
(*Help Browser Tree*)


helpBrowserCoreDSLoad[key:_String:"SystemDocs"]:=
  $helpBrowserCoreDS[key] =
    Map[
     Association@*
      Map[
       Lookup[#, "title"] -> #
        &
       ]
     ] /@  
     GroupBy[
      Normal@helpSearcherDocMetadataDS[key],
      Key["type"] -> KeyDrop["type"],
      GroupBy[Key["context"] -> KeyDrop["context"]]
      ];


If[!AssociationQ@$helpBrowserCoreDS,
	$helpBrowserCoreDS=<||>
	];


helpBrowserCoreDS[key:_String:"SystemDocs"]:=
	If[!KeyMemberQ[$helpBrowserCoreDS, key],
		helpBrowserCoreDSLoad[key],
		$helpBrowserCoreDS[key]
		]


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
  key:_String:"SystemDocs",
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
     helpSearcherDocMetadataDS[key],
     Key["type"] -> KeyDrop["type"],
     GroupBy[Key["context"] -> KeyDrop["context"]]
     ]


(* ::Subsubsection:: *)
(*Autocomplete*)


helpBrowserAutocompleteLoad[key:_String:"SystemDocs"]:=
 $helpBrowserAutocomplete[key] = 
  Autocomplete[
   Keys@
    With[{syms=helpBrowserCoreDS[key]["Symbol"]},
      If[KeyMemberQ[syms, "System`"],
        syms["System`"],
        syms[[1]]
        ]
     ]
   ];


If[!AssociationQ@$helpBrowserAutocomplete,
	$helpBrowserAutocomplete = <||>
	]


helpBrowserAutocomplete[key:_String:"SystemDocs"]:=
	If[!KeyMemberQ[$helpBrowserAutocomplete, key],
		helpBrowserAutocompleteLoad[key],
		$helpBrowserAutocomplete[key]
		]


helpBrowserAutocompleteFunction[key:_String:"SystemDocs"][e___]:=
  helpBrowserAutocomplete[key][e]


(* ::Subsubsection:: *)
(*Constants*)


$helpBrowserTaggingRulesPath={TaggingRules,"OldHelpBrowser","Path"};
$helpBrowserTaggingRulesKey={TaggingRules,"OldHelpBrowser","Key"};


(* ::Subsubsection:: *)
(*SetBrowserPath*)


helpBrowserSetPath[browser_, path_List, key:_String:"SystemDocs"]:=
 (
   CurrentValue[browser, $helpBrowserTaggingRulesKey] = key;
   CurrentValue[browser, $helpBrowserTaggingRulesPath] = path;
   )


(* ::Subsubsection:: *)
(*Search*)


helpSearcherDSNameSearch[key:_String:"SystemDocs", name_, type_:"Symbol"]:=
 SortBy[
  Normal@Select[helpSearcherDocMetadataDS[key],
   #type===type&&StringContainsQ[#title, name]&
   ],
  StringLength[#["title"]]&
  ]


helpBrowserNameSearch[key:_String:"SystemDocs", browser_, name_, type_:"Symbol"]:=
  With[
    {
      result=Replace[helpSearcherDSNameSearch[key, name,type], {a_,___}:>a]
      },
    If[AssociationQ@result,
      helpBrowserSetPath[
       browser, 
       Lookup[result, {"type", "context", "title"}],
       key
       ]
      ]
    ]


(* ::Subsubsection:: *)
(*PacletLookup*)


helpBrowserPathGetURI[key:_String:"SystemDocs", path_]:=
 Replace[
  helpBrowserCoreDS[key] @@ path, {
   a_Association?(KeyMemberQ["uri"]):>
    (a["uri"])
   }]


helpBrowserPacletGetPath[key:_String:"SystemDocs", pacletURI_]:=
 With[{
   baseFile=Documentation`ResolveLink[pacletURI],
   met=helpSearcherDocData[key]["Metadata"]
   },
   If[StringQ@baseFile,
     Lookup[
       Lookup[met, baseFile,
         Lookup[met, 
           fileNameSplit[baseFile],
           <|"type"->Nothing,"context"->Nothing,"title"->Nothing|>
           ]
         ],
       {"type","context","title"}
       ],
     {}
     ]
   ]


helpBrowserPacletLookup[key:_String:"SystemDocs", browser_ ,pacletURI_]:=
  With[{
    new=helpBrowserPacletGetPath[key, pacletURI],
    current=CurrentValue[browser, $helpBrowserTaggingRulesPath]
    },
    If[current===new,
     If[StringContainsQ[pacletURI, "#"],
      NotebookFind[$helpBrowserTaggingRulesPath, 
       StringSplit[pacletURI,"#"][[-1]], Next, CellID, AutoScroll -> Top]
      ],
     helpBrowserSetPath[browser, new, key]
     ]
    ];


(* ::Subsubsection:: *)
(*HelpLookup*)


helpLookup[key:_String:"SystemDocs", s_String, ops___?OptionQ]:=
 helpBrowserPacletLookup[
   CurrentValue[$helpBrowsers[key], 
     {TaggingRules, "OldHelpBrowser", "Key"}
     ],
   $helpBrowsers[key],
   s]


(* ::Subsubsection:: *)
(*HelpBrowserSearch*)


helpBrowserSearch[key:_String:"SystemDocs", browser_, name_, type_:"Symbol"]:=
 With[{pl=Documentation`ResolveLink[name]},
  If[pl=!=Null&&!
   StringStartsQ[pl,
    FileNameJoin@{PacletManager`$UserBasePacletsDirectory, "Temporary"}
    ],
   helpBrowserPacletLookup[key, browser, name],
   helpBrowserNameSearch[key, browser, name, type]
   ]
  ]


(* ::Subsubsection:: *)
(*Styles*)


(* ::Text:: *)
(*Sets the styles for the PanePicker Notebook*)


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
     Apply[
      helpBrowserPacletLookup,
      Prepend[
        Reverse@Last@CurrentValue["EventData"],
        CurrentValue[EvaluationNotebook[], 
          {TaggingRules, "OldHelpBrowser", "Key"}
          ]
        ]
      ]
    }
  ],
 Cell[StyleData["Link"],
  ButtonBoxOptions->{
   ButtonFunction:>
    Function[
     KernelExecute[
      helpBrowserPacletLookup[
        CurrentValue[EvaluationNotebook[], 
          {TaggingRules, "OldHelpBrowser", "Key"}
          ],
        EvaluationNotebook[],
        #
        ]
      ]
     ],
   Evaluator->"Local"
   }
  ]
 },
 StyleDefinitions->"PrivateStylesheetFormatting.nb"
 ];


(* ::Subsubsection:: *)
(*PanePicker*)


(* ::Text:: *)
(*Implements a PanePicker for choosing the current doc page*)


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


(* ::Subsubsection:: *)
(*SetNotebook*)


helpBrowserSetNotebook[
 key_,
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
      "Key" -> key,
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
          helpBrowserPathGetURI[key, Lookup[#, "LoadedPath", {}]]
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
     ]&@
     CurrentValue[EvaluationNotebook[],
       {TaggingRules, "OldHelpBrowser"}, 
       FirstCase[
         CurrentValue[EvaluationNotebook[], DockedCells],
         HoldPattern[Set[_, TaggingRules->v_]]:>v,
         <||>,
         Infinity
         ]
       ],
   sd=$helpBrowserStyleDefinitions,
   cleanURI="paclet:"<>StringTrim[uri,"paclet:"],
   enb=nb
   },
   Replace[Documentation`ResolveLink[cleanURI],{
    f_String:>
     With[
       {
         put=
           Get[f]/.
             HoldPattern[Documentation`HelpLookup[a__]]:>helpLookup[key, a]
           },
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
           DeleteDuplicatesBy[First]@Join[
            {
              "NewStyles"->$VersionNumber>=11.1
              },
            Lookup[Options[put], TaggingRules, {}], 
            {
              "OldHelpBrowser"->tr
              }
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


(* ::Subsubsection:: *)
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
           (helpBrowserPacletLookup[
              CurrentValue[nb, 
                {TaggingRules, "OldHelpBrowser", "Key"}
                ],
              nb, e
              ]),
          HoldPattern[Documentation`HelpLookup[e_,n_]]:>
           (helpBrowserPacletLookup[
              CurrentValue[n, 
                {TaggingRules, "OldHelpBrowser", "Key"}
                ],
              n, e
              ])
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
      FrontEnd`SetOptions[#,
       {
         Deletable -> True,
         Editable->True
         }
        ]&,
      c
      ],
     {
      FrontEnd`NotebookDelete[c],
      FrontEnd`SetOptions[nb,
       {
         Selectable->True,
         Editable->True,
         Deployed->False
         }
       ],
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


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
(*XIcon*)


$helpBrowserXIcon=
  {
    "Default"->
       ToExpression@FrontEndResource["FEBitmaps","CircleXIcon"],
    "Hover"->
       ToExpression@FrontEndResource["FEBitmaps","CircleXIconHighlight"],
    "Pressed"->
       ToExpression@FrontEndResource["FEBitmaps","CircleXIconPressed"]
    };


(* ::Subsubsection:: *)
(*Docked Cell*)


Off[General::shdw];
System`WholeCellGroupOpener;
On[General::shdw];


helpBrowserDockedCell[key:_String:"SystemDocs", path : _List : {}] :=
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
    autoCompleteLastFill,
    taggingRules
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
               Spacer[{2, 15}]
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
              helpBrowserSearch[key, EvaluationNotebook[], searchString](*,
            PassEventsDown\[Rule]True*)
            }],
           Button["",
            helpBrowserSearch[key, EvaluationNotebook[], searchString],
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
                 helpBrowserCoreDS[key] @@ Take[panePathCached, #2[[1]]], {
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
                Sort@Keys[helpBrowserCoreDS[key]],
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
         helpBrowserCoreDS[key] @@ panePathCached, {
          a_Association?(KeyMemberQ["uri"]):>
           (setNB[a["uri"]])
         }]
        ];
     Button[
      Column[{
       Row[{"","Show Browser"}, Spacer[{2, 15}]],
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
        {TaggingRules, "OldHelpBrowser", ".loc-k"}
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
       With[
         {
          core=
           Replace[
             CurrentValue[EvaluationNotebook[], 
               {TaggingRules, "OldHelpBrowser"}],
             Inherited:>
              Set[
                CurrentValue[EvaluationNotebook[], 
                 {TaggingRules, "OldHelpBrowser"}],
                Normal@KeyDrop[
                  Lookup[
                    Replace[taggingRules, 
                      Except[_Rule]:>taggingRules
                      ],
                    TaggingRules, 
                    {}
                    ],
                  {"Path", "LoadedPath", "SearchString"}
                  ]
               ]
             ]
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
         Lookup[core, "BrowserVisible"],
         {
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
           "Key",
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
            key,
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
        taggingRules=`temp`taggingRules=TaggingRules->varMap;
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
        key,
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
         {currentLoadedPath, helpBrowserPathGetURI[key, currentLoadedPath]}
         ];
       searchString = p[[2]];
       setDMVars[];
       helpBrowserSetNotebook[
        key,
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
         {currentLoadedPath, helpBrowserPathGetURI[key, currentLoadedPath]}
         ];
       searchString = p[[2]];
       setDMVars[];
       helpBrowserSetNotebook[
        key,
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
 key:_String:"SystemDocs",
 path : {___String} : {},
 ops:OptionsPattern[]
 ] :=
  Notebook[{},
   DockedCells ->
    Cell[BoxData@ToBoxes@helpBrowserDockedCell[key, path],
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


If[!AssociationQ@$helpBrowsers,
 $helpBrowsers=<||>
 ];


helpBrowserOverrideDocumentationHelpLookup[key:_String:"SystemDocs"]:=
 If[MatchQ[$helpBrowsers[key], _NotebookObject?(NotebookRead[#] =!= $Failed &)],
  SelectedNotebook[]===$helpBrowsers[key],
  Unprotect[Documentation`HelpLookup];
  Quiet[
   (
     Documentation`HelpLookup[s_String, ops___]/;
        helpBrowserOverrideDocumentationHelpLookup[key]
      )=.
    ];
  Protect[Documentation`HelpLookup];
  ]


OpenHelpBrowser//Clear


OpenHelpBrowser[
  k: ("Key" -> _String|"SystemDocs") : ("Key"-> "SystemDocs"),
  path : (_String?(StringStartsQ["paclet:"]) | {___String} | Automatic) : Automatic
  ]:=
 With[{key="Key"/.k},
   Block[{$documentationLoadMonitor=!KeyMemberQ[$helpSearcherDocData, key]},
     If[
      MatchQ[$helpBrowsers[key], _NotebookObject?(NotebookRead[#] =!= $Failed &)],
      SetOptions[$helpBrowsers[key], {
        WindowFloating -> True,
        Visible -> True
        }];
      SetOptions[$helpBrowsers[key], WindowFloating -> False];
      SetSelectedNotebook@$helpBrowsers[key];
      If[ListQ@path, helpBrowserSetPath[$helpBrowsers[key], path, key]];
      $helpBrowsers[key],
      Quiet@
       Check[
        loadCachedDocumentationData[key],
        preLoadDocumentationMetadata[key];
        cacheDocumentationData[key]
        ];
      (*Unprotect[Documentation`HelpLookup];
      (
        Documentation`HelpLookup[s_String, ops___]/;
          helpBrowserOverrideDocumentationHelpLookup[key]
        ):=
       helpBrowserPacletLookup[
         CurrentValue[$helpBrowsers[key], 
           {TaggingRules, "OldHelpBrowser", "Key"}
           ],
         $helpBrowsers[key],
         s];
      DownValues[Documentation`HelpLookup]=
        SortBy[DownValues[Documentation`HelpLookup], 
          FreeQ[_Condition]
          ];
      Protect[Documentation`HelpLookup];*)
      $helpBrowsers[key] = 
       CreateDocument@
        helpBrowserNotebook[
         key,
         Replace[path,
          {
            s_String:>
             helpBrowserPacletGetPath[key, s],
            Automatic:>
             Replace[CurrentValue[$FrontEndSession, HomePage],
              {
                "paclet:guide/WolframRoot"->
                  If[key==="SystemDocs", 
                    {"Symbol", "System`"}, 
                    {"Symbol", 
                      If[KeyMemberQ[$helpBrowserCoreDS, key],
                        Replace[
                          Keys[$helpBrowserCoreDS[key, "Symbol"]],
                          {
                            {s_, ___}:>s,
                            {}->Nothing
                            }
                          ],
                        Nothing 
                        ]
                      }
                    ],
                e_:>
                  helpBrowserPacletGetPath[key, e]
                }
               ]
            }
          ]
        ]
      ]
    ]
  ];


OpenHelpBrowser[
  k: ("Key" -> _String|"SystemDocs") : ("Key"-> "SystemDocs"),
  selectionFunction:Except[_?OptionQ|_?StringPattern`StringPatternQ]
  ]:=
 With[{key="Key"/.k},
   OpenHelpBrowser[k, helpBrowserPacletGetPath[key, #[[1]]]]&@
   MinimalBy[StringLength[#["title"]]&]@
    helpSearcherDocMetadataDS[key][
     Select[selectionFunction],
     {"title", "uri"}
     ]
   ]


OpenHelpBrowser[
  k: ("Key" -> _String|"SystemDocs") : ("Key"-> "SystemDocs"),
  p_?StringPattern`StringPatternQ
  ]:=
  OpenHelpBrowser[
    k,
    StringMatchQ[#["title"], p]&
    ]


OpenHelpBrowser[
  k: ("Key" -> _String|"SystemDocs") : ("Key"-> "SystemDocs"),
  ops_?OptionQ
  ]:=
  HelpPagesSearch[
    k,
    With[
      {
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


(* ::Subsubsection:: *)
(*HelpPagesSearch*)


HelpPagesSearch//Clear


HelpPagesSearch[
  k: ("Key" -> _String|"SystemDocs") : ("Key"-> "SystemDocs"),
  selectionFunction:Except[_?OptionQ|_?StringPattern`StringPatternQ], 
  openInBrowser:True|False:False
  ] :=
 With[{key="Key"/.k},
   If[openInBrowser,
     Hyperlink[#, "paclet:" <> #2,
      ButtonFunction->
       Function@OpenHelpBrowser[helpBrowserPacletGetPath[key, #]],
      Evaluator->"Local"
      ]&,
     Hyperlink[#, "paclet:" <> #2] &
     ] @@@
    SortBy[StringLength[#["title"]]&]@
    helpSearcherDocMetadataDS[key][
     Select[selectionFunction],
     {"title", "uri"}
     ]
   ]


HelpPagesSearch[
  k: ("Key" -> _String|"SystemDocs") : ("Key"-> "SystemDocs"),
  p_?StringPattern`StringPatternQ, 
  openInBrowser:True|False:False
  ]:=
  HelpPagesSearch[
    k,
    StringMatchQ[#["title"], p]&,
    openInBrowser
    ]


HelpPagesSearch[
  k: ("Key" -> _String|"SystemDocs") : ("Key"-> "SystemDocs"),
  ops_?OptionQ, 
  openInBrowser:True|False:False
  ]:=
  HelpPagesSearch[
    k,
    With[
      {
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
