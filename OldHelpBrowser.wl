(* ::Package:: *)

(* ::Section:: *)
(*OldHelpBrowser*)


BeginPackage["OldHelpBrowser`"];


(*Package Declarations*)
OpenHelpBrowser::usage="
OpenHelpBrowser[] opens a help browser
OpenHelpBrowser[path] opens a help browser initialized with path
";


(* ::Subsubsection:: *)
(*Private Declarations*)


AppendTo[$ContextPath,$Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
loadDocumentationData::usage="loadDocumentationData[]";
loadDocumentationMetadata::usage="loadDocumentationMetadata[]";
ensureLoadedDocumentationMetadata::usage="ensureLoadedDocumentationMetadata[]";
preLoadDocumentationMetadata::usage="preLoadDocumentationMetadata[]";
loadCachedDocumentationData::usage="loadCachedDocumentationData[]";
cacheDocumentationData::usage="cacheDocumentationData[]";
$helpSearcherDocMetadataDS::usage="$helpSearcherDocMetadataDS";
helpBrowserCoreDS::usage="helpBrowserCoreDS";
helpBrowserDSButton::usage="helpBrowserDSButton[entry, onClick]";
helpBrowserDS::usage="helpBrowserDS[formatFunction, onClick]";
helpBrowserDockedCell::usage="helpBrowserDockedCell[path]";
helpBrowserNotebook::usage="helpBrowserNotebook[path]";


End[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


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
     DeleteDuplicatesBy[#[[1]]["Name"] &]@
      Select[DirectoryQ@*Last]@
       Map[
        # -> FileNameJoin[{#["Location"], "Documentation"}] &,
        PacletFind["*"]
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
          Options[Import[#]], {TaggingRules, "Metadata"}],
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
 \
(ensureLoadedDocumentationMetadata[]; 
  Scan[Identity, $helpSearcherDocData["Metadata"]])


(* ::Subsubsection::Closed:: *)
(*Cache Data*)


loadCachedDocumentationData[] :=
  $helpSearcherDocData = 
   Get[LocalObject["docsDataCache"]];
cacheDocumentationData[] :=
  
  Put[$helpSearcherDocData, LocalObject["docsDataCache"]];


(* ::Subsubsection::Closed:: *)
(*Docs Metadata Dataset*)


$helpSearcherDocMetadataDS :=
  $helpSearcherDocMetadataDS =
   (
    preLoadDocumentationMetadata[];
    Dataset@
     Select[Values@$helpSearcherDocData["Metadata"], KeyMemberQ["uri"]]
    );


(* ::Subsubsection::Closed:: *)
(*Help Browser Tree*)


helpBrowserCoreDS :=
  helpBrowserCoreDS =
   GroupBy[
    $helpSearcherDocMetadataDS,
    Key["type"] -> KeyDrop["type"],
    GroupBy[Key["context"] -> KeyDrop["context"]]
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
   ] /@ helpBrowserCoreDS


(* ::Subsubsection::Closed:: *)
(*Docked Cell*)


helpBrowserDockedCell[path : _List : {}] :=
  DynamicModule[
   {
    panePath = path,
    panePicker,
    coreDS,
    setNB,
    showBrowser = True
    },
   coreDS = 
    Normal@helpBrowserDS["paclet:" <> Lookup[#, {"uri"}, ""] &, 
      Null];
   panePicker =
    Function@
     With[{
       choices = #, idx = #2
       },
      ListPicker[
       Dynamic[
        If[Length@panePath >= idx, panePath[[{idx}]]],
        Function@
         Set[
          panePath,
          ReplacePart[
           If[Length@panePath >= idx,
            Take[panePath, idx],
            PadRight[panePath, idx, ""]
            ], idx -> First@#]
          ]
        ],
       choices,
       ImageSize -> {150, 150},
       Background -> {{GrayLevel[.95], White}}
       ]
      ];
   setNB =
    Function[
     CheckAll[
      FrontEndExecute@
       FrontEnd`NotebookSuspendScreenUpdates[EvaluationNotebook[]];
      SelectionMove[EvaluationNotebook[], All, Notebook];
      SetOptions[NotebookSelection[EvaluationNotebook[]], 
       Deletable -> True];
      NotebookDelete[EvaluationNotebook[]];
      With[{
        nb =
         DeleteCases[
           WindowSize | WindowMargins | DockedCells | 
             StyleDefinitions -> _]@
          Replace[Documentation`ResolveLink[#], {
            f_String?FileExistsQ :> Import[f],
            _ -> Notebook[{}]
            }]
        },
       NotebookWrite[
        EvaluationNotebook[],
        First@nb
        ];
       SetOptions[EvaluationNotebook[],
        Join[
         {
          StyleDefinitions ->
           
           FrontEnd`FileName[{"Wolfram"}, "Reference.nb", 
            CharacterEncoding -> "UTF-8"]
          },
         List @@ Rest@nb
         ]
        ]
       ];
      SelectionMove[EvaluationNotebook[], Before, Notebook];
      FrontEndExecute@
       FrontEnd`NotebookResumeScreenUpdates[EvaluationNotebook[]],
      FrontEndExecute@
       FrontEnd`NotebookResumeScreenUpdates[EvaluationNotebook[]]
      ];
     Nothing
     ];
   Dynamic@
    If[showBrowser,
     Column[{
       Button["Hide Browser",
        showBrowser = False,
        Appearance -> None,
        BaseStyle -> "Hyperlink"
        ],
       Row@
        Prepend[
         MapIndexed[
          Replace[
            coreDS @@ Take[panePath, #2[[1]]], {
             a_Association :> panePicker[a // Keys, #2[[1]] + 1],
             e_ :> setNB[e]
             }] &,
          panePath
          ],
         panePicker[Keys[coreDS], 1]
         ]
       }],
     Button["Show Browser",
      showBrowser = True,
      Appearance -> None,
      BaseStyle -> "Hyperlink"
      ]
     ]
   ];


helpBrowserNotebook[path : _List : {}] :=
  Notebook[{},
   DockedCells ->
    Cell[BoxData@ToBoxes@helpBrowserDockedCell[path],
     CellFrame -> {{0, 0}, {1, 0}},
     CellMargins -> None,
     CellFrameMargins -> None
     ],
   System`ClosingSaveDialog -> False,
   Saveable -> False,
   WindowTitle -> "Help Browser"
   ];


(* ::Subsubsection:: *)
(*OpenHelpBrowser*)


OpenHelpBrowser[path : _List : {}] :=
 
 If[MatchQ[$helpBrowser, _NotebookObject?(NotebookRead[#] =!= $Failed \
&)],
  SetOptions[$helpBrowser, {
    WindowFloating -> True,
    Visible -> True
    }];
  SetOptions[$helpBrowser, WindowFloating -> False],
  Quiet@
   Check[
    loadCachedDocumentationData[],
    preLoadDocumentationMetadata[];
    cacheDocumentationData[]
    ];
  $helpBrowser = CreateDocument@helpBrowserNotebook[]
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
