(* ::Package:: *)

(* ::Section:: *)
(*PanelFileBrowser*)


BeginPackage["PanelFileBrowser`"];


(*Package Declarations*)
Options::usage="Options[PaneFileBrowser]";
PaneFileBrowser::usage="PaneFileBrowser[dir, disp, ops]";


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


Options[PaneFileBrowser] =
  {
   "FileFEEvents" -> {},
   "DirectoryFEEvents" -> {},
   "FileOnClick" -> Function[#],
   "FileOnDoubleClick" -> Function[SystemOpen@#],
   "DirectoryOnClick" -> Function[""],
   "DirectoryOnDoubleClick" ->
    Function[
     Set[Evaluate@Extract[#2["RootDirectory"], 1, HoldPattern], #];],
   "SelectFunction" ->
    Function[MatchQ[FileNameTake[#], Except[".DS_Store"]]],
   "SortFunction" -> Sort,
   "BottomBarElements" -> {}
   };
PaneFileBrowser[
  dir : (_String | _File)?DirectoryQ | Automatic : Automatic,
  disp :
   (
    Verbatim[Dynamic][dvar_, {__, f_}, ___?OptionQ] |
     Verbatim[Dynamic][dvar_, f_, ___?OptionQ] |
     Verbatim[Dynamic][dvar_, ___?OptionQ] | None
    ) : None,
  ops : OptionsPattern[]
  ] :=
 DynamicModule[{
   dirState = <||>,
   populateDirState,
   root = Replace[dir, Automatic :> Directory[]],
   browserElement,
   directoryTree,
   directorySubTree,
   eventHandlerElement,
   directoryOpener,
   sort = OptionValue["SortFunction"],
   selFunc = OptionValue["SelectFunction"],
   filOnClick = OptionValue["FileOnClick"],
   dirOnClick = OptionValue["DirectoryOnClick"],
   filOnDubClick = OptionValue["FileOnDoubleClick"],
   dirOnDubClick = OptionValue["DirectoryOnDoubleClick"],
   extraFileEvents = OptionValue["FileFEEvents"],
   extraDirEvents = OptionValue["DirectoryFEEvents"],
   pvar,
   prevVar,
   prevFunc = If[Length@{f} == 0, Set, f],
   dispSize = 650*{1, 1/GoldenRatio},
   barWidth = 200,
   topHeight = 50,
   bottomHeight = 25,
   bbels
   },
  dispSize = dispSize - {0, topHeight + bottomHeight};
  prevVar =
   If[disp =!= None,
    dvar = "";
    Dynamic[dvar],
    pvar = "";
    Dynamic[pvar]
    ];
  extraDirEvents =
   OptionValue["DirectoryFEEvents"] /. {
     #["RootDirectory"] :> Dynamic[root],
     #["DirectoryTree"] :> dirState,
     #["DisplayVariable"] :> prevVar,
     #["PaneSize"] :> Dynamic[dispSize - {barWidth, 0}]
     };
  extraFileEvents =
   OptionValue["FileFEEvents"] /. {
     #["RootDirectory"] :> Dynamic[root],
     #["DirectoryTree"] :> dirState,
     #["DisplayVariable"] :> prevVar,
     #["PaneSize"] :> Dynamic[dispSize - {barWidth, 0}]
     };
  bbels =
   Replace[OptionValue["BottomBarElements"],
     Except[_List] -> {}
     ] /.
    {
     #["RootDirectory"] :> Dynamic[root],
     #["DirectoryTree"] :> Dynamic@dirState,
     #["DisplayVariable"] :> prevVar,
     #["PaneSize"] :> Dynamic[dispSize - {barWidth, 0}]
     };
  Framed[#, FrameStyle -> GrayLevel[.8], 
     FrameMargins -> {{0, 0}, {0, 0}}] &@
   Column[{
     Panel[
      Row@{
        Spacer[10],
        Dynamic[
         InputField[Dynamic[root, If[DirectoryQ[#], Set[root, #]] &], 
          String,
          FieldSize -> {{50, 100}, 1}
          ],
         Function[
          root = #;
          dirState = <||>;
          populateDirState[#]
          ]
         ]
        },
      ImageSize -> {Dynamic[dispSize[[1]]], topHeight},
      Alignment -> {Left, Center},
      Appearance ->
       {
        "Default" ->
         Lookup[
          
          FrontEndResource["FEExpressions", 
           "MoreLeftSetterNinePatchAppearance"],
          "Hover"
          ]
        }
      ],
     Pane[
      Grid[{
        {
         Pane[
          Dynamic@directoryTree@root,
          Dynamic[
           {
            barWidth,
            dispSize[[2]]
            },
           Set[barWidth, #] &
           ],
          Scrollbars -> True
          ],
         Pane[
          Dynamic@prevVar,
          Dynamic[dispSize - {barWidth, 0}]
          ]
         }
        },
       Alignment -> {Left, Top},
       Spacings -> {0, 0}
       ],
      Dynamic[dispSize],
      BaseStyle -> LineBreakWithin -> False,
      AppearanceElements -> {"ResizeArea"},
      Scrollbars -> False
      ],
     Item[#,
        Alignment -> {Center, Bottom},
        FrameStyle -> GrayLevel[.8],
        Frame -> {None, None, GrayLevel[.8], None}
        ] &@
      Panel[
       Style[
        Grid[{bbels},
         Alignment -> {Left, Bottom},
         RowMinHeight -> 0,
         Spacings -> {0, 0}
         ],
        ButtonBoxOptions ->
         {
          ButtonMinHeight -> 0
          }
        ],
       ImageMargins -> {{0, 0}, {-2, 1}},
       ImageSize -> {Dynamic[dispSize[[1]]], {bottomHeight, 1000}},
       Appearance ->
        {
         "Default" ->
          Lookup[
           
           FrontEndResource["FEExpressions", 
            "MoreLeftSetterNinePatchAppearance"],
           "Default"
           ]
         },
       FrameMargins -> None,
       Alignment -> {Left, Center},
       ContentPadding -> False
       ]
     },
    Spacings -> 0,
    RowMinHeight -> 0
    ],
  UnsavedVariables :> {dirState, directoryTree, directorySubTree},
  Initialization :> {
    dirState = <||>,
    populateDirState[d_] :=
     dirState[d] =
      <|
       "Open" -> False,
       "Children" ->
        Replace[Quiet@Check[FileNames["*", d], $Failed],
         l_List :> sort@Select[l, selFunc]
         ]
       |>,
    populateDirState[root],
    directoryTree[d_String?DirectoryQ] :=
     PaneSelector[{
         False -> #,
         True :>
          Dynamic@Column[{#, directorySubTree[d]}]
         },
        Dynamic@TrueQ@dirState[d, "Open"],
        ImageSize -> Automatic
        ] &[
      Row[{
        directoryOpener[d],
        eventHandlerElement[d]
        }]
      ],
    directorySubTree[d_String?DirectoryQ] :=
     Grid[{{
        Invisible[Opener[]],
        Column[
         browserElement /@ dirState[d, "Children"]
         ]
        }},
      Spacings -> {0, 0}
      ],
    directoryOpener[d_] :=
     Opener[
      Dynamic[TrueQ@dirState[d, "Open"],
       {
        Automatic,
        Function[
         dirState[d, "Open"] = #;
         If[#,
          populateDirState /@
           dirState[d, "Children"],
          Map[
           Function[dirState[#] =.],
           dirState[d, "Children"]
           ]
          ]
         ]
        }
       ],
      Enabled -> Length@dirState[d, "Children"] > 0
      ],
    eventHandlerElement[d_String?FileExistsQ] :=
     
     MouseAppearance[
      EventHandler[
       If[dirState[d, "Children"] === $Failed,
        Tooltip[
         Style[Row@{"\[WarningSign] ", FileNameTake[d]}, Orange],
         "Access Denied"
         ],
        FileNameTake[d]
        ],
       {
        With[{ev = Extract[prevVar, 1, Unevaluated]},
         "MouseClicked" :>
          prevFunc[ev,
           Replace[Null -> ""]@
            If[CurrentValue["MouseClickCount"] == 2,
              If[DirectoryQ@d,
               dirOnDubClick,
               filOnDubClick
               ],
              If[DirectoryQ@d,
               dirOnClick,
               filOnClick
               ]
              ][d,
             <|
              "RootDirectory" :> Dynamic@root,
              "DirectoryTree" :> Dynamic@dirState,
              "DisplayVariable" :> prevVar,
              "PaneSize" :> Dynamic[dispSize]
              |>
             ]
           ]
         ],
        If[DirectoryQ@d,
         Sequence @@ extraDirEvents,
         Sequence @@ extraFileEvents
         ],
        Method -> "Queued"
        }
       ],
      "LinkHand"
      ],
    browserElement[d_String?DirectoryQ] :=
     directoryTree[d],
    browserElement[d_String?FileExistsQ] :=
     eventHandlerElement[d]
    }
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
