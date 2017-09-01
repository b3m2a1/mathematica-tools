(* ::Package:: *)

(* ::Section:: *)
(*BlockProgramming*)


BeginPackage["BlockBuilder`"];


(*Package Declarations*)
$defaultLocatorObjects::usage=
	"$defaultLocatorObjects provides default appearance objects";
$defaultLocatorEvents::usage=
	"$defaultLocatorEvents provides default events";
$defaultLocatorDefinitions::usage=
	"$defaultLocatorDefinitions is the set of default attributes";
makeAnchorLocator::usage=
	"makeAnchorLocator[Dynamic[locatorManager], pt, obj, ops] creates a basic OO locator element";
makePointAnchorLocator::usage=
	"makePointAnchorLocator[Dynamic[locatorManager], pt, obj, ops]  creates a Point locator element";
makeBlockAnchorLocator::usage=
	"makeBlockAnchorLocator[Dynamic[locatorManager], pts, obj, ops] creates a Block locator element";
makeLineAnchorLocator::usage=
	"makeLineAnchorLocator[Dynamic[locatorManager], pts, obj, ops] creates a Line locator element";
setAnchorLocatorPositionConnected::usage=
	"setAnchorLocatorPositionConnected[locatorManager, uuid, pos, shift, connected] sets the positions of a collection of anchors connected to a main one";
setAnchorLocatorPosition::usage=
	"setAnchorLocatorPosition[locatorManager, uuid, pos, shift, moveConnected, moveBound] sets the position of a locator element";
callAnchorLocatorEvent::usage=
	"callAnchorLocatorEvent[locatorManager, uuid, event, args] calls a locator event with args";
updateAnchorLocatorBound::usage=
	"updateAnchorLocatorBound[locatorManager, pos, radii] determines which locators should be bound to one another";
updateAnchorLocatorPositions::usage=
	"updateAnchorLocatorPositions[locatorManager, movableAnchors] updates bound locator positions";
updateAnchorLocators::usage=
	"updateAnchorLocators[locatorManager] called after each move is done; calls updateAnchorLocatorBound and updateAnchorLocatorPositions";
makeInsetAnchorLocator::usage=
	"makeInsetAnchorLocator[Dynamic[locatorManager], pts, expr, ops] makes a locator for Inset";
$insetWindowChangeObject::usage=
	"$insetWindowChangeObject the object used to configure the bottom bar of an insetWindow";
insetWindow::usage=
	"insetWindow[expr, changeObj, imageSize] a little window block element";
anchorLocatorModuleSpec::usage=
	"anchorLocatorModuleSpec[{pt, ops}] expands to a Point, Line, or Block locator
anchorLocatorModuleSpec[{pt, expr, ops}] expands to an Inset locator";
anchorLocatorModule::usage=
	"anchorLocatorModule[specs, controls, ops] creates a module with blocks from specs";

Begin["`Private`"];

(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Locator Creation*)


(* ::Subsubsection::Closed:: *)
(*Defaults*)


$defaultLocatorObjects =
  <|
   "Point" -> None,
   "Line" ->
    Function[
     With[{locatorManager = First@#},
      Line@
       Lookup[Lookup[locatorManager, #2], "Position"]
      ]
     ],
   "Block" ->
    Function[
     With[{locatorManager = First@#},
      Polygon@
       Lookup[Lookup[locatorManager, #2], "Position"]
      ]
     ]
   |>;


$defaultLocatorEvents =
  <|
   "MoveStart" -> (Null &),
   "Move" ->
    Function[
     Null,
     setAnchorLocatorPosition[##],
     HoldFirst
     ],
   "MoveEnd" ->
    Function[Null,
     updateAnchorLocators[#],
     HoldFirst
     ]
   |>;


$defaultLocatorDefinitions =
  <|
   "AttractionRadius" -> .5,
   "Bound" -> {},
   "Connected" -> {},
   "ConnectedMove" -> False,
   "Type" -> "Point",
   "Draggable" -> False,
   "Events" :>
    $defaultLocatorEvents
   |>;


(* ::Subsubsection::Closed:: *)
(*Base Anchor*)


Clear[makeAnchorLocator];
makeAnchorLocator[Verbatim[Dynamic][locatorManager_],
   pt : {_, _},
   obj : Except[_?OptionQ] : None,
   ops___?OptionQ] :=
  With[{uuid = CreateUUID[]},
   locatorManager[uuid] =
    Merge[{
      $defaultLocatorDefinitions,
      <|
       "Position" -> pt,
       "UUID" -> uuid
       |>,
      ops
      },
     Last
     ];
   uuid ->
    Locator[
     Dynamic[locatorManager[uuid, "Position"],
      {
       callAnchorLocatorEvent[locatorManager,
         uuid,
         "MoveStart"
         ] &,
       Function[
        callAnchorLocatorEvent[locatorManager,
         uuid,
         "Move",
         #,
         False,
         True,
         True
         ]
        ],
       callAnchorLocatorEvent[locatorManager,
         uuid,
         "MoveEnd"
         ] &
       }
      ],
     Replace[
      Replace[obj,
       Automatic :>
        $defaultLocatorObjects["Point"]
       ],
      None :> Sequence @@ {}
      ]
     ]
   ];


(* ::Subsubsection::Closed:: *)
(*Point Anchors*)


Clear[makePointAnchorLocator];
makePointAnchorLocator[Verbatim[Dynamic][locatorManager_],
   pt : {_, _},
   obj : _Graphics | None | Automatic : Automatic,
   ops___?OptionQ] :=
  Last[
   makeAnchorLocator[
    Dynamic[locatorManager],
    pt,
    obj,
    ops
    ]
   ];


(* ::Subsubsection::Closed:: *)
(*Block Anchors*)


Clear[makeBlockAnchorLocator];
makeBlockAnchorLocator[Verbatim[Dynamic][locatorManager_],
   pts : {{_, _} ..},
   obj : _Function | Automatic : Automatic,
   ops___?OptionQ] :=
  With[{
    locs =
     makeAnchorLocator[
        Dynamic[locatorManager],
        #,
        Automatic,
        "Type" -> "Block",
        ops
        ] & /@ pts
    },
   With[{uuids = First /@ locs},
    Map[
     Function[
      locatorManager[#, "Connected"] = DeleteCases[uuids, #]
      ],
     uuids
     ];
    Prepend[Last /@ locs,
     DynamicModule[
      {dragPos},
      EventHandler[
       Dynamic[
        Replace[obj,
          Automatic :>
           $defaultLocatorObjects["Block"]
          ][Dynamic[locatorManager], First /@ locs]
        ],
       {
        "MouseDown" :>
         If[locatorManager[First[uuids]]["Draggable"],
          dragPos = MousePosition["Graphics"]
          ],
        "MouseDragged" :>
         If[locatorManager[First[uuids]]["Draggable"],
          With[{mpos = MousePosition["Graphics"]},
           If[ListQ@mpos,
            If[ListQ@dragPos,
             callAnchorLocatorEvent[locatorManager,
              First[uuids],
              "Move",
              mpos - dragPos,
              True,
              True,
              True
              ];
             dragPos = mpos
             ]
            ]
           ]
          ],
        "MouseUp" :>
         If[locatorManager[First[uuids]]["Draggable"],
          dragPos =.
          ],
        PassEventsDown -> True
        }
       ]
      ]
     ]
    ]
   ];


(* ::Subsubsection::Closed:: *)
(*Line Anchors*)


Clear[makeLineAnchorLocator];
makeLineAnchorLocator[Verbatim[Dynamic][locatorManager_],
  pts : {{_, _}, {_, _}},
  obj : _Function | Automatic : Automatic,
  ops___?OptionQ] :=
 makeBlockAnchorLocator[
  Dynamic[locatorManager],
  pts,
  Replace[obj,
   Automatic :>
    Function[$defaultLocatorObjects["Line"][##]]
   ],
  "Type" -> "Line",
  ops
  ]


(* ::Subsubsection::Closed:: *)
(*Set Position*)


Clear[setAnchorLocatorPositionConnected];
setAnchorLocatorPositionConnected[locatorManager_,
   uuid_,
   pos_,
   shift_,
   connected_
   ] :=
  With[{shf =
     If[shift // TrueQ,
      pos,
      pos - locatorManager[uuid]["Position"]
      ]},
   Map[
    setAnchorLocatorPosition[locatorManager,
      #,
      shf,
      True,
      False,
      True
      ] &,
    connected
    ]
   ];
setAnchorLocatorPositionConnected~SetAttributes~HoldFirst


Clear[setAnchorLocatorPosition];
setAnchorLocatorPosition[locatorManager_,
   uuid_,
   pos_,
   shift_,
   moveConnected_,
   moveBound_
   ] :=
  (
   If[moveConnected && locatorManager[uuid]["ConnectedMove"] //
     TrueQ,
    setAnchorLocatorPositionConnected[locatorManager,
      uuid,
      pos,
      shift,
      locatorManager[uuid]["Connected"]
      ];
    ];
   If[(moveBound && locatorManager[uuid]["Type"] === "Block") // TrueQ,
    setAnchorLocatorPositionConnected[locatorManager,
      uuid,
      pos,
      shift,
      Select[locatorManager[uuid]["Bound"],
       locatorManager[#]["Type"] =!= "Block" &
       ]
      ];
    ];
   If[shift // TrueQ,
     AddTo,
     Set
     ][locatorManager[uuid, "Position"], pos]
   );
setAnchorLocatorPosition~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*Call Event*)


Clear[callAnchorLocatorEvent];
callAnchorLocatorEvent[locatorManager_,
   uuid_,
   event_,
   args___
   ] :=
  locatorManager[uuid, "Events"][event][
   locatorManager,
   uuid,
   args];
callAnchorLocatorEvent~SetAttributes~HoldAllComplete


(* ::Subsubsection::Closed:: *)
(*Update Neighbors*)


Clear[updateAnchorLocatorBound];
updateAnchorLocatorBound[locatorManager_,
   pos_,
   radii_
   ] :=
  With[{
    binds =
     KeyValueMap[
       # ->
         Keys@
          With[{p = #2, r = radii[#]},
           Select[Norm[# - p] & /@ KeyDrop[pos, #],
            # < r &
            ]
           ] &,
       pos
       ] // Association
    },
   KeyValueMap[
    Function[locatorManager[#, "Bound"] = #2],
    binds
    ]
   ];
updateAnchorLocatorBound~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*Update Positions*)


Clear[updateAnchorLocatorPositions];
updateAnchorLocatorPositions[locatorManager_,
   movableAnchors_
   ] :=
  Map[
   With[{moveTo = locatorManager[#, "Bound"]},
     If[Length[moveTo] > 0,
      locatorManager[#, "Position"] =
       Mean[
        Lookup[
         Lookup[locatorManager, moveTo],
         "Position"
         ]
        ]
      ]
     ] &,
   movableAnchors
   ];
updateAnchorLocatorPositions~SetAttributes~HoldFirst;


(* ::Subsubsection::Closed:: *)
(*Update Locators*)


Clear[updateAnchorLocators];
updateAnchorLocators[locatorManager_] :=
  Module[
   {
    pos = #["Position"] & /@ locatorManager,
    radii = #["AttractionRadius"] & /@ locatorManager,
    movableAnchors =
     Keys@Select[locatorManager, MatchQ[#Type, "Point" | "Line"] &]
    },
   updateAnchorLocatorBound[locatorManager,
    pos,
    radii
    ];
   updateAnchorLocatorPositions[locatorManager,
    movableAnchors
    ];
   ];
updateAnchorLocators~SetAttributes~HoldFirst


(* ::Subsection:: *)
(*Special Anchors*)


(* ::Subsubsection::Closed:: *)
(*Inset*)


Clear[makeInsetAnchorLocator];
makeInsetAnchorLocator[Verbatim[Dynamic][locatorManager_],
   pts : {{_, _}, {_, _}},
   expr_,
   ops___?OptionQ] :=
  makeBlockAnchorLocator[Dynamic[locatorManager],
   pts,
   Function[
    With[{pos = Lookup[Lookup[First@#, #2], "Position"]},
     Inset[
      Pane[expr,
       ImageSizeAction -> "ResizeToFit"
       ],
      Mean@pos,
      Center,
      (*If[#[[2]]\[Equal]0,#[[1]],#]&@*)

      First@Abs[Subtract @@ pos],
      Sequence @@
       FilterRules[{
         ops,
         Alignment -> {Center, Center}
         },
        Options[Inset]
        ]
      ]
     ]
    ],
   "ConnectedMove" -> True,
   "Draggable" -> True,
   Sequence @@

    FilterRules[{ops},
     Except[Alternatives @@ Map[First, Options[Inset]]]]
   ];


(* ::Subsubsection::Closed:: *)
(*InsetWindow*)


$insetWindowChangeObject =
  Function[
   Null,
   Row@{
     Button["Expr",
      # = Input["Set the Expression"],
      Method -> "Queued"],
     Button["Meta",
      #2 = Input["Set the Metadata"], Method -> "Queued"]
     },
   HoldFirst
   ];


Clear[insetWindow];
insetWindow[
  expr_,
  changeObj : _Function | Automatic : Automatic,
  imageSize : {_, _} : {150, 100}
  ] :=
 DynamicModule[{
   exp = expr,
   meta
   },
  Grid[List /@ {
      Panel["",
       ImageSize -> {imageSize[[1]] + 10, 35},
       Appearance ->
        {
         "Default" ->
          Lookup[

           FrontEndResource["FEExpressions",
            "MoreLeftSetterNinePatchAppearance"],
           "Hover"
           ]
         },
       FrameMargins -> None
       ],
      EventHandler[
       Framed[
        Pane[Dynamic[exp],
         ImageSize -> imageSize,
         ImageSizeAction -> "Scrollable"
         ],
        Background -> White,
        FrameStyle -> GrayLevel[.95]
        ],
       {
        "MouseDown" :> None,
        "MouseDragged" :> None,
        "MouseUp" :> None,
        PassEventsDown -> False
        }
       ],
      Panel[
       Replace[
        Replace[changeObj,
         Automatic :>
          $insetWindowChangeObject
         ],
        f_Function :>
         f[exp, meta]
        ],
       ImageSize -> {imageSize[[1]] + 10, 25},
       Appearance ->
        {
         "Default" ->
          Lookup[

           FrontEndResource["FEExpressions",
            "MoreLeftSetterNinePatchAppearance"],
           "Default"
           ]
         },
       ImageMargins -> None
       ]
      },
    Spacings -> {0, 0}
    ] //
   Framed[#,
     FrameMargins -> None,
     FrameStyle -> GrayLevel[.5]
     ] &
  ]


(* ::Subsection:: *)
(*Manager Module*)


(* ::Subsubsection::Closed:: *)
(*Parse Spec*)


Clear[anchorLocatorModuleSpec];


anchorLocatorModuleSpec[
  {
   pt : {Except[_List], Except[_List]},
   ops___?OptionQ
   }
  ] :=
 <|
  "Function" -> makePointAnchorLocator,
  "Args" -> Prepend[pt]@Flatten@{ops}
  |>


anchorLocatorModuleSpec[
  {
   pt : {
     {Except[_List], Except[_List]},
     {Except[_List], Except[_List]}
     },
   ops___?OptionQ
   }
  ] :=
 <|
  "Function" -> makeLineAnchorLocator,
  "Args" -> Prepend[pt]@Flatten@{ops}
  |>


anchorLocatorModuleSpec[
  {
   pt_List,
   ops___?OptionQ
   }
  ] :=
 <|
  "Function" -> makeBlockAnchorLocator,
  "Args" -> Prepend[pt]@Flatten@{ops}
  |>


anchorLocatorModuleSpec[
  {
   pt_List,
   expr_,
   ops___?OptionQ
   }
  ] :=
 <|
  "Function" -> makeInsetAnchorLocator,
  "Args" -> Join[{pt, expr}, Flatten@{ops}]
  |>


(* ::Subsubsection::Closed:: *)
(*Module*)


Clear[anchorLocatorModule];
Options[anchorLocatorModule] =
  Options[Graphics];
anchorLocatorModule[
  specs_List,
  controls : _Function | Automatic : Automatic,
  ops : OptionsPattern[]
  ] :=
 With[{
   sp =
    anchorLocatorModuleSpec /@ specs,
   functions =
    Names["BlockBuilder`*"],
   imSize =
    Replace[Replace[OptionValue[ImageSize], Automatic :> 800],
     {
      i_Integer :> {i, i}
      }
     ]
   },
  Deploy@
   DynamicModule[{$manager = <||>},
    Column@{
      Graphics[
       Map[
        #["Function"][
          Dynamic[$manager],
          Sequence @@
           Prepend[
            If[AllTrue[Rest[#["Args"]], OptionQ],
             Prepend[Rest[#["Args"]],
              "AttractionRadius" ->
               imSize[[1]]*.05
              ],
             Insert[Rest[#["Args"]],
              "AttractionRadius" ->
               imSize[[1]]*.05,
              2
              ]
             ],
            #["Args"][[1]] //.
             {
              {Scaled[i_], h_} :>
               {i*imSize[[1]], h},
              {i_, Scaled[h_]} :>
               {i, imSize[[2]]*h}
              }
            ]
          ] &,
        sp
        ],
       ImageSize -> imSize,
       PlotRange -> {
         {-imSize[[1]], imSize[[1]]}/2 // Floor,
         {-imSize[[2]], imSize[[2]]}/2 // Floor
         },
       ops
       ],
      Replace[
       Replace[controls,
        Automatic :>
         Row@{Button["Print", Print[$manager]]}
        ],
       f_Function :> f[$manager]
       ]
      }
    ]
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
