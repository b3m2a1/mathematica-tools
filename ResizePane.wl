(* ::Package:: *)

(* ::Section:: *)
(*ResizePane*)


BeginPackage["ResizePane`"];


(*Package Declarations*)
Options::usage="Options[resizeArea]
Options[ResizeArea]
Options[ResizePane]";
ResizeArea::usage="ResizeArea[d, graphics, ops]";
ResizePane::usage="ResizePane[expr, var, ops]";


(* ::Subsubsection::Closed:: *)
(*Private Declarations*)


AppendTo[$ContextPath,$Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
resizeArea::usage="resizeArea[{Dynamic[size, {__, f} | f, ___] | Dynamic[size, ___], Dynamic[dragBase, ___], Dynamic[sizeBase, ___]}, mode, graphics, ops]";


End[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


(* ::Subsubsection::Closed:: *)
(*ResizeArea*)


Options[resizeArea] =
  Join[
   Options[Graphics],
   {
    MouseAppearance -> Automatic
    }
   ];
resizeArea[
   {
    Verbatim[Dynamic][size_,
      {__, f_} | f : Except[_?OptionQ],
      ___?OptionQ
      ] |
     Verbatim[Dynamic][size_,
      ___?OptionQ
      ],
    Verbatim[Dynamic][dragBase_, ___],
    Verbatim[Dynamic][sizeBase_, ___]
    },
   mode : "Vertical" | "Horizontal" | "Both" : "Both",
   graphics : _List?(Not@*OptionQ) | {} : {},
   ops : OptionsPattern[]
   ] :=
  Deploy@EventHandler[
    MouseAppearance[#,
       Replace[OptionValue[MouseAppearance],
        Automatic :>
         Switch[mode,
          "Vertical", "FrameTBResize",
          "Horizontal", "FrameLRResize",
          "Both", "DragAndDrop"
          ]
        ]
       ] &@
     Graphics[
      graphics,
      FilterRules[{
        ImageSize ->
         Replace[OptionValue[ImageSize],
          Automatic :>
           If[graphics === {},
            Switch[mode,
             "Vertical", {Full, 2},
             "Horizontal", {2, Full},
             "Both", {22, 22}
             ],
            Automatic
            ]
          ],
        ops,
        Background -> GrayLevel[.95],
        Frame -> True,
        FrameStyle -> GrayLevel[.8],
        FrameMargins -> None,
        AspectRatio -> Full,
        ImagePadding -> -1,
        PlotRangePadding -> 0,
        Method -> {"ShrinkWrap" -> True},
        ImageMargins -> -1
        },
       Options@Graphics
       ]
      ], {
     "MouseDown" :>
      (
       If[! (
          
          NumericQ@dragBase ||
           
           AllTrue[Flatten@{dragBase}, NumericQ]
          ),
        Replace[MousePosition["ScreenAbsolute"], {
          {x_, y_} :>
           Switch[mode,
            "Vertical", Set[dragBase, y],
            "Horizontal", Set[dragBase, x],
            "Both", Set[dragBase, {x, y}]
            ]
          }]
        ];
       If[! (
          
          NumericQ@sizeBase ||
           
           AllTrue[Flatten@{sizeBase}, NumericQ]
          ),
        If[! (
           
           NumericQ@size ||
            
            AllTrue[Flatten@{size}, NumericQ]
           ),
         size = dragBase
         ];
        sizeBase = size
        ]
       ),
     "MouseUp" :>
      Clear[dragBase, sizeBase],
     "MouseDragged" :>
      (
       Replace[MousePosition["ScreenAbsolute"],
        {x_, y_} :>
         With[{m =
            Switch[mode,
             "Vertical", y,
             "Horizontal", x,
             "Both", {x, y}
             ]
           },
          If[! (
             
             NumericQ@dragBase ||
              
              AllTrue[Flatten@{dragBase}, NumericQ]
             ),
           Set[dragBase, m];
           sizeBase = size,
           With[{new = sizeBase +  m - dragBase},
            If[Length@{f} === 0,
             Set[size,
              If[! NumericQ@size,
               Switch[mode,
                "Both", new,
                "Vertical", {size[[1]], new[[2]]},
                "Horizontal", {new[[1]], size[[2]]}
                ],
               new
               ]
              ],
             f[new]
             ]
            ]
           ]
          ]
        ]
       ),
     PassEventsDown -> True
     }];


Options[ResizeArea] =
  Join[
   Options@resizeArea,
   {
    "Orientation" -> "Both"
    }
   ];
ResizeArea[d_Dynamic,
   graphics : Except[_?OptionQ] : {},
   ops : OptionsPattern[]
   ] :=
  DynamicModule[{tmp1, tmp2},
   resizeArea[
    {d, Dynamic[tmp1], Dynamic[tmp2]},
    Replace[OptionValue["Orientation"],
     Except["Horizontal" | "Vertical"] -> "Both"
     ],
    Replace[graphics, {
      i_?ImageQ :> {Inset[i]},
      Except[_List] -> {graphics}
      }]
    ],
   UnsavedVariables :> {tmp1, tmp2}
   ];


(* ::Subsubsection:: *)
(*ResizePane*)


Options[ResizePane] =
  Join[
   Options[Pane],
   Options[resizeArea],
   Options[Grid],
   {
    "ResizeLocations" -> {Right, Bottom},
    "ResizeCorners" -> True,
    "ResizeEdges" -> True,
    "ResizeAreaSize" -> 1,
    "ResizeCornerPadding" -> 2,
    "ResizeAreaOptions" -> {FrameTicks -> None}
    }
   ];
ResizePane[expr_,
  Optional[var : Verbatim[Dynamic][imSize_, e___] | None, None],
  ops : OptionsPattern[]
  ] :=
 DynamicModule[
  {
   imSizeVar,
   imsx,
   imsy,
   ims,
   tmp1,
   tmp2,
   resizeAreaSize =
    Replace[OptionValue["ResizeAreaSize"], {
      {w_, h_} :>
       {
        Replace[w, Except[_Integer?Positive] -> 1],
        Replace[h, Except[_Integer?Positive] -> 1]
        },
      Except[_Integer?Positive] -> {1, 1},
      i_Integer?Positive :> {i, i}
      }],
   cornerPad =
    Replace[OptionValue["ResizeCornerPadding"],
     Except[_Integer] -> 0
     ],
   rops =
    Flatten@{
      Replace[OptionValue["ResizeAreaOptions"],
       Except[_?OptionQ] -> {}
       ]
      },
   le = MemberQ[OptionValue["ResizeLocations"], Left],
   te = MemberQ[OptionValue["ResizeLocations"], Top],
   re = MemberQ[OptionValue["ResizeLocations"], Right],
   be = MemberQ[OptionValue["ResizeLocations"], Bottom],
   useEdges = MatchQ[OptionValue["ResizeEdges"], Except[False]],
   useCorners = MatchQ[OptionValue["ResizeCorners"], Except[False]],
   calr,
   catb
   },
  calr = Which[le && re, Center, le, Right, re, Left, True, Center];
  catb = Which[te && be, Center, te, Bottom, be, Top, True, Center];
  imSizeVar =
   If[var === None,
    Dynamic[ims,
     Set[ims, {Max[{#[[1]], 1}], Max[{#[[2]], 1}]}] &
     ],
    Dynamic[imSize,
     Set[imSize, {Max[{#[[1]], 1}], Max[{#[[2]], 1}]}] &
     ]
    ];
  Extract[imSizeVar, 1,
   Function[Null,
    Set[#,
     Replace[OptionValue[ImageSize], {
       Automatic :>
        Replace[{imSize}, {
          {i_?NumericQ} :> {Max@{i, 1}, Max@{i, 1}},
          {{x_?NumericQ, y_?NumericQ}} :> {Max@{x, 1}, Max@{y, 1}},
          _ -> {100, 50}
          }],
       i : {_, _} :> i,
       i_ :> {Replace[i, Automatic :> 100], 
         Replace[i, Automatic :> 50]}
       }
      ]
     ],
    HoldAllComplete
    ]
   ];
  imsx =
   Extract[imSizeVar, 1,
    Function[Null,
     Replace[HoldComplete[#],
      HoldComplete[v_] :>
       
       Dynamic[v[[1]], Set[v[[1]], Max@{#, 1}] &]
      ],
     HoldAllComplete]
    ];
  imsy =
   Extract[imSizeVar, 1,
    Function[Null,
     Replace[HoldComplete[#],
      HoldComplete[v_] :>
       
       Dynamic[v[[2]], Set[v[[2]], Max@{#, 1}] &]
      ],
     HoldAllComplete]
    ];
  Grid[{
    If[te,
     {
      If[le,
       If[useCorners,
        Item[#, Alignment -> {Left, Top}] &@
         resizeArea[
          {
           imSizeVar,
           Dynamic[tmp1],
           Dynamic[tmp2]
           },
          "Both",
          FilterRules[
           {
            ImageSize -> cornerPad + resizeAreaSize,
            Sequence @@ rops
            },
           Options@resizeArea
           ]
          ],
        Null
        ],
       Sequence @@ {}
       ],
      If[useEdges,
       Item[#,
          Alignment -> {calr, Top}
          ] &@
        resizeArea[
         {
          imsy,
          Dynamic[tmp1],
          Dynamic[tmp2]
          },
         "Vertical",
         FilterRules[{
           ImageSize -> {imsx, resizeAreaSize[[2]]},
           Sequence @@ rops
           },
          Options@resizeArea
          ]
         ],
       Null
       ],
      If[re,
       If[useCorners,
        Item[#, Alignment -> {Right, Bottom}] &@
         resizeArea[
          {
           imSizeVar,
           Dynamic[tmp1],
           Dynamic[tmp2]
           },
          "Both",
          FilterRules[{
            ImageSize -> cornerPad + resizeAreaSize,
            Sequence @@ rops
            },
           Options@resizeArea
           ]
          ],
        Null
        ],
       Sequence @@ {}
       ]
      },
     Sequence @@ {}
     ],
    {
     If[le,
      Item[#, Alignment -> {Left, catb}] &@
       resizeArea[
        {
         imsx,
         Dynamic[tmp1],
         Dynamic[tmp2]
         },
        "Horizontal",
        FilterRules[{
          ImageSize -> {resizeAreaSize[[1]], imsy},
          Sequence @@ rops
          },
         Options@resizeArea
         ]
        ],
      Sequence @@ {}
      ],
     Item[#, Alignment -> {calr, catb}] &@
      Pane[expr,
       ImageSize -> imSizeVar,
       FilterRules[{ops},
        Options@Pane
        ]
       ],
     If[re,
      If[useEdges,
       Item[#, Alignment -> {Right, catb}] &@
        resizeArea[
         {
          imsx,
          Dynamic[tmp1],
          Dynamic[tmp2]
          },
         "Horizontal",
         FilterRules[{
           ImageSize -> {resizeAreaSize[[1]], imsy},
           Sequence @@ rops
           },
          Options@resizeArea
          ]
         ],
       Null
       ],
      Sequence @@ {}
      ]
     },
    If[be,
     {
      If[le,
       If[useCorners,
        Item[#, Alignment -> {Left, Bottom}] &@
         resizeArea[
          {
           imSizeVar,
           Dynamic[tmp1],
           Dynamic[tmp2]
           },
          "Both",
          FilterRules[{
            ImageSize -> cornerPad + resizeAreaSize,
            Sequence @@ rops
            },
           Options@resizeArea
           ]
          ],
        Null
        ],
       Sequence @@ {}
       ],
      If[useEdges,
       Item[#,
          Alignment -> {calr, Bottom}
          ] &@
        resizeArea[
         {
          imsy,
          Dynamic[tmp1],
          Dynamic[tmp2]
          },
         "Vertical",
         FilterRules[{
           ImageSize -> {imsx, resizeAreaSize[[2]]},
           Sequence @@ rops
           },
          Options@resizeArea
          ]
         ],
       Null
       ],
      If[re,
       If[useCorners,
        Item[#, Alignment -> {Right, Bottom}] &@
         resizeArea[
          {
           imSizeVar,
           Dynamic[tmp1],
           Dynamic[tmp2]
           },
          "Both",
          FilterRules[{
            ImageSize -> cornerPad + resizeAreaSize,
            Sequence @@ rops
            },
           Options@resizeArea
           ]
          ],
        Null
        ],
       Sequence @@ {}
       ]
      },
     Sequence @@ {}
     ]
    },
   FilterRules[{
     Alignment -> {Center, Center},
     ops,
     RowMinHeight -> 0,
     Spacings -> {0, 0},
     Frame -> True,
     FrameStyle ->
      Replace[OptionValue[FrameStyle], {} | Automatic -> GrayLevel[.8]]
     },
    Join[
     Alternatives @@ Keys@Options@Grid,
     Alternatives[RowMinHeight]
     ]
    ]
   ]
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
