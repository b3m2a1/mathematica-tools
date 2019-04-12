(* ::Package:: *)

BeginPackage["RubiksCubes`"];


RubiksCube::usage="A Rubik's cube object";


Begin["`Private`"];


<<InterfaceObjects`


RegisterInterface[
  RubiksCube,
  {"Size", "Origin", "Colors", "Cuboids"},
  "Constructor"->constructRubiksCube,
  "MutationFunctions"->{"Keys", "Parts"}
  ];


InterfaceMethod[RubiksCube]@
  r_RubiksCube["Show"][ops:OptionsPattern[]]:=
    showRubik[r, ops];
InterfaceMethod[RubiksCube]@
  r_RubiksCube["Twist"][theta_, axis_]:=
    twistRubik[r, theta, axis];


constructRubiksCube[ops:OptionsPattern[]]:=
  Merge[
    {
      ops,
      "Size"->1.,
      "Origin"->{0., 0., 0.},
      "Colors"->{Black, Red, Orange, Blue, Green, Yellow, White},
      "Cuboids"->setupCuboids[]
      },
    First
    ];


RegisterInterface[
  RubiksCuboid,
  {"Coordinates", "ColorIndices"},
  "Constructor"->makeRubiksCuboid,
  "MutationFunctions"->{"Keys", "Parts"}
  ];


cindices=
  {
    {8,4,2,6},{8,6,5,7},{8,7,3,4},
    {4,3,1,2},{1,3,7,5},{2,1,5,6}
    };
getVertList[spacing_:.05]:=
  {
    {-(1/2),-(1/2),-(1/2)},{-(1/2),-(1/2),1/2},{-(1/2),1/2,-(1/2)},
    {-(1/2),1/2,1/2},{1/2,-(1/2),-(1/2)},{1/2,-(1/2),1/2},
    {1/2,1/2,-(1/2)},{1/2,1/2,1/2}
    }*(1-spacing);
cindices=
  {
    {8,4,2,6},{8,6,5,7},{8,7,3,4},
    {4,3,1,2},{1,3,7,5},{2,1,5,6}
    };
makeRubiksCuboid[cols_, center_]:=
  <|
    "Coordinates"->Map[#+center&, getVertList[]],
    "ColorIndices"->cols
    |>;
makeCuboidGraphic[parent_, cuboid_]:=
  With[{o=parent@"Origin", size=parent@"Size"},
    GraphicsComplex[
      #+o&/@size*cuboid["Coordinates"],
      MapThread[
        {FaceForm[#1], Polygon[#2]}&,
        {
          parent["Colors"][[cuboid@"ColorIndices"]], 
          cindices
          }
        ]
      ]
    ];
InterfaceMethod[RubiksCuboid]@
  c_RubiksCuboid["Show"][parent_]:=
   makeCuboidGraphic[parent, c];


(* handles cube coloring *)
stickerPos=
  {
    (* back: {0,0,0,1,0,0} *)
    {{{0,0,0,1,5,3},{0,0,0,1,0,3},{6,0,0,1,0,3}},
    {{0,0,0,1,5,0},{0,0,0,1,0,0},{6,0,0,1,0,0}},
    {{0,0,4,1,5,0},{0,0,4,1,0,0},{6,0,4,1,0,0}}},
    (* middle, {0,0,0,0,0,0} *)
    {{{0,0,0,0,5,3},{0,0,0,0,0,3},{6,0,0,0,0,3}},
    {{0,0,0,0,5,0},{0,0,0,0,0,0},{6,0,0,0,0,0}},
    {{0,0,4,0,5,0},{0,0,4,0,0,0},{6,0,4,0,0,0}}},
    (* front, {0,2,0,0,0,0} *)
    {{{0,2,0,0,5,3},{0,2,0,0,0,3},{6,2,0,0,0,3}},
    {{0,2,0,0,5,0},{0,2,0,0,0,0},{6,2,0,0,0,0}},
    {{0,2,4,0,5,0},{0,2,4,0,0,0},{6,2,4,0,0,0}}}
    };
originalCenters = Outer[List,{-1,0,1},{-1,0,1},{-1,0,1}];
setupCuboids[]:=
  With[
    {
      stickers = 1+stickerPos, 
      origins = originalCenters
      },
    MapThread[
      RubiksCuboid[#, #2]&,
      {stickers, origins},
      3
      ]
    ]


showRubik//Clear
showRubik[cube_RubiksCube, ops:OptionsPattern[]]:=
  Graphics3D[
    #@"Show"[cube]&/@Flatten[cube["Cuboids"]],
    PlotRange->cube["Size"]*2+.1,
    SphericalRegion->True,
    Boxed->False,
    BaseStyle->{12},
    Lighting->{
      {"Ambient",GrayLevel[0.9]},
      {"Point",GrayLevel[0.1],ImageScaled[{0.5,3,0.5}]}
      },
    ImageSize->{350,350},
    ops
    ]


setPos[cub_, pos_]:=
  Module[{c = cub, o},
    c["Cuboids"] = 
      MapThread[
        Function[
          o=#;
          o["Coordinates"]=#2;
          o
          ], 
        {c["Cuboids"], pos},
        3
        ];
    c
    ]


With[{allPositions=Round[Flatten[originalCenters+2, 2]]},

twistRubik[cube_, theta_, face:"Back"|"Front"|"Left"|"Right"|"Down"|"Up"]:=
  Module[
    {
      mp, 
      pos = Map[#["Coordinates"]&, cube["Cuboids"], {3}],
      module 
      },
    mp=Flatten[permList[face],1];
    pos=
      Join[
        Extract[pos, Complement[allPositions, mp]],
        turn[face, theta, Extract[pos, mp]]
        ];
    pos = 
      ArrayReshape[
        pos[[Ordering[Join[Complement[allPositions, mp], mp]]]], 
        {3, 3, 3, 8, 3}
        ];
    setPos[cube, pos]
    ];
    
]


(* ::Text:: *)
(*Everything from here down is just to implement face-specific rotations and permutations*)


(* cube permutation data *)
perms=
  <|
    "Back"->
      {
        {{1,1,1},{1,3,1},{1,3,3},{1,1,3}},
        {{1,2,1},{1,3,2},{1,2,3},{1,1,2}},
        {{1,2,2}}
        },
    "Left"->
      {
        {{1,1,1},{1,1,3},{3,1,3},{3,1,1}},
        {{2,1,1},{1,1,2},{2,1,3},{3,1,2}},
        {{2,1,2}}
        },
    "Down"->
      {
        {{1,1,1},{3,1,1},{3,3,1},{1,3,1}},
        {{2,1,1},{3,2,1},{2,3,1},{1,2,1}},
        {{2,2,1}}
        }
    |>;
perms["Front"]=
  Reverse/@perms["Back"]/.{1,y_,z_}:>{3,y,z};
perms["Right"]=
  Reverse/@perms["Left"]/.{x_,1,z_}:>{x,3,z};
perms["Up"]=
  Reverse/@perms["Down"]/.{x_,y_,1}:>{x,y,3};


(* cube axis data *)
axes=
  <|
    "Back"->{1, 0, 0},
    "Left"->{0, 1, 0},
    "Down"->{0, 0, 1}
    |>;
axes["Front"]=-axes["Back"];
axes["Right"]=-axes["Left"];
axes["Up"]=-axes["Down"];


rotMat[key_]:=
  If[KeyExistsQ[axes, key],
    RotationMatrix[Pi/2., axes[key]],
    Identity
    ];
rotMat[key_, a_]:=
  If[KeyExistsQ[axes, key],
    RotationMatrix[a, axes[key]],
    Identity
    ];
permList[key_]:=
  Lookup[perms, key, {}];


apply[mat_, coords_]:=
  Transpose[mat.Transpose[coords]];


permute[clist_, tensor_]:=
  Fold[ReplacePart[#1, Thread[#2->Extract[#1,RotateRight[#2]]]]&, tensor, clist];
permute[clist_, rot_, tensor_]:=
  Fold[
    ReplacePart[#1,
      Thread[#2->(apply[rot, #]&/@Extract[#1,RotateRight[#2]])]]&,
    tensor,
    clist
    ];
twist[move_, cube_]:=permute[permList[move], rotMat[move],cube];
turn[move_, \[Alpha]_, part_]:=apply[rotMat[move, \[Alpha]],#]&/@part;


End[];


EndPackage[];
