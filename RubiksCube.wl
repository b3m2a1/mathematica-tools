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


constructRubiksCube//Clear
constructRubiksCube[ops___]:=
  Merge[{#, "Cuboids"->setupCuboids[#["Size"]]}, First]&@
    Merge[
      {
        ops,
        "Size"->3,
        "Origin"->{0., 0., 0.},
        "Colors"->{Black, Red, Orange, Blue, Green, Yellow, White}
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
makeRubiksCuboid[cols_, center_]:=
  <|
    "Coordinates"->Map[#+center&, getVertList[]],
    "ColorIndices"->cols
    |>;
makeCuboidGraphic[parent_, cuboid_]:=
  With[{o=parent@"Origin"},
    GraphicsComplex[
      #+o&/@cuboid["Coordinates"],
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
stickerPos//Clear;
stp[111]={0,0,0,1,5,3}; stp[112]={6,0,0,1,0,3};
stp[121]={0,0,4,1,5,0}; stp[122]={6,0,4,1,0,0};
stp[211]={0,2,0,0,5,3}; stp[212]={6,2,0,0,0,3};
stp[221]={0,2,4,0,5,0}; stp[222]={6,2,4,0,0,0};
stickerPos[2]=
  {
    (* back: {0,0,0,1,0,0} *)
    {
      {stp[111],stp[112]},
      {stp[121],stp[122]}
      },
    (* front, {0,2,0,0,0,0} *)
    {
      {stp[211],stp[212]},
      {stp[221],stp[222]}
      }
    };
stp[113]={0,0,0,1,0,3};
stp[131]={0,0,0,1,5,0}; stp[132]={0,0,0,1,0,0}; stp[133]={6,0,0,1,0,0};
stp[123]={0,0,4,1,0,0};
{
  {stp[311], stp[312], stp[313]},
  {stp[321], stp[322], stp[323]},
  {stp[331], stp[332], stp[333]}
  } = {
    {{0,0,0,0,5,3},{0,0,0,0,0,3},{6,0,0,0,0,3}},
    {{0,0,0,0,5,0},{0,0,0,0,0,0},{6,0,0,0,0,0}},
    {{0,0,4,0,5,0},{0,0,4,0,0,0},{6,0,4,0,0,0}}
    };
stp[213]={0,2,0,0,0,3};
stp[231]={0,2,0,0,5,0}; stp[232]={0,2,0,0,0,0}; stp[233]={6,2,0,0,0,0};
stp[223]={0,2,4,0,0,0};
reep[e_, n_]:=
  Sequence@@ConstantArray[e, n-2]
reep[n_][e_]:=
  reep[e, n];
stickerPos[n_?(#>2&)]:=
  {
    (* back: {0,0,0,1,0,0} *)
    {
      {stp[111], stp[113]//reep[n], stp[112]},
      {stp[131], stp[132]//reep[n], stp[133]}//reep[n],
      {stp[121], stp[123]//reep[n], stp[122]}
    },
    (* middle, {0,0,0,0,0,0} *)
    {
      {stp[311], stp[312]//reep[n], stp[313]},
      {stp[321], stp[322]//reep[n], stp[323]}//reep[n],
      {stp[331], stp[332]//reep[n], stp[333]}
      }//reep[n],
    (* front, {0,2,0,0,0,0} *)
    {
      {stp[211], stp[213]//reep[n], stp[212]},
      {stp[231], stp[232]//reep[n], stp[233]}//reep[n],
      {stp[221], stp[223]//reep[n], stp[222]}
    }
   };


originalCenters//Clear;
originalCenters[n_?(#>1&)] := 
  Outer[List, #, #, #]&@
    If[EvenQ[n],
      Range[-n/2, (n/2)-1]+.5,
      Range[-Floor[n/2], Floor[n/2]]
      ];


setupCuboids//Clear;
setupCuboids[size_]:=
  With[
    {
      stickers = 1+stickerPos[size], 
      origins = originalCenters[size]
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
    PlotRange->Sqrt[2]*Ceiling[(cube["Size"]/2)],
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


twistRubik[cube_, theta_, face_]:=
  Module[
    {
      mp, 
      pos = Map[#["Coordinates"]&, cube["Cuboids"], {3}],
      module,
      s = cube["Size"],
      allPositions,
      numbering
      },
    allPositions=
      Round[Flatten[If[EvenQ[s], .5, 1]+originalCenters[s]+Floor[s/2], 2]];
    numbering=AssociationThread[allPositions, Range[Length[allPositions]]];
    mp=Flatten[permList[s, face], 1];
    pos=
      Join[
        Extract[pos, Complement[allPositions, mp]],
        turn[face, theta, Extract[pos, mp]]
        ];
    pos = 
      ArrayReshape[
        pos[[
          Ordering@Lookup[numbering, Key/@Join[Complement[allPositions, mp], mp]]
          ]], 
        {s, s, s, 8, 3}
        ];
    setPos[cube, pos]
    ];


(* ::Text:: *)
(*Everything from here down is just to implement face-specific rotations and permutations*)


(* cube permutation data *)
perms//Clear
basePerm[row_, type_, m_, s_]:=
   {
    Sequence@@
      Flatten[
        Table[
          Table[
            With[{s2=s-(n-1), p2=p-n},
              {
                {row, p, n}, {row, s2, p}, 
                {row, (s2-p2), s2}, {row, n, (s2-p2)}
                }
              ],
            {p, n, s-n}
            ],
          {n, Floor[s/2]}
          ],
        1
        ][[All, All, RotateRight[{1, 2, 3}, type-1]]],
    If[OddQ[s], {{1, m, m}}, Nothing]
    };
perms[size_]:=
  perms[size]=
    Module[
      {
       m=Ceiling[size/2], s=size,
       baseAss
       },
       baseAss=
        AssociationMap[
          basePerm[#[[2]], Replace[#[[1]], {"X"->1, "Y"->2, "Z"->3}], m, s]&,
          Tuples[
            {
              {"X", "Y", "Z"},
              Range[size]
              }
            ]
          ];
       baseAss["Back"] = baseAss[{"X", 1}];
       baseAss["Left"] = baseAss[{"Y", 1}];
       baseAss["Down"] = baseAss[{"Z", 1}];
       baseAss["Front"]= Reverse/@baseAss[{"X", size}];
       baseAss["Right"]= Reverse/@baseAss[{"Y", size}];
       baseAss["Up"]= Reverse/@baseAss[{"Z", size}];
       baseAss
       ];


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


axisVec[k_]:=
  Lookup[axes, Key[k], None];
axisVec[{"X", _}]:=
  axes["Back"];
axisVec[{"Y", _}]:=
  axes["Left"];
axisVec[{"Z", _}]:=
  axes["Down"];


rotMat[key_]:=
  Replace[axisVec[key],
    {
      None:>IdentityMatrix[3],
      ax_:>RotationMatrix[Pi/2., ax]
      }
    ];
rotMat[key_, a_]:=
  Replace[axisVec[key],
    {
      None:>IdentityMatrix[3],
      ax_:>RotationMatrix[a, ax]
      }
    ];
permList[size_, key_]:=
  Lookup[perms[size], Key@key, {}];


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
twist[move_, cube_]:=permute[permList[cube["Size"], move], rotMat[move],cube];
turn[move_, \[Alpha]_, part_]:=apply[rotMat[move, \[Alpha]], #]&/@part;


End[];


EndPackage[];
