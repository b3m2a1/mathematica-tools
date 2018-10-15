(* ::Package:: *)



BeginPackage["SVG`"];


(* ::Text:: *)
(*
	Simple SVG primitive support
*)



ToSVG::usage=
  "Export to an SVG XMLObject"


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*toXML*)



(* ::Subsubsubsection::Closed:: *)
(*toNotStupidString*)



(* ::Text:: *)
(*
	ToString[x*10^7] is ridiculously stupid so we need to do this to fix it
*)



toNotStupidString[n_?IntegerQ]:=
  StringTrim[Quiet@ToString@NumberForm[n, ExponentFunction->(Null&)], "."];
toNotStupidString[n_?NumericQ]:=
  StringTrim[Quiet@ToString@NumberForm[N@n, ExponentFunction->(Null&)], "."];
toNotStupidString[e_]:=
  ToString@e


(* ::Subsubsubsection::Closed:: *)
(*$SVGColorMap*)



$SVGColorMap=
  KeyMap[
    ReleaseHold,
    AssociationMap[
      Replace[Hold[a_]:>ToLowerCase@SymbolName[Unevaluated[a]]],
      Thread@Hold@{
        Black, White, Gray,
        Red, Blue, Green, Yellow,
        Orange, Pink, Purple
        }
      ]
    ]


(* ::Subsubsubsection::Closed:: *)
(*$SVGValMap*)



$SVGValMap=
  Join[
    $SVGColorMap,
    KeyMap[N, $SVGColorMap],
    <|
      (* other directly convertable values *)
      None->"none",
      Opacity[0]->"none",
      Opacity[0.]->"none",
      Automatic->"auto"
      |>
    ]


(* ::Subsubsubsection::Closed:: *)
(*ScalingFactors*)



$symbScalingFactors=
  6*10^-2*<|
    Tiny->.005,
    Small->.01,
    Medium->.05,
    Large->.1
    |>


$symbDashingFactors=
  Map[#*{2, .6}&]@<|
    Tiny->.005,
    Small->.01,
    Medium->.05,
    Large->.1
    |>


(* ::Subsubsubsection::Closed:: *)
(*toSVGString*)



(* ::Text:: *)
(*
	This attempts to take an SVG option value and get a proper string representation for it. There is some hacky-ness as handling of things like Dashing[{Small, Small}] must be different from that of Thickness[Large]. Hopefully this can be cleaned sometime.
*)



toSVGString//Clear


toSVGString[RGBColor[r__], h__]:=
  "#"<>IntegerString[Floor[{r}*255], 16, 2];
toSVGString[h_?ColorQ, __]:=
  toSVGString[ColorConvert[h, RGBColor], None, None];
toSVGString[v_, _, "AbsoluteThickness"]:=
  toSVGString[v, None, None]<>"pt";
toSVGString[l_List, _, "Dashing"]:=
  StringRiffle@Map[ToString]@
    Module[
      {
        vbmin=$svgViewBox[[3]](*Min[$svgViewBox[[3;;]]]*)
        },
      MapIndexed[
        Replace[$symbDashingFactors[#],
          {
            m_Missing:>#,
            e_:>e[[Mod[#2[[1]], 2, 1]]]*vbmin
            }
          ]&,
        l
        ]
      ];
toSVGString[(h:Except[List])[x_], _, _]:=(* handling things like Rotate *)
  ToLowerCase@ToString[h]<>"("<>toSVGString[x, None, None]<>")";
toSVGString[a_, head_, _]:=
  Block[
    {
      $scaling=$symbScalingFactors
        (*If[head==="stroke-dasharray", $symbDashingFactors, $symbScalingFactors]*),
      vbmin=$svgViewBox[[3]](*Min[$svgViewBox[[3;;]]]*)
      },
    a//.{(* a bunch of junk to handle symbolic sizes *)
        k:Alternatives@@Keys[$scaling]:>
          Lookup[$scaling, k]*vbmin
        }//ReplaceAll@{
        {n_?NumericQ, m_?NumericQ}:>
          (
            toNotStupidString[n]<>","<>toNotStupidString[m]
            ),
        e:_?NumericQ:>toNotStupidString[e]
        }//
      Replace@{
        l_List:>StringRiffle[ToString/@l],
        e_:>ToString@e
        }
    ]


(* ::Subsubsubsection::Closed:: *)
(*$SVGPropMap*)



$SVGFacePropMap=
  KeyMap[SymbolName]@<|
    Filling->"fill",
    Opacity->"fill-opacity",
    RoundingRadius->{"rx", "ry"}
    |>;
$SVGEdgePropMap=
  KeyMap[SymbolName]@<|
    LineColor->"stroke",
    Filling->"stroke",
    RoundingRadius->"stroke-round",
    CapForm->"stroke-linecap",
    Thickness->"stroke-width",
    AbsoluteThickness->"stroke-width",
    JoinForm->"stroke-linejoin",
    Dashing->"stroke-dasharray",
    AbsoluteDashing->"stroke-dasharray"
    |>;


$SVGPropMap=
  KeyMap[SymbolName]@<|
    TransformationFunction->"transform",
    FontFamily->"font"
    |>


(* ::Subsubsubsection::Closed:: *)
(*toSVGProp*)



(* ::Text:: *)
(*
	Builds an attribute rule for an SVG elements. This has to handle face and edge elements differently so there are different prop maps for each.
*)



toSVGProp//Clear


(*decamel[s_]:=
	StringTrim[
		StringJoin@
			StringSplit[s, l:LetterCharacter?(Not@*LowerCaseQ)\[RuleDelayed]"-"<>ToLowerCase@l],
		Except[LetterCharacter]
		]*)


toSVGProp[(form:EdgeForm|FaceForm|None)[key_], val_]:=
  Module[{k1, k, m},
    k1=k=ToString@key;
    m=
      Which[
        form===EdgeForm, 
          $SVGEdgePropMap, 
        form===FaceForm,
          $SVGFacePropMap,
        True,
          $SVGPropMap
        ];
    k=Lookup[m, k, Lookup[$SVGPropMap, k, If[StringQ@key, k(*decamel[k]*), None]]];
    Which[
      k===None,
        Nothing,
      ListQ@k,
        MapThread[
          #->Lookup[$SVGValMap, Key@#2, toSVGString[#2, #, k1]]&,
          {
            k,
            val
            }
          ],
      True,
        k->Lookup[$SVGValMap, Key@val, toSVGString[val, k, k1]]
      ]
    ];
toSVGProp[key_, val_]:=
  toSVGProp[None@key, val]


(* ::Subsubsubsection::Closed:: *)
(*toXML*)



toXML//Clear


toXML[svgElement[head_, attrs_]]:=
  XMLElement[head, 
    DeleteDuplicatesBy[First]@KeyValueMap[toSVGProp,
      KeyDrop[Association@attrs, "Body"]
      ],
    Flatten@List@ReplaceAll[
      Lookup[attrs, "Body", {}],
      s:(Alternatives@@Map[Blank, Prepend[Keys@$svgPrimitives, svgElement]]):>
        toXML@toEl[s]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*toEl*)



(* ::Text:: *)
(*
	This defines the DSL and primary aliasing for Mathematica objects. Extra rules can be added in the Aliases section.
*)



ClearAll@toEl


toEl~SetAttributes~Listable


opsPatRule=(_FaceForm|_EdgeForm|_String|_Symbol)->_;
opsPat[]=___?(MatchQ[Flatten[{#}], {opsPatRule...}]&);
notOp[]:=_?(Not@MatchQ[Flatten@{#}, {opsPatRule...}]&)


(* ::Subsubsubsection::Closed:: *)
(*register*)



$svgPrimitives=
  <|
    
    |>;


register[fn_, head_, args__]:=
  With[{flargs=Flatten@{args}},
    $svgPrimitives[fn]=head;
    Replace[
      {
        {args}/.(pat_->key_String):>pat,
        Cases[flargs, (_->key_String):>key],
        Thread[
          Replace[flargs, 
            {
              (Verbatim[Pattern][a_, _]->key_String):>Hold[a],
              (
                (Optional|PatternTest|Condition)[Verbatim[Pattern][a_, _], _]->key_String
                ):>Hold[a],
              _->Nothing
              }, 
            1
            ], 
          Hold
          ]
        },
      {
        {
          {pats__},
          vars_,
          Hold[vals_]
          }:>
        (
          toEl[fn[pats, ops:opsPat[]]]:=
            svgElement[head, 
              Merge[
                {
                  Thread[vars->vals],
                  ops
                  },
                First
                ]
              ]
          ),
        _:>Failure["RegistrationFailure",
          <|
            "MessageTemplate"->
              "Failed to register conversion rule for head `` and type `` with argspec ``",
            "MessageParameters":>
              {fn, head, {args}}
            |>
          ]
        }
    ]
  ]


(* ::Subsubsubsection::Closed:: *)
(*alias*)



ClearAll@alias;
ClearAll@aliasedQ;
ClearAll@aliasTarget;
ClearAll@lineTypeAlias;


alias/:(alias[lineType:True|False][expr_]:=form_):=
  Block[{$svgLineType=False},
    aliasedQ[expr]=True;
    lineTypeAlias[expr]=lineType;
    aliasTarget[expr]=
      FirstCase[
        HoldComplete[form],
        Alternatives@@Keys[$svgPrimitives],
        None,
        \[Infinity],
        Heads->True
        ];
    toEl[expr]:=toEl[form]
    ];
alias/:(alias[expr_]:=form_):=
  (alias[False][expr]:=form)


(* ::Subsubsubsection::Closed:: *)
(*Fallthrough*)



toEl[e:_svgElement|_XMLElement]:=
  e;


toEl[e_]:=
  Failure["XMLConversion", 
    <|
      "MessageTemplate"->"Failed to convert ``",
      "MessageParameters":>{e}
      |>
    ]


(* ::Subsubsubsection::Closed:: *)
(*circle*)



register[svgCircle, "circle", 
  {cx_?NumericQ->"cx", cy_?NumericQ->"cy"}, 
  r_?NumericQ->"r"
  ]


(* ::Subsubsubsection::Closed:: *)
(*ellipse*)



register[svgEllipse, "ellipse", 
  {cx_?NumericQ->"cx", cy_?NumericQ->"cy"}, 
  {rx_?NumericQ->"rx", ry_?NumericQ->"ry"}
  ]


(* ::Subsubsubsection::Closed:: *)
(*line*)



register[svgLine, "line", 
  {
    {x1_?NumericQ->"x1", y1_?NumericQ->"y1"}, 
    {x2_?NumericQ->"x2", y2_?NumericQ->"y2"}
    }
  ]


(* ::Subsubsubsection::Closed:: *)
(*path*)



register[svgPath, "path", path_?StringQ->"d"]


(* ::Subsubsubsection::Closed:: *)
(*polygon*)



register[svgPolygon, "polygon", 
  pts:{
    {_?NumericQ, _?NumericQ}..
    }->"points"
  ]


(* ::Subsubsubsection::Closed:: *)
(*polyline*)



register[svgPolyline, "polyline", 
  pts:{
    {_?NumericQ, _?NumericQ}..
    }->"points"
  ]


(* ::Subsubsubsection::Closed:: *)
(*rect*)



register[svgRect, "rect", 
  {x_?NumericQ->"x", y_?NumericQ->"y"},
  {w_?NumericQ->"width", h_?NumericQ->"height"}
  ]


(* ::Subsubsubsection::Closed:: *)
(*marker*)



register[svgMarker, "marker", id_?StringQ->"id", body_->"Body"]


(* ::Subsubsubsection::Closed:: *)
(*text*)



register[svgText, "text", body_->"Body", {x_?NumericQ->"x", y_?NumericQ->"y"}]


(* ::Subsubsection::Closed:: *)
(*Aliases*)



(* ::Subsubsubsection::Closed:: *)
(*yAdjustCoord*)



(* ::Text:: *)
(*
	Little function to get the coordinate system corrected
*)



yAdjustCoordC=
  Compile[{{xy, _Real, 1}, {pr, _Real, 2}},
    {xy[[1]], pr[[2, 2]]-(xy[[2]]-pr[[2, 1]])},
    RuntimeOptions->"Speed",
    RuntimeAttributes->{Listable}
    ];
yAdjustCoord[blah_]:=
  yAdjustCoordC[blah, $svgPlotRange]


(* ::Subsubsubsection::Closed:: *)
(*Disk*)



alias@
Disk[center_, rad:notOp[]:Automatic, ops:opsPat[]]:=
  If[ListQ@rad,
    MapThread[svgCircle[##, ops]&, 
      {
        yAdjustCoord@center, 
        Replace[rad, {
          r_?NumericQ:>ConstantArray[r, Length@center],
          Except[{__?NumericQ}]:>
            ConstantArray[1., Length@center],
          e_:>
            PadRight[e, Length@center, e][[;;Length@center]]
          }]
        }
      ],
    svgCircle[yAdjustCoord@center, 
      Replace[rad,
        Except[_?NumericQ]->1
        ], 
      ops]
    ]


(* ::Subsubsubsection::Closed:: *)
(*Circle*)



alias[True]@
Circle[center_, rad:notOp[]:Automatic, ops:opsPat[]]:=
  toEl[Disk[yAdjustCoord@center, rad, ops]]/.
    svgElement["circle", a_]:>svgElement["circle", Append[a, FaceForm[Filling]->None]]


(* ::Subsubsubsection::Closed:: *)
(*Ellipsoid*)



alias@
Ellipsoid[center_, radii_, ops:opsPat[]]:=
  svgEllipsoid[yAdjustCoord@center, radii, ops]


(* ::Subsubsubsection::Closed:: *)
(*Rectangle*)



alias@
Rectangle[{xmin_, ymin_}, {xmax_, ymax_}, ops:opsPat[]]:=
  svgRect[yAdjustCoord@{xmin, ymin}, {xmax-xmin, ymax-ymin}, ops]


alias@
Rectangle[{xmin_, ymin_}, ops:opsPat[]]:=
  svgRect[yAdjustCoord@{xmin, ymin}, Lookup[Flatten@{ops}, ImageSize, {1, 1}], ops]


(* ::Subsubsubsection::Closed:: *)
(*Polygon*)



alias@
Polygon[pts:{{_?NumericQ, _?NumericQ}..}, ops:opsPat[]]:=
  svgPolygon[yAdjustCoord@pts, ops]


alias@
Polygon[poly:{{{_?NumericQ, _?NumericQ}..}..}, ops:opsPat[]]:=
  svgPolygon[yAdjustCoord@#, ops]&/@poly


(* ::Subsubsubsection::Closed:: *)
(*Line*)



alias[True]@
Line[pts:{{_?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}}, ops:opsPat[]]:=
  svgLine[yAdjustCoord@pts, ops]


alias[True]@
Line[pts:{{_?NumericQ, _?NumericQ}..}, ops:opsPat[]]:=
  With[{dat=yAdjustCoord@pts},
    svgPolyline[yAdjustCoord@pts, FaceForm[Filling]->None, ops]
    ]


(* ::Subsubsubsection::Closed:: *)
(*Arrow*)



arrowMarker//Clear


arrowMarker[ops:opsPat[]]:=
  Module[
    {
      heads=Lookup[ops, EdgeForm@Arrowheads, ops]
      },
    svgMarker["arrow-"<>StringTake[ToString@Hash@ops, 5],
      svgPath@"M0,0 L0,6 L9,3 z",
      "markerWidth"->10,
      "markerHeight"->10,
      "refX"->0,
      "refY"->0,
      "orient"->Automatic,
      "markerUnits"->"strokeWidth",
      ops
      ]
    ]


alias[True]@
Arrow[pts:{{_?NumericQ, _?NumericQ}..}, setBack:notOp[]:Automatic, ops:opsPat[]]:=
  (* I'm ignoring setBack for now but I'll have to handle it some day... *)
  With[{mark=arrowMarker[ops]},
    Sow@mark;
    Line[pts, ops, "marker"->URL[mark[[1]]]]
    ]


(* ::Subsubsubsection::Closed:: *)
(*GraphicsComplex*)



alias@
g:GraphicsComplex[pts_, data_, o___]:=
  toSVGCore[
    Flatten@Replace[Normal@g,
      gc_GraphicsComplex:>
        With[{map=AssociationThread[Range[Length@pts], pts]},
          data/.map (* this isn't actually going to be entirely clean, but it's good enough for now *)
          ]
      ],
    {o}
    ]


(* ::Subsubsubsection::Closed:: *)
(*Rotate*)



(* ::Text:: *)
(*
	This doesn\[CloseCurlyQuote]t actually work yet...
*)



alias@
Rotate[g_, q_]:=
  Append[g, TransformationFunction->Rotate[q*Degree]]


(* ::Subsubsubsection::Closed:: *)
(*Annotation*)



alias@
Annotation[a_, d_, ___]:=
  toSVGCore[a,
    {"AnnotationData"->d}
    ];


(* ::Subsubsection::Closed:: *)
(*ToSVG*)



(* ::Subsubsubsection::Closed:: *)
(*ToSVG*)



(* ::Text:: *)
(*
	This is the main function. Currently I only allow Graphics to be passed in.
	It parse the directives into rules which are all wrapped in FaceForm or EdgeForm to specify to which they should apply.
	These then get fed to the DSL convert, then finally get pushed through to the XMLElement generator. The latter probably needs the most work.
*)



ToSVG//Clear
ToSVG[g_Graphics, ops:OptionsPattern[]]:=
Module[
  {
    opp=Flatten@{ops, Options[g]},
    imsize,
    viewbox,
    pr,
    prpad,
    svgBody,
    defs,
    extra
    },
  imsize=
    4/3*Replace[OptionValue[Graphics, opp, ImageSize],
      {
        Automatic:>{360, 338},
        n_?NumericQ:>
          {n, Replace[OptionValue[Graphics, opp, AspectRatio], Except[_?NumericQ]->1]*n}
        }
      ];
  viewbox=Flatten@Transpose@PlotRange@g;
  prpad=
    Replace[OptionValue[Graphics, opp, PlotRangePadding],
      {
        i:_?NumericQ|_Scaled:>{i, i},
        Except[{_, _}]->{Scaled[.02], Scaled[.02]}
        }
      ];
  {prpad, viewbox}=getViewBox[viewbox, prpad, imsize];
  pr=PlotRange@g;
  extra=axesAndFrame[opp, pr, prpad];
  Block[
    {
      $svgPlotRange=pr,
      $svgViewBox=viewbox,
      $svgViewSize=imsize
      },
    Quiet[
      svgBody=Reap[toXML/@Flatten@toSVGCore[Throw@{extra, First@g}, opp]];
      defs=Flatten@svgBody[[2]],
      {
        (* optx messages don't matter here, so we quiet them if they show up *)
        GraphicsComplex::optx,
        GraphicsComplex::optrs
        }
      ];
    XMLElement["svg",
      Flatten@{
        Thread[
          {"width","height"}->Map[toNotStupidString]@Floor@imsize
          ],
        "viewbox"->StringRiffle@Map[toNotStupidString]@viewbox,
        "version"->"1.1",
        {"http://www.w3.org/2000/xmlns/","xmlns"}->"http://www.w3.org/1999/xhtml"
        },
      If[Length@defs>0,
        Prepend@XMLElement["defs",
          {},
          toXML@*toEl/@DeleteDuplicates@defs
          ],
        Identity
        ]@
        svgBody[[1]]
      ]
    ]
  ]


(* ::Subsubsubsection::Closed:: *)
(*getViewBox*)



(* ::Text:: *)
(*
	Handling things like the PlotRangePadding is non-trivial. 
	This is probably only like 50% of the way there.
*)



getViewBox[view_, pad_, size_]:=
  Module[
    {
      viewbox=view,
      prpad=pad
      },
    viewbox=Join[viewbox[[;;2]], viewbox[[3;;]]-viewbox[[;;2]]];
    prpad=
      Replace[
        prpad,
        {
          {i:_?NumericQ|_Scaled, j:_?NumericQ|_Scaled}:>
            ({{i/2, j/2}, {i/2, j/2}}/.(Scaled[a_]/2):>Scaled[a/2]),
          {l_List, e:Except[_List]}:>
            {l, {e, e}},
          {e:Except[_List], l_List}:>
            {{e, e}, l}
          }
        ];
    prpad=
      prpad//.{
        {Scaled[x_], y_}:>
          {viewbox[[3]]*x, y},
        {x_, Scaled[y_]}:>
          {x, viewbox[[4]]*y}
        };
    {
      prpad,
      viewbox+Join[-prpad[[1, ;;2]], prpad[[1]]+prpad[[2]]]
      }
    ]


(* ::Subsubsubsection::Closed:: *)
(*axesAndFrame*)



getPlotTicksObjects[{{xmin_, xmax_}, {ymin_, ymax_}}, {x0_, y0_}]:=
  Module[{xbase, ybase, ends},
    xbase=Charting`ScaledTicks[{Identity,Identity}][xmin, xmax, 25];
    ybase=Rest@Charting`ScaledTicks[{Identity,Identity}][ymin, ymax, 25];
    {xbase, ybase}=
      MapIndexed[
        If[Mod[#2[[1]], 5]==4, 
          ReplacePart[#, 3->{1.1, 1}*#[[3]]],
          ReplacePart[#, 2->""]
          ]&,
        #
        ]&/@{xbase, ybase};
    {
      Function[
        ends={{#[[1]], y0-#[[3, 2]]}, {#, y0+#[[3, 1]]}};
        {#[[-1]], If[#[[2]]=!="", Text[#[[2]], ends[[1]]], Nothing], Line[ends]}
        ]/@xbase,
      Function[
        ends={{x0-#[[3, 2]], #[[1]]}, {x0+#[[3, 1]], #}};
        {#[[-1]], If[#[[2]]=!="", Text[#[[2]], ends[[1]]], Nothing], Line[ends]}
        ]/@ybase
      }
    ]


axesAndFrame[opp_, prange_, pad_]:=
  Module[
    {
      pr=prange+pad,
      axes,
      aticks,
      ast,
      orgy,
      axesLines,
      fst,
      frame,
      frameLines,
      axesTicks,
      frameTicks
      },
    axes=
      Replace[OptionValue[Graphics, opp, Axes], 
        {
          Automatic->{False, False},
          e:Except[{_, _}]:>{e, e}
          }
        ];
    orgy=
      Replace[OptionValue[Graphics, opp, AxesOrigin], 
        {
          e:Except[{_, _}]:>{prange[[1, 1]], prange[[2, 1]]}
          }
        ];
    ast=Flatten@OptionValue[Graphics, opp, AxesStyle];
    axesLines=
      If[Or@@Map[TrueQ, axes],
        {
          ast,
          If[TrueQ@axes[[1]],
            Line[Thread[{pr[[1]], orgy[[2]]}]],
            Nothing
            ],
          If[TrueQ@axes[[2]],
            Line[Thread[{orgy[[1]], pr[[2]]}]],
            Nothing
            ]
          },
        Nothing
        ];
    aticks=
      Replace[OptionValue[Graphics, opp, Ticks], 
        {
          Automatic->{axes[[1]], axes[[2]]},
          e:Except[{_, _}]:>{e, e}
          }
        ];
    aticks[[1]]=TrueQ[aticks[[1]]]||(aticks[[1]]===Automatic&&axes[[1]]);
    aticks[[2]]=TrueQ[aticks[[2]]]||(aticks[[2]]===Automatic&&axes[[2]]);
    axesTicks=
      {
        If[aticks[[1]], #[[1]], Nothing],
        If[aticks[[2]], #[[2]], Nothing]
        }&@getPlotTicksObjects[pr, orgy];
    frame=
      Replace[OptionValue[Graphics, opp, Frame], 
        {
          Automatic->
            {
              {False, False},
              {False, False}
              },
          f:{{_, _}, {_, _}}:>f,
          {a_, b_}:>
            {{a, b}, {a, b}},
          e:Except[{_, _}]:>{{e, e}, {e, e}}
          }
        ];
    fst=
      Replace[OptionValue[Graphics, opp, FrameStyle], 
        {
          Automatic->
            {
              {{}, {}},
              {{}, {}}
              },
          f:{{_, _}, {_, _}}:>f,
          {a_, b_}:>
            {{a, b}, {a, b}},
          e:Except[{_, _}]:>{{e, e}, {e, e}}
          }
        ];
    frameLines=
      If[Or@@Map[TrueQ, Flatten@frame],
        {
          If[TrueQ@frame[[1, 1]]&&fst[[1, 1]]=!=None,
            {fst[[1, 1]], Line@Thread[{pr[[1, 1]], pr[[2]]}]},
            Nothing
            ],
          If[TrueQ@frame[[1, 2]]&&fst[[1, 2]]=!=None,
            {fst[[1, 2]], Line@Thread[{pr[[1]], pr[[2, 1]]}]},
            Nothing
            ],
          If[TrueQ@frame[[2, 1]]&&fst[[2, 1]]=!=None,
            {fst[[2, 1]], Line@Thread[{pr[[1, 2]], pr[[2]]}]},
            Nothing
            ],
          If[TrueQ@frame[[2, 2]]&&fst[[2, 2]]=!=None,
            {fst[[2, 2]], Line@Thread[{pr[[1]], pr[[2, 2]]}]},
            Nothing
            ]
          },
        Nothing
        ];
    {
      axesLines,
      axesTicks,
      frameLines
      (* need to do the FrameTicks stuff *)
      }
    ]


(* ::Subsubsubsection::Closed:: *)
(*toSVGCore*)



toSVGCore[e_, opp_]:=
  exportGraphics[Join[Most[#], opp, {Last@#}]]&/@
    splitGraphicsList[e]


(* ::Subsubsubsection::Closed:: *)
(*exportGraphics*)



exportGraphics//Clear


exportGraphics[{dir___, expr_}, ops:OptionsPattern[]]:=
  toEl@
    Insert[
      expr,
      Flatten@{
        canonicalizeDirectives[{dir}, expr],
        ops
        },
      -1
      ];


(* ::Subsubsubsection::Closed:: *)
(*canonicalizeDirectives*)



(* ::Text:: *)
(*
	I leave the expr there in case it\[CloseCurlyQuote]s someday useful. Every format conversion
*)



(* ::Subsubsubsubsection::Closed:: *)
(*canonicalizeDirectives*)



canonicalizeDirectives//Clear


canonicalizeDirectives[{}, expr_]:=
  {};
canonicalizeDirectives[dir_, expr_]:=
  Block[
    {
      targ=aliasTarget[expr],
      $svgLineType=TrueQ@lineTypeAlias[expr]
      },
    DeleteDuplicatesBy[First]@Map[
      canonicalizeDirective[#, targ]&,
      {dir}//.
        {
          e:EdgeForm[__]:>Thread[e, Directive],
          e:FaceForm[__]:>Thread[e, Directive],
          Directive[d___]:>d
          }//Flatten
      ]
    ];


(* ::Subsubsubsubsection::Closed:: *)
(*canonicalizeDirective*)



canonicalizeDirective//ClearAll


canonicalizeDirective[c_?ColorQ, _]:=
  If[$svgLineType, 
    Evaluate[EdgeForm[Filling]->c],
    Evaluate[FaceForm[Filling]->c]
    ];
canonicalizeDirective[FaceForm[x_], targ_]:=
  Block[{$svgLineType=False},
    canonicalizeDirective[x, targ]
    ];
canonicalizeDirective[EdgeForm[x_], targ_]:=
  Block[{$svgLineType=True},
    canonicalizeDirective[x, targ]
    ];
canonicalizeDirective[r:_Rule|_RuleDelayed, _]:=
  r;
canonicalizeDirective[(head:Except[List])[x_, ___], _]:=
  If[$svgLineType, 
    Evaluate[EdgeForm[head]->x],
    Evaluate[FaceForm[head]->x]
    ];
canonicalizeDirective[e___]:=
  (Nothing)


(* ::Subsubsubsection::Closed:: *)
(*$defaultDirectives*)



$defaultDirectives=
  {
    EdgeForm@
      Directive[Black, Thickness[.005]],
    FaceForm[Black]
    }


(* ::Subsubsubsection::Closed:: *)
(*splitGraphicsList*)



splitGraphicsListRec[gl_]:=
  Block[
    {direct=direct},
    Which[
      ListQ@#, 
        splitGraphicsListRec[#],
      TrueQ@aliasedQ[#],
        Sow@Append[Reverse@Flatten@{direct}, #],
      True,
        direct={direct, #}
      ]&/@Flatten[{gl}, 1]
    ] 


splitGraphicsList[gl_]:=
  Block[{direct=$defaultDirectives},
    Flatten[Reap[splitGraphicsListRec[gl];][[2]], 1]
    ]


End[];


EndPackage[];



