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
      Opacity[0.]->"none"
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



toSVGString[RGBColor[r__], h_]:=
  "#"<>IntegerString[Floor[{r}*255], 16, 2];
toSVGString[h_?ColorQ, _]:=
  toSVGString@ColorConvert[h, RGBColor];
toSVGString[l_List, "stroke-dasharray"]:=
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
toSVGString[h:Except[List][x_], _]:=(* handling things like Rotate *)
  ToLowerCase@ToString[h]<>"("<>toSVGString[x]<>")";
toSVGString[a_, head_]:=
  Block[
    {
      $scaling=$symbScalingFactors
        (*If[head==="stroke-dasharray", $symbDashingFactors, $symbScalingFactors]*),
      vbmin=$svgViewBox[[3]](*Min[$svgViewBox[[3;;]]]*)
      },
    Replace[
      a//.{(* a bunch of junk to handle symbolic sizes *)
        k:Alternatives@@Keys[$scaling]:>
          Lookup[$scaling, k]*vbmin,
        {n_?NumericQ, m_?NumericQ}:>(ToString[n]<>","<>ToString[m])
        },
      {
        l_List:>StringRiffle[ToString/@l],
        e:Except[_Integer, _?NumericQ]:>ToString@N[e],
        e_:>ToString@e
        }
      ]
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



decamel[s_]:=
  StringTrim[
    StringJoin@
      StringSplit[s, l:LetterCharacter?(Not@*LowerCaseQ):>"-"<>ToLowerCase@l],
    Except[LetterCharacter]
    ]


toSVGProp[(form:EdgeForm|FaceForm|None)[key_], val_]:=
  Module[{k, m},
    k=ToString@key;
    m=
      Which[
        form===EdgeForm, 
          $SVGEdgePropMap, 
        form===FaceForm,
          $SVGFacePropMap,
        True,
          $SVGPropMap
        ];
    k=Lookup[m, k, Lookup[$SVGPropMap, k, If[StringQ@key, decamel[k], None]]];
    Which[
      k===None,
        Nothing,
      ListQ@k,
        MapThread[
          #->Lookup[$SVGValMap, Key@#, toSVGString[val, #]]&,
          {
            k,
            val
            }
          ],
      True,
        k->Lookup[$SVGValMap, Key@val, toSVGString[val, k]]
      ]
    ]


(* ::Subsubsubsection::Closed:: *)
(*toXML*)



toXML[svgElement[head_, attrs_]]:=
  XMLElement[head, KeyValueMap[toSVGProp, Association@attrs], {}]


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



ClearAll@aliasedQ


alias/:(alias[expr_]:=form_):=
  (
    aliasedQ[expr]=True;
    aliasTarget[expr]=
      FirstCase[
        HoldComplete[form],
        Alternatives@@Keys[$svgPrimitives],
        None,
        \[Infinity],
        Heads->True
        ];
    toEl[expr]:=toEl[form]
    )


(* ::Subsubsubsection::Closed:: *)
(*Fallthrough*)



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



register[svgPath, "path", {path_?StringQ->"d"}]


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


(* ::Subsubsection::Closed:: *)
(*Aliases*)



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



alias@
Circle[center_, rad:notOp[]:Automatic, ops:opsPat[]]:=
  Block[{$svgLineType=True},
    toEl[Disk[yAdjustCoord@center, rad, ops]]/.
      svgCircle[a__]:>svgCircle[a, Filling->None]
    ]


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
(*Line*)



alias@
Line[{pts:{_?NumericQ, _?NumericQ}..}, ops:opsPat[]]:=
  Block[{$svgLineType=True},
    svgLine[#, ops]&/@
      yAdjustCoord@Partition[Riffle[Most@pts, Rest@pts], 2]
    ]


(* ::Subsubsubsection::Closed:: *)
(*Rotate*)



alias@
Rotate[g_, q_]:=
  Append[g, TransformationFunction->Rotate[q*Degree]]


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
    prpad
    },
  imsize=
    4/3*Replace[OptionValue[Graphics, opp, ImageSize],
      {
        Automatic:>{360, 338},
        n_?NumericQ:>{n, n}(* this isn't actually right, but we'll roll with it for now *)
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
  viewbox=getViewBox[viewbox, prpad, imsize];
  XMLElement["svg",
    Flatten@{
      Thread[
        {"width","height"}->Map[ToString]@Floor@imsize
        ],
      "viewbox"->StringRiffle@Map[ToString]@viewbox,
      "version"->"1.1",
      {"http://www.w3.org/2000/xmlns/","xmlns"}->"http://www.w3.org/1999/xhtml"
      },
    Block[
      {
        $svgPlotRange=PlotRange@g,
        $svgViewBox=viewbox,
        $svgViewSize=imsize
        },
      toXML@exportGraphics[Join[Most[#], opp, {Last@#}]]&/@
        splitGraphicsList[First@g]
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
    viewbox=
      viewbox+Join[-prpad[[1, ;;2]], prpad[[1]]+prpad[[2]]]
    ]


(* ::Subsubsubsection::Closed:: *)
(*exportGraphics*)



exportGraphics//Clear


exportGraphics[{dir___, expr_}, ops:OptionsPattern[]]:=
  Block[{$svgLineType=False},
    toEl@
      Insert[
        expr,
        Flatten@{
          canonicalizeDirectives[{dir}, expr],
          ops
          },
        -1
        ]
    ]


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
  With[{targ=aliasTarget[expr]},
    Map[
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
canonicalizeDirective[head_[x_], _]:=
  If[$svgLineType, 
    Evaluate[EdgeForm[head]->x],
    Evaluate[FaceForm[head]->x]
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
canonicalizeDirective[e___]:=
  (Echo@{e};Nothing)


(* ::Subsubsubsection::Closed:: *)
(*splitGraphicsList*)



splitGraphicsListRec[gl_]:=
  Block[
    {direct=If[!ListQ@direct, {}, direct]},
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
  Flatten[Reap[splitGraphicsListRec[gl];][[2]], 1]


End[];


EndPackage[];



