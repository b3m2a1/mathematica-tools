(* ::Package:: *)

(* ::Section:: *)
(*DynamicWebImage*)


BeginPackage["DynamicWebImage`"];


(*Package Declarations*)
DynamicWebImage::usage="DynamicWebImage[imgVar, caching, ops]";
DynamicWebImageBrowser::usage="DynamicWebImageBrowser[img, list]";


Begin["`Private`"];


(* ::Section:: *)
(*Caching Dynamic Image Browser*)


DynamicWebImage[
  imgVar_Dynamic, 
  caching : True | False : True, 
  ops : OptionsPattern[Join[Options@DynamicModule, Options@Image]]
  ] :=
 DynamicModule[
  {img, serialno},
  With[{
    imops =
     FilterRules[{ops},
      Options@Image
      ]
    },
   Replace[
    imgVar, {
     Verbatim[Dynamic][var_, o___?OptionQ] :>
      Dynamic[
       img =
        Image[
         DynamicWebImage`Private`qwantSingleSearch[var]["Image"],
         imops
         ],
       o],
     Verbatim[Dynamic][var_, f : Except[_?OptionQ], o___?OptionQ] :>
      Dynamic[
       img =
        Image[
         Last@Through[
           
           Flatten[List@f][
            DynamicWebImage`Private`qwantSingleSearch[var]]
           ],
         imops
         ],
       o
       ]
     }]
   ],
  Initialization :>
   {
    serialno =
     Max@
      Cases[
       Flatten@List@ToExpression@
          StringTrim[
           Names["FE`DynamicModuleVariableList$*"],
           "FE`DynamicModuleVariableList$"
           ],
       _Integer
       ],
    OptionValue[Initialization],
    If[! AssociationQ@DynamicWebImage`Private`$qwantCache, 
     DynamicWebImage`Private`$qwantCache = <||>
     ],
    DynamicWebImage`Private`qwantSingleSearch[ 
      q_String?(StringLength[StringTrim[#]] > 0 &)] :=
     Lookup[
      DynamicWebImage`Private`$qwantCache,
      q,
      With[{
        r =
         First@
          DynamicWebImage`Private`qwantInstance["ImageSearch", 
           "q" -> q, "count" -> "1"]
        },
       If[caching, DynamicWebImage`Private`$qwantCache[q] = r, r]
       ]
      ],
    DynamicWebImage`Private`qwantSingleSearch[__] :=
     None,
    If[! TrueQ[DynamicWebImage`Private`qwantPacletUpdated],
     DynamicWebImage`Private`qwantPacletUpdated = True;
     If[Length@PacletManager`PacletFind["ServiceConnection_Qwant"] > 0,
       PacletInstall,
       PacletUpdate
       ][
      "ServiceConnection_Qwant", 
      "Site" -> 
       "https://www.wolframcloud.com/objects/b3m2a1.paclets/PacletServer"
      ]
     ],
    DynamicWebImage`Private`qwantInstance = ServiceConnect["Qwant"]
    },
  Evaluate[
   Sequence @@
    FilterRules[{
      ops
      },
     Join[
      Options@DynamicModule,
      {
       BoxID -> _
       }
      ]
     ]
   ],
  SynchronousInitialization->True
  ]


DynamicWebImageBrowser[img_Dynamic, list : {___String} : {}] :=
 
 DynamicModule[{
   imsize = 350,
   curIm,
   webImageTag = CreateUUID["dynamic-image-"],
   getInnerSerialNo,
   innerSerialNo,
   me
   },
  Framed[
   Pane[
    Column[{
      Panel[
       InputField[img, String, MenuList -> list],
       ImageSize -> {
         Dynamic[
          Replace[imsize, {
            i_?NumericQ :> i,
            {i_?NumericQ, _} :> i,
            {Automatic, v_} :>
             Replace[
              If[! IntegerQ@innerSerialNo,
               getInnerSerialNo[]
               ];
              If[IntegerQ@innerSerialNo,
               ToExpression["FE`img$$" <> ToString@innerSerialNo]
               ], {
               i_?ImageQ :>
                v*Function[#[[1]]/#[[2]]]@ImageDimensions[i],
               Except[_?ImageQ] :> v
               }]
            }]
          ], Automatic}, 
       Appearance -> 
        Lookup[
         FrontEndResource["FEExpressions", 
          "MoreLeftSetterNinePatchAppearance"], "Hover"],
       Alignment -> Center
       ],
      DynamicWebImage[
       img,
       ImageSize -> Dynamic[imsize],
       DynamicEvaluationTimeout -> Infinity,
       BoxID -> webImageTag
       ]
      },
     Spacings -> 0
     ]
    ],
   FrameMargins -> None,
   FrameStyle -> GrayLevel[.85]
   ],
  Initialization :> {
    me = EvaluationBox[],
    getInnerSerialNo =
     Function[
      Replace[
       With[{b = List@NotebookRead[me], t = webImageTag},
        FirstCase[
         FirstCase[b,
          Verbatim[DynamicModuleBox][__, BoxID -> _, ___],
          None,
          \[Infinity]
          ],
         HoldPattern[
           Set[
            _Symbol?(
              Function[Null,
               SymbolName[Unevaluated@#] === "serialno$$",
               HoldAllComplete
               ]),
            sn_Integer
            ]
           ] :> sn,
         $Failed,
         \[Infinity]
         ]
        ],
       i_Integer :> Set[innerSerialNo, i]
       ]
      ]
    }
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
