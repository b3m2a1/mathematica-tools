(* ::Package:: *)

(* ::Section:: *)
(*SearchBrowser*)


BeginPackage["SearchBrowser`"];


(*Package Declarations*)
SearchBrowser::usage="SearchBrowser[Dynamic[_, f, ___] | None]
SearchBrowser[Dynamic[v, ___]]";


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


(* ::Section:: *)
(*SearchBrowser*)


SearchBrowser[
   Optional[Verbatim[Dynamic][_, f_, ___] | None, None]
   ] :=
  DynamicModule[
   {
    currentQ = "Qwant",
    searchType,
    searchResults,
    $searchTypes =
     {
      "Web", "Image", "News",
      "SocialMedia", "Music", "Video"
      },
    resultsPerPage = 10,
    resultsPage = 1,
    $so,
    performSearch
    },
   Framed[
      Pane[#,
       ImageSize -> ({#*GoldenRatio, #} &@335),
       AppearanceElements -> {"ResizeArea"}
       ],
      FrameMargins -> None, FrameStyle -> Gray
      ] &@
    Column[{
      Panel[
       Grid@List@{
          Spacer[2],
          Row[{
            Button[Dynamic[searchType],
             Appearance ->
              {
               "Default" ->
                Lookup[
                 
                 FrontEndResource["FEExpressions", 
                  "TabAbuttingLeftRightNinePatchAppearance"
                  ],
                 "Hover"
                 ]
               },
             Enabled -> False,
             FrameMargins -> {{10, 10}, {0, 0}},
             ImageSize -> {Automatic, 26},
             ImageMargins -> 0
             ],
            PopupMenu[
             Dynamic[searchType],
             $searchTypes,
             None,
             Button["",
              Appearance ->
               FrontEndResource["NotebookTemplatingExpressions",
                "ButtonDropdownRightAppearance"
                ]
              ],
             ImageSize -> {Automatic, 24},
             ImageMargins -> 0
             ]
            }],
          EventHandler[
           InputField[Dynamic[currentQ], String],
           {
            "ReturnKeyDown" :>
             CompoundExpression[
              resultsPage = 1;
              performSearch[]
              ],
            Method -> "Queued"
            }],
          Button["",
           resultsPage = 1;
           performSearch[],
           Appearance ->
            Function[{
               "Default" -> #,
                "Hover" ->
                 Image[Darker[#, .5],
                  "Byte",
                  "ColorSpace" -> "RGB",
                  Interleaving -> True],
               "Pressed" ->
                Image[Lighter[#, .5],
                 "Byte",
                  "ColorSpace" -> "RGB",
                  Interleaving -> True
                 ]
                }]@
             ToExpression@
              FrontEndResource["FEBitmaps", "SearchIcon"],
           ImageSize -> Automatic,
           Method -> "Queued"
           ],
          Row[{
            Button["\[LeftGuillemet]",
             resultsPage--;
             performSearch[],
             Appearance ->
              FrontEndResource["FEExpressions",
               "TabAbuttingRightNinePatchAppearance"],
             Enabled -> Dynamic[resultsPage > 1],
             FrameMargins -> {{3, 2}, {2, 2}},
             ImageSize -> {Automatic, 17},
             Method -> "Queued"
             ],
            InputField[
             Dynamic[resultsPage,
              Function[
               Set[resultsPage,
                Max@{IntegerPart[#], 1}
                ];
               performSearch[]
               ],
              SynchronousUpdating -> False
              ],
             Number,
             FieldSize -> 3,
             ImageSize -> {Automatic, 15}
             ],
            Button["\[RightGuillemet]",
             resultsPage++;
             performSearch[],
             Appearance ->
              FrontEndResource["FEExpressions",
               "TabAbuttingLeftNinePatchAppearance"],
             FrameMargins -> {{3, 2}, {2, 2}},
             ImageSize -> {Automatic, 17},
             Method -> "Queued"
             ]
            }],
          InputField[
           Dynamic[resultsPerPage,
            Function[
             Set[resultsPerPage,
              Min@{Max@{IntegerPart[#], 1}, 10}
              ];
             performSearch[]
             ],
            SynchronousUpdating -> False
            ],
           Number,
           FieldSize -> 3,
           ImageSize -> {Automatic, 15}
           ]
          },
       ImageSize -> {Full, Automatic},
       Appearance ->
        Lookup[
         FrontEndResource["FEExpressions", 
          "MoreLeftSetterNinePatchAppearance"],
         "Hover"
         ],
       Alignment -> {Left, Center}
       ],
      Pane[
       Dynamic[
        If[Length@{f} > 1 && ListQ@f,
         Map[
          #[searchResults] &,
          f
          ],
         f[searchResults]
         ];
        Replace[searchResults,
         Except[_Dataset] -> ""
         ]
        ],
       ImageSize -> Full,
       Scrollbars -> Automatic,
       AppearanceElements -> None
       ]
      }],
   Initialization :> (
     If[Length@PacletFind["ServiceConnection_Qwant"] === 0,
      PacletInstall["ServiceConnection_Qwant",
       "Site" ->
        "https://www.wolframcloud.com/objects/b3m2a1.paclets/PacletServer"
       ],
      If[! TrueQ@QwantWebBrowser`Private`$qwantUpdated,
       PacletUpdate["ServiceConnection_Qwant",
        "Site" ->
         "https://www.wolframcloud.com/objects/b3m2a1.paclets/PacletServer"
        ];
       QwantWebBrowser`Private`$qwantUpdated = True;
       QwantWebBrowser`Private`$qwantUpdated // Protect
       ]
      ];
     performSearch =
      Function[
       If[! MatchQ[$so, ServiceObject],
        $so = ServiceConnect["Qwant"]
        ];
       Set[
        searchResults,
        $so[searchType <> "Search",
         "q" -> currentQ,
         "count" -> ToString@resultsPerPage,
         "offset" -> ToString@((resultsPage - 1)*resultsPerPage)
         ]
        ]
       ]
     )
   ];
SearchBrowser[Verbatim[Dynamic][v_, ___?OptionQ]] :=
 
 SearchBrowser[Dynamic[v, Set[v, #] &]]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
