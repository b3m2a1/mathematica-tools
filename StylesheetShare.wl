(* ::Package:: *)

(* ::Section:: *)
(*Make Stylesheet Paclet*)


BeginPackage["StylesheetShare`"]


StylesheetShare::usage="StylesheetsShare[styles, ops] shares stylesheets to the cloud";
StylesheetInstall::usage="StylesheetInstall[...] is a wrapper to PacletInstall";


CreateStylesheetPreview::usage="CreateStylesheetPreview[...] creates a preview notebook";


(* ::Subsubsection:: *)
(*Package*)


BeginPackage["`Package`"];


createStylesheetsPaclet::usage="";
createStylesheetsDirectory::usage="";
createStylesheetsPacletInfo::usage="";
uploadPacletToCloud::usage="";
gatherStylesheetsFiles::usage="";
createPreviewNotebookTemplate::usage="";
createRasterNotebookExpr::usage="";
createPreviewNotebook::usage="";


EndPackage[];


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*PITemplate*)


PITemplate="Paclet[
  Name->\"`name`\",
  Version->\"`version`\",
  Creator->\"`creator`\",
  Description->\"`description`\",
  Extensions->{`exts`}
  ]";


PIExtTemplate="{\"`name`\", `opts`}"


createPIExt[name_, opts_]:=
  TemplateApply[
    PIExtTemplate,
    <|
      "name"->name,
      "opts"->StringRiffle[Map[ToString[#, InputForm]&, Normal[opts]], ", "]
      |>
    ]


(* ::Subsubsection:: *)
(*createStylesheetsPacletInfo*)


Options[createStylesheetsPacletInfo]=
  {
    "Version"->"1.0.0",
    "Name"->Automatic,
    "Creator"->Automatic,
    "Description"->"A stylesheets paclet",
    "PackageContext"->None,
    "PackageRoot"->None,
    "Extensions"->{},
    "FrontEndRoot"->Automatic
    };
createStylesheetsPacletInfo[
  dir_,
  ops:OptionsPattern[]
  ]:=
  Export[
    FileNameJoin@{dir, "PacletInfo.m"},
    PITemplate~TemplateApply~<|
      "name"->Replace[OptionValue["Name"], Automatic->FileBaseName[dir]],
      "version"->OptionValue["Version"],
      "creator"->Replace[OptionValue["Creator"], Automatic:>$WolframID],
      "description"->OptionValue["Description"],
      "exts"->
        StringRiffle[
          KeyValueMap[
            createPIExt,
            Association[
              Join[
                {
                  "FrontEnd"->{
                    "Root"->
                      Replace[OptionValue["FrontEndRoot"], 
                        {
                          Nothing->".",
                          Except[_String]->"FrontEnd"
                          }
                        ]
                    },
                  Replace[OptionValue["PackageContext"],
                    {
                      s:_String|{__String}:>
                        "Kernel"->{
                          "Root"->
                            Replace[
                              OptionValue["PackageRoot"], 
                              Except[_String]->"Include"
                              ],
                           "Context"->Flatten[{s}]
                           },
                       _->Nothing
                       }
                    ]
                  },
                Replace[
                  Normal@OptionValue["Extensions"],
                  l:{{_String, ___?OptionsPattern}..}:>
                    Map[#[[1]]->Rest[#]&, l]
                  ]
                ]
              ]
            ],
          ", "
          ]
      |>,
    "Text"
    ]


(* ::Subsubsection::Closed:: *)
(*createStylesheetsDirectory*)


Options[createStylesheetsDirectory]=
  {
    "BuildDirectory"->Automatic,
    "FrontEndRoot"->Automatic,
    "MenuName"->"Custom"
    };
createStylesheetsDirectory[files:{__String?FileExistsQ}, ops:OptionsPattern[]]:=
  Module[{root=OptionValue["BuildDirectory"], sub},
    sub=
      CreateDirectory[
        FileNameJoin@{
          root, 
          Replace[OptionValue["FrontEndRoot"], 
            Except[_String|Nothing]->"FrontEnd"
            ],
          "StyleSheets", 
          OptionValue["MenuName"]
          }, 
        CreateIntermediateDirectories->True
        ];
    Do[
      CopyFile[f, FileNameJoin@{sub, FileNameTake[f]}],
      {f, files}
      ];
    root
    ]


(* ::Subsubsection::Closed:: *)
(*addPacletStuff*)


addPacletStuff[dir_, adds_, root:Nothing|_String:"Include"]:=
  Module[{source, target},
    Do[
      {source, target} = 
        Replace[a, 
          {
            d_String:>
              {d, FileNameJoin@{dir, root, FileNameTake[d]}},
            (d_String->p_String):>
              {d, FileNameJoin@{dir, p}}
            }
          ];
      If[!DirectoryQ@DirectoryName[target],
        CreateDirectory[DirectoryName[target], CreateIntermediateDirectories->True]
        ];
      If[DirectoryQ@source,
        CopyDirectory[source, target],
        CopyFile[source, target]
        ],
      {a, adds}
      ]
   ]


(* ::Subsubsection::Closed:: *)
(*createStylesheetsPaclet*)


Options[createStylesheetsPaclet]=
  Join[
    Options[createStylesheetsDirectory],
    Options[createStylesheetsPacletInfo],
    {
      "IncludesRoot"->"Include",
      "Includes"->{}
      }
    ];
createStylesheetsPaclet[
  files:{__String?FileExistsQ}, 
  ops:OptionsPattern[]
  ]:=
  Module[
    {
      dir,
      pac
      },
    dir = 
      createStylesheetsDirectory[files, 
        FilterRules[{ops}, Options[createStylesheetsDirectory]]
        ];
    createStylesheetsPacletInfo[dir, 
      FilterRules[{ops}, Options[createStylesheetsPacletInfo]]
      ];
    addPacletDirectories[dir, 
      OptionValue["Includes"],
      OptionValue["IncludesRoot"]
      ];
    PacletManager`PackPaclet[dir]
    ]


(* ::Subsubsection::Closed:: *)
(*uploadPacletToCloud*)


Options[uploadPacletToCloud]=
  Join[
    {
      "PacletsRoot"->"",
      Permissions->"Public"
      }
    ];
uploadPacletToCloud[pac_, 
  ops:OptionsPattern[{uploadPacletToCloud, CloudObject}]
  ]:=
  Module[
    {
      co
      },
    co = 
      CloudObject[
        URLBuild[
          <|
            "Path"->{
              OptionValue["PacletsRoot"], 
              "Paclets",
              FileNameTake[pac]
              }
            |>
          ],
        Permissions->OptionValue[Permissions],
        ops
        ];
    CopyFile[pac, co]
    ]


(* ::Subsubsection::Closed:: *)
(*gatherStylesheetsFiles*)


gatherStylesheetsFiles[dir_String?DirectoryQ]:=
  FileNames["*.nb", dir];
gatherStylesheetsFiles[file_String?FileExistsQ]:=
  If[FileExtension[file]==="nb", 
    {file},
    Nothing
    ];
gatherStylesheetsFiles[list_List]:=
  Flatten[gatherStylesheetsFiles/@list]


(* ::Subsubsection::Closed:: *)
(*StylesheetShare*)


StylesheetShare//Clear
Options[StylesheetShare]=
  DeleteDuplicatesBy[First]@
    Join[
      Options[createStylesheetsPaclet],
      Options[uploadPacletToCloud],
      Options[CloudObject]
      ];
StylesheetShare[files_, ops:OptionsPattern[]]:=
  With[
    {
      stylesheets=gatherStylesheetsFiles[files]
      },
    Module[
      {
        name,
        pac,
        bd = OptionValue["BuildDirectory"],
        co
        },
      name=FileBaseName[stylesheets[[1]]];
      If[bd===Automatic,
        bd = CreateDirectory[
              FileNameJoin@{CreateDirectory[], name},
              CreateIntermediateDirectories->True
              ]
        ];
      pac = 
        createStylesheetsPaclet[
          stylesheets,
          FilterRules[
            {
              "BuildDirectory" -> bd,
              ops
              },
            Options[createStylesheetsPaclet]
            ]
          ];
      If[OptionValue["BuildDirectory"]===Automatic,
        DeleteDirectory[bd, DeleteContents->True]
        ];
      co = 
        uploadPacletToCloud[
          pac,
          FilterRules[
            {ops},
            Join[
              Options[uploadPacletToCloud],
              Options[CloudObject]
              ]
            ]
          ];
       DeleteFile[pac];
       co
      ]/;ListQ[stylesheets]
    ]


(* ::Subsubsection::Closed:: *)
(*getStylesheetPacletURL*)


Options[getStylesheetPacletURL]=
  {
    "Version"->"1.0.0",
    "WolframID"->Automatic,
    "PacletsRoot"->Nothing,
    CloudBase->Automatic
    };
getStylesheetPacletURL[sheetName_, ops:OptionsPattern[]]:=
  URLBuild[
      {
        Replace[OptionValue[CloudBase], Automatic:>$CloudBase],
        "obj", 
        First@StringSplit[
          Replace[
            OptionValue["WolframID"],
            Automatic:>$WolframID
            ],
          "@"
          ],
        OptionValue["PacletsRoot"],
        "Paclets",
        sheetName<>"-"<>OptionValue["Version"]<>".paclet"
        }
      ]


(* ::Subsubsection::Closed:: *)
(*StylesheetInstall*)


StylesheetInstall//Clear
Options[StylesheetInstall]=
  Join[
    Options[getStylesheetPacletURL],
    Options[PacletManager`PacletInstall]
    ];
StylesheetInstall[sheetName_String, ops:OptionsPattern[]]:=
  With[
    {
      u=
        getStylesheetPacletURL[sheetName, 
          FilterRules[{ops}, Options[getStylesheetPacletURL]]
          ]
      },
    PacletManager`PacletInstall[
      u,
      FilterRules[{ops}, Options[PacletManager`PacletInstall]]
      ]/;StringQ[u]
    ]


(* ::Subsubsection::Closed:: *)
(*createPreviewNotebookTemplate*)


(* ::Input:: *)
(*(*$textStyles=*)
(*Cases[Except["Code"|"Input",_String]]@*)
(*Values@FE`Evaluate[FEPrivate`GetPopupList["MenuListStyles"]]*)*)


Options[createPreviewNotebookTemplate]=
  {
    "Styles"->{
        "Title","Subtitle","Chapter",
        "Section","Subsection","Subsubsection",
        "Text", "Code", "Input", "Output",
        "Item","ItemNumbered","ItemParagraph",
        "Subitem","SubitemNumbered", "SubitemParagraph",
        "InlineFormula", "DisplayFormula", "Program"
      },
    "StyleContentMap"->{
      "Code"->BoxData, 
      "Input"->BoxData, 
      "Output"->BoxData,
      _->Identity
      }
    };
createPreviewNotebookTemplate[ops:OptionsPattern[]]:=
  With[{map=OptionValue["StyleContentMap"]},
    Notebook[
      Map[Cell[(#/.map)[#], #]&, OptionValue["Styles"]]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*createRasterNotebookExpr*)


createRasterNotebookExpr[s_, template_]:=
  Join[
    template,
    Notebook[
      WindowSize->{808,755},
      StyleDefinitions->s
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*createPreviewNotebook*)


createPreviewNotebook[sheets_, template_]:=
  Module[{nb, img},
    Notebook[
      List@Cell[BoxData@ToBoxes@
        Pane[
          Grid[
            Partition[
              Table[
                With[
                  {
                    rast=createRasterNotebookExpr[Replace[s, (a_->b_):>a], template],
                    tmp=createRasterNotebookExpr[Replace[s, (a_->b_):>b], template]
                    },
                  Internal`WithLocalSettings[
                    nb=CreateDocument[rast, Visible->False],
                    img=Rasterize[nb, 
                        Background->CurrentValue[nb, Background], 
                        ImageResolution->72
                        ];
                    Button[
                      "",
                      CreateDocument[tmp];
                      NotebookClose[ButtonNotebook[]],
                      Appearance->img,
                      ImageSize->{808, 755}/4,
                      FrameMargins->5
                      ],
                    NotebookClose[nb];
                    ]
                  ],
                {s, sheets}
                ],
              UpTo[2]
              ]
            ],
          ImageSize->{808/2, Automatic},
          ImageMargins->25,
          Alignment->Center
          ],
        CellMargins->{{0, 0}, {0, 0}}
        ],
      WindowTitle->"Stylesheet Preview",
      WindowSize->{All, 755/2},
      ScrollingOptions->{"VerticalScrollRange"->Fit},
      Background->Gray,
      WindowMargins->{Automatic, Automatic},
      StyleDefinitions->"Palette.nb"
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*CreateStylesheetPreview*)


Options[CreateStylesheetPreview]=
  Options[createPreviewNotebookTemplate];
CreateStylesheetPreview[
  sheets_,
  ops:OptionsPattern[]
  ]:=
  createPreviewNotebook[
    Flatten@{sheets},
    createPreviewNotebookTemplate[ops]
    ]


(* ::Subsubsection:: *)
(*End*)


End[];


(* ::Subsection:: *)
(*EndPackage*)


EndPackage[];
