(* ::Package:: *)

(* ::Section:: *)
(*Make Stylesheet Paclet*)


BeginPackage["StylesheetsShare`"]


StylesheetShare::usage="StylesheetsShare[styles, ops] shares stylesheets to the cloud";
StylesheetInstall::usage="StylesheetInstall[...] is a wrapper to PacletInstall";


(* ::Subsubsection:: *)
(*Package*)


BeginPackage["`Package`"];


createStylesheetsPaclet::usage="";
createStylesheetsDirectory::usage="";
createStylesheetsPacletInfo::usage="";
uploadPacletToCloud::usage="";
gatherStylesheetsFiles::usage="";


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
  Extensions->{
   {\"FrontEnd\", \"Root\"->\".\"}
   }
  ]";


(* ::Subsubsection::Closed:: *)
(*createStylesheetsPacletInfo*)


Options[createStylesheetsPacletInfo]=
  {
    "Version"->"1.0.0",
    "Name"->Automatic,
    "Creator"->Automatic,
    "Description"->"A stylesheets paclet"
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
      "description"->OptionValue["Description"]
      |>,
    "Text"
    ]


(* ::Subsubsection::Closed:: *)
(*createStylesheetsDirectory*)


Options[createStylesheetsDirectory]=
  {
    "BuildDirectory":>CreateDirectory[],
    "MenuName"->"Custom"
    };
createStylesheetsDirectory[files:{__String?FileExistsQ}, ops:OptionsPattern[]]:=
  Module[{root=OptionValue["BuildDirectory"], sub},
    sub=
      CreateDirectory[FileNameJoin@{root, "StyleSheets", OptionValue["MenuName"]}, 
        CreateIntermediateDirectories->True
        ];
    Do[
      CopyFile[f, FileNameJoin@{sub, FileNameTake[f]}],
      {f, files}
      ];
    root
    ]


(* ::Subsubsection:: *)
(*createStylesheetsPaclet*)


createStylesheetsPaclet[
  files:{__String?FileExistsQ}, 
  ops:OptionsPattern[{createStylesheetsDirectory, createStylesheetsPacletInfo}]
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
    PacletManager`PackPaclet[dir]
    ]


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
(*StylesheetShare*)


StylesheetShare//Clear
Options[StylesheetShare]=
  DeleteDuplicatesBy[First]@
    Join[
      Options[createStylesheetsPacletInfo],
      Options[createStylesheetsDirectory],
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
        pac
        },
      name=FileBaseName[stylesheets[[1]]];
      pac = 
        createStylesheetsPaclet[
          stylesheets,
          FilterRules[
            {
              ops,
              "BuildDirectory":>
                CreateDirectory[
                  FileNameJoin@{CreateDirectory[], name},
                  CreateIntermediateDirectories->True
                  ]
              },
            Join[
              Options[createStylesheetsPacletInfo],
              Options[createStylesheetsDirectory]
              ]
            ]
          ];
      uploadPacletToCloud[
        pac,
        FilterRules[
          {ops},
          Join[
            Options[uploadPacletToCloud],
            Options[CloudObject]
            ]
          ]
        ]
      ]/;ListQ[stylesheets]
    ]


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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
          FilterRules[{ops}, Options[PacletManager`PacletInstall]]
          ]
      },
    PacletManager`PacletInstall[
      u,
      FilterRules[{ops}, Options[PacletManager`PacletInstall]]
      ]/;StringQ[u]
    ]


(* ::Subsubsection:: *)
(*End*)


End[];


(* ::Subsection:: *)
(*EndPackage*)


EndPackage[];
