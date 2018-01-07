(* ::Package:: *)

(*MakeIndentable["IndentCharacter"\[Rule]"  "]*)


(* ::Section:: *)
(*GitBookBuilder*)


BeginPackage["GitBookBuilder`"];


ClearAll["`*`", "`*`*"];


(*Package Declarations*)
GitBookBuild::usage="GitBookBuild[srcDir, bookDir]";
GitBookPush::usage="GitBookPush[bookDir]
GitBookPush[bookDir, remote]";


(* ::Subsubsection::Closed:: *)
(*Private Declarations*)


AppendTo[$ContextPath,$Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
gitBookCopyContent::usage="gitBookCopyContent[srcDir, bookDir]";
gitBookExportMD::usage="gitBookExportMD[postDir, bookDir, content]";
gitBookMakeSummary::usage="gitBookMakeSummary[postDir, bookDir, metas]";
gitBookValidateDirectory::usage="gitBookValidateDirectory[dir]";


End[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


(* ::Subsubsection::Closed:: *)
(*Errors*)


GitBookBuild::baddir = "Source directory `` is lacking content ``";
GitBookBuild::badmat = "Source file `` does not contain metadata";


(* ::Subsubsection:: *)
(*gitBookCopyContent*)


(* ::Text:: *)
(*Copies base content*)


gitBookCopyContent[srcDir_, bookDir_] :=
  With[{
    main = FileNameJoin@{srcDir, "content"}
    },
   With[{aboutSplit =
      StringSplit[
       Import[FileNameJoin@{main, "pages", "About.md"}, "Text"],
       "\n\n",
       2
       ]
     },
    If[Length[aboutSplit] != 2,
     Message[GitBookBuild::badmat, 
      FileNameJoin@{main, "pages", "About.md"}];
     Throw[$Failed]
     ];
    Export[
     FileNameJoin@{bookDir, "README.md"},
     aboutSplit[[2]],
     "Text",
     CharacterEncoding -> "UTF8"
     ]
    ];
   Map[
    If[! MatchQ[FileNameTake[#], "pages" | "posts" | ".git"],
      With[{f = FileNameJoin@{bookDir, StringTrim[#, main]}},
       Which[DirectoryQ@#,
        If[FileExistsQ@f,
         DeleteDirectory[f, DeleteContents -> True]
         ];
        CopyDirectory[#, f],
        MatchQ[FileExtension[#], "png" | "jpeg" | "md" | "css" | "html" | "js" ],
        CopyFile[#, f, OverwriteTarget -> True]
        ]
       ]
      ] &,
    FileNames["*", main]
    ]
   ];


(* ::Subsubsection:: *)
(*gitBookExportMD*)


(* ::Text:: *)
(*Exports markdown and cleans out possible template conflicts*)


gitBookExportMD[postDir_, bookDir_, content_] :=
  KeyValueMap[
   Function[
    With[{f =
       FileNameJoin@Flatten@
         {
          bookDir,
          FileNameSplit@
           StringTrim[#, postDir]
          }
      },
     If[! DirectoryQ@DirectoryName@f,
      CreateDirectory[DirectoryName@f, 
       CreateIntermediateDirectories -> True]
      ];
     Export[
      f,
      StringJoin[
       StringReplace[#2,
         { 
          "{{" -> "{ {",
           "}}" -> "} }",
          "{filename}" ->
           URLBuild@
            ConstantArray["..", 
             FileNameDepth[StringTrim[#, postDir]] - 2
             ]
          }
         ] /.
        StringExpression[a___] :> a
       ],
      "Text",
      CharacterEncoding -> "UTF8"
      ]
     ]
    ],
   content
   ];


(* ::Subsubsection:: *)
(*gitBookMakeSummary*)


(* ::Text:: *)
(*Builds the summary index from the IDs*)


gitBookHyperlink[assoc_]:=
  ButtonBox[assoc["Title"],
   BaseStyle -> "Hyperlink",
   ButtonData ->
    {
     FrontEnd`FileName[
       Evaluate@URLParse[assoc["Path"], "Path"]
       ], 
     None
     }
   ];


gitBookSummaryLinkSection[title_, data_,
  parentStyle_
  ]:=
  If[title=!="",
    Cell@*CellGroupData,
    Identity
    ]@
      With[{t=If[title=="", CreateUUID[], title]},
        With[{headerPage=Select[data, #Title==t&]},
          Flatten@{
            If[title=="",
              Nothing,
              If[Length@headerPage>0,
                Cell[
                  TextData[gitBookHyperlink[headerPage[[1]]]], 
                  If[parentStyle=="Subsection",
                    "Item",
                    "Subitem"
                    ]
                  ],
                Cell[title, 
                  If[parentStyle=="Subsection",
                    "Item",
                    "Subitem"
                    ]
                  ]
                ]
              ],
          Map[
           Cell[
             TextData[gitBookHyperlink[#]],
             If[parentStyle=="Subsection",
               "Subsubitem", 
               "Subitem"
               ]
             ] &,
           If[title=!="", Select[data, #Title=!=title&], data]
           ]
         }
       ]
     ]


gitBookMakeSummary[postDir_,  bookDir_, metas_] :=
  Module[{data,  cells, nb, postCounter=1},
   data =
    GroupBy[First -> Last] /@
     GroupBy[
       If[Length@#[[1]]>0, #[[1, 1]], ""] & -> 
         (If[Length@#[[1]]>1, #[[1, 2]], ""] -> #[[2]] &)
         ]@
      KeyValueMap[
       If[KeyMemberQ[#2, "Path"], 
						Take[URLParse[#2["Path"], "Path"], UpTo[2]],
						{}
						] ->
         <|
          "Path" ->
            URLBuild[FileNameSplit@StringTrim[#, postDir]],
          "Title" -> 
            If[KeyMemberQ[#2, "Title"], 
              #2["Title"],
              "Post #"<>ToString[postCounter++]
              ]
          |> &,
       metas
       ];
   cells =
    KeyValueMap[
     Cell[
       CellGroupData[
         With[{main=#, noPath=Lookup[#2, "", <||>]},
           With[{titlePage=Select[noPath, #Title==main&]},
             Flatten@{
              If[Length@titlePage>0, 
                Cell[
                  TextData[gitBookHyperlink[titlePage[[1]]]],
                  "Item"
                  ],
                Cell[main, "Subsection"]
                ],
              gitBookSummaryLinkSection["", Select[noPath, #Title!=main&],
                If[Length@titlePage>0, "Item", "Subsection"]
                ],
              KeyValueMap[
               gitBookSummaryLinkSection[##, 
                 If[Length@titlePage>0, "Item", "Subsection"]
                 ]&,
               KeyDrop[#2, ""]
               ]
             }
           ]
         ]
        ]
      ]&,
     data
     ];
   CheckAbort[
    nb =
     CreateDocument[
      Prepend[
       cells,
       Cell[
        TextData[
         ButtonBox[
          "Introduction",
          BaseStyle -> "Hyperlink",
          ButtonData ->
           {
            "README.md", 
            None
            }
          ]
         ],
        "Item"
        ]
       ],
      Visible -> False,
      StyleDefinitions -> 
       If[Length@PacletManager`PacletFind["BTools"]>0,
        FrontEnd`FileName[{"BTools"}, "MarkdownNotebook.nb"],
        FrontEnd`FileName[{"SiteBuilder"}, "MarkdownNotebook.nb"]
        ],
      NotebookFileName ->
       FileNameJoin@{bookDir, "SUMMARY.nb"}
      ];
    If[Length@PacletManager`PacletFind["BTools"]>0,
       ToExpression["BTools`NotebookMarkdownSave"][nb],
       ToExpression["SiteBuilder`NotebookMarkdownSave"][nb]
       ];
    NotebookClose[nb];
    DeleteFile[FileNameJoin@{bookDir, "SUMMARY.nb"}];,
    NotebookClose[nb]
    ];
   ];


(* ::Subsubsection:: *)
(*gitBookValidateDirectory*)


(* ::Text:: *)
(*Validates a source directory*)


gitBookValidateDirectory[dir_] :=
  CompoundExpression[
   If[! DirectoryQ@FileNameJoin@{dir, "content", "posts"},
    Message[GitBookBuild::baddir, dir, 
     FileNameJoin@{"content", "posts"}];
    Throw[$Failed]
    ],
   If[!FileExistsQ@
      FileNameJoin@{dir, "content", "pages", "About.md"},
    Message[GitBookBuild::baddir, dir, 
     FileNameJoin@{"content", "pages", "About.md"}];
    Throw[$Failed]
    ];
   True
   ];


(* ::Subsubsection:: *)
(*GitBookBuild*)


(* ::Text:: *)
(*Builds a GitBook from srcDir in bookDir*)


GitBookBuild[srcDir:_String?DirectoryQ, bookDir_] :=
  Catch@
   Module[
    {postDir, content},
    gitBookValidateDirectory[srcDir];
    If[!DirectoryQ@bookDir,
      CreateDirectory[bookDir, CreateIntermediateDirectories->True];
      ];
    gitBookCopyContent[srcDir, bookDir];
    postDir =
     FileNameJoin@{srcDir, "content", "posts"};
    content =
     AssociationMap[
      With[{
         split = StringSplit[Import[#, "Text"], "\n\n", 2]
         },
        If[Length[split] != 2,
         Message[GitBookBuild::badmat, #];
         Throw[$Failed]
         ];
        <|
         "Meta" ->
          Association@
           Map[
            Rule @@ StringTrim@StringSplit[#, ":", 2] &,
            StringSplit[split[[1]], "\n"]
            ],
         "Body" ->
          split[[2]]
         |>
        ] &,
      FileNames["*.md", postDir, \[Infinity]]
      ];
    gitBookExportMD[postDir, bookDir, content[[All, "Body"]] ];
    gitBookMakeSummary[postDir, bookDir, 
     SortBy[
      content[[All, "Meta"]],
      ToExpression[StringSplit[#ID, "."]] &
      ]
     ];
    bookDir
    ];


(* ::Subsubsection:: *)
(*GitBookPush*)


GitBookPush::noremote="No remote passed and no remote already set for book ``";
Options[GitBookPush]=
  {
    "CreateGitHubRepo"->True,
    "MessageTemplate"->Automatic,
    Quiet->True
    }
GitBookPush[
  bookDir:_String?DirectoryQ, 
  remote:_String|Automatic:Automatic,
  ops:OptionsPattern[]
  ]:=
  Catch@If[TrueQ@OptionValue[Quiet], Quiet, Identity]@Module[{
    git=ToExpression["BTools`Git"], 
    gitRepoQ=ToExpression["BTools`GitRepoQ"],
    gitHub=ToExpression["BTools`GitHub"],
    initted=False,
    mTemp1=
      Replace[OptionValue["MessageTemplate"], Automatic:>"First build for GitBook"],
    mTemp=
      Replace[OptionValue["MessageTemplate"], Automatic:>"Built GitBook @ `Time`"],
    tempPars=
      <|
        "Time"->Now,
        "Repo"->bookDir,
        "RepoName"->FileBaseName[bookDir]
        |>
    },
    If[!gitRepoQ@bookDir,
      initted=True;
      git["Init", bookDir];
      git["AddGitIgnore", bookDir];
      git["Add", bookDir,"-A"];
      git["Commit", bookDir, 
        Message->TemplateApply[mTemp1, tempPars]
        ];,
      git["Add", bookDir, "-A"];
      git["Commit", bookDir, 
        Message->TemplateApply[mTemp, tempPars]
        ]
      ];
    If[!StringQ@git["ListRemotes", bookDir],
      If[!StringQ@remote, Message[GitBookPush::noremote, bookDir];Throw[$Failed]];
      git["AddRemote", bookDir, remote];
      If[OptionValue["CreateGitHubRepo"],
        gitHub["Create", URLParse[remote, "Path"][[-1]]]
        ]
      ];
    git["PushOrigin", bookDir];
    ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
