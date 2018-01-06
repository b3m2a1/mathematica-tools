(* ::Package:: *)

(*MakeIndentable["IndentCharacter"\[Rule]"  "]*)


(* ::Section:: *)
(*GitBookBuilder*)


BeginPackage["GitBookBuilder`"];


(*Package Declarations*)
GitBookBuild::usage="GitBookBuild[srcDir, bookDir]";


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
        MatchQ[FileExtension[#], "png" | "jpeg"],
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
       CellGroupData[Flatten@{
          Cell[#, "Section"],
          KeyValueMap[
           Cell[
             CellGroupData[
              Flatten@{
                Cell[#, "Subsection"],
                Map[
                 Cell[
                   TextData[
                    ButtonBox[#Title,
                     BaseStyle -> "Hyperlink",
                     ButtonData ->
                      {
                       FrontEnd`FileName[
                       Evaluate@URLParse[#Path, "Path"]], 
                       None
                       }
                      ]
                    ],
                   "Item"
                   ] &,
                 #2
                  ]
                }
              ]
             ] &,
           #2
           ]
          }
        ]
       ] &,
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


(* ::Subsubsection::Closed:: *)
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


GitBookBuild[srcDir : (_String | _File )?DirectoryQ, bookDir_] :=
  Catch@
   Module[
    {postDir, content},
    gitBookValidateDirectory[srcDir];
    If[!DirectoryQ@bookDir,
      CreateDirectory[bookDir, CreateIntermediateDirectories->True];
      ToExpression["BTools`Git"]["Init", bookDir];
      ToExpression["BTools`Git"]["AddGitIgnore", bookDir];
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


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
