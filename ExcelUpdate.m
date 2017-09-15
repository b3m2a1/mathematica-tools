(* ::Package:: *)

(* ::Section:: *)
(*ExcelUpdate*)


BeginPackage["ExcelUpdate`"];


(*Package Declarations*)
ExcelUpdate::usage="
ExcelUpdate[file, vals] updates the vals in file
ExcelUpdate[file, vals, True] overwrites file with vals
ExcelUpdate[file, worksheet ...] operates on the sheet worksheet
";


(* ::Subsubsection::Closed:: *)
(*Private Declarations*)


AppendTo[$ContextPath,$Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
excelExtract::usage="excelExtract[file]";
excelExtractedWorksheets::usage="excelExtractedWorksheets[dir]
excelExtractedWorksheets[file]";
excelValuesExtract::usage="excelValuesExtract[xml]";
excelValuesFormat::usage="excelValuesFormat[rules]";
excelValueAssociationFormat::usage="excelValueAssociationFormat[v]";
excelValuesMerge::usage="excelValuesMerge[rules]";
excelValuesUpdate::usage="excelValuesUpdate[file, vals, overwrite]
excelValuesUpdate[xml, vals, overwrite]
excelValuesUpdate[dir, ws, vals, overwrite]
excelValuesUpdate[dir, ws, vals, overwrite]
excelValuesUpdate[file, ws, vals, overwrite]";
excelCompress::usage="excelCompress[file, path]
excelCompress[file]";
excelUpdate::usage="excelUpdate[file, worksheet, vals, overwrite]";


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


NotebookToPackage@EvaluationNotebook[] // CreateDocument


(* ::Section:: *)
(*ExcelUpdate*)


(* ::Subsubsection::Closed:: *)
(*excelExtract*)


Clear[excelExtract];
excelExtract[file_] :=
 
 With[{eDir = 
    FileNameJoin@{$TemporaryDirectory, "_excel_extractor", 
      FileBaseName[file]}},
  Quiet@
   DeleteDirectory[
    eDir,
    DeleteContents -> True
    ];
  CreateDirectory[eDir,
   CreateIntermediateDirectories -> True
   ];
  CopyFile[file,
   FileNameJoin@{DirectoryName@eDir, FileBaseName[file] <> ".zip"},
   OverwriteTarget -> True
   ];
  ExtractArchive[
   FileNameJoin@{DirectoryName@eDir, FileBaseName[file] <> ".zip"},
   eDir
   ];
  DeleteFile[
   FileNameJoin@{DirectoryName@eDir, FileBaseName[file] <> ".zip"}];
  eDir
  ]


(* ::Subsubsection::Closed:: *)
(*excelExtractedWorksheets*)


Clear[excelExtractedWorksheets];
excelExtractedWorksheets[dir_String?DirectoryQ] :=
  
  FileNames["*.xml", FileNameJoin@{dir, "xl", "worksheets"}];
excelExtractedWorksheets[file_] :=
  
  With[{eDir = 
     FileNameJoin@{$TemporaryDirectory, "_excel_extractor", 
       FileBaseName[file]}},
   If[! DirectoryQ[eDir], excelExtract[file]];
   excelExtractedWorksheets[eDir]
   ];


(* ::Subsubsection::Closed:: *)
(*excelValuesExtract*)


Clear[excelValuesExtract];
excelValuesExtract[xml_] :=
  Cases[xml,
   XMLElement["c", {a___, "r" -> id_, b___}, v_] :>
    (id ->
      <|
       "CellValue" ->
        Replace[v,
         {
          {XMLElement["v", _, {s_String}]} :> ToExpression@s,
          _ -> Nothing
          }],
       "CellMeta" -> {a, b}
       |>),
   \[Infinity]
   ];


(* ::Subsubsection::Closed:: *)
(*excelValuesFormat*)


Clear[excelValuesFormat];
excelValuesFormat[rules_] :=
  With[{coreData =
     GroupBy[
      With[{
          v = Lookup[#[[2]], "CellValue", Null],
          m = Lookup[#[[2]], "CellMeta", {}]
          },
         #[[1]] ->
          XMLElement["c",
           {"r" -> #[[1]], Sequence @@ m},
           {
            Replace[v, {
              Null -> Nothing,
              _ :>
               XMLElement["v", {},
                {ToString[v, InputForm]}
                ]
              }]}
           ]
         ] & /@ rules,
      (StringTrim[#[[1]], LetterCharacter ..] &) -> Last
      ]
    },
   XMLElement["sheetData", {},
    KeyValueMap[
     With[{
        keys =
         FromDigits[Flatten[{LetterNumber[#]}, 1], 26] & /@
          
          StringTrim[
           Lookup[List @@ #2[[All, 2]],
            "r"
            ],
           DigitCharacter ..
           ]
        },
       XMLElement["row",
        Flatten@
         {"r" -> ToString@#, "spans" ->
           StringRiffle[
            ToString /@
             MinMax[keys],
            ":"
            ]
          },
        #2[[Ordering[keys]]]
        ]
       ] &,
     coreData
     ]
    ]
   ];


(* ::Subsubsection::Closed:: *)
(*excelValuesMerge*)


excelValueAssociationFormat[v_] :=
  
  If[AssociationQ[v] && AnyTrue[{"CellValue"}, KeyMemberQ[v, #] &],
   v,
   <|
    "CellValue" -> v
    |>
   ];


Clear[excelValuesMerge];
excelValuesMerge[rules_] :=
 Merge[rules,
  <|
     
     "CellValue" -> 
      Replace[Lookup[#, "CellValue", 
        Nothing], {{___, l_} :> l, {} -> Null}],
     "CellMeta" -> 
      DeleteDuplicates@Apply[Join, Lookup[#, "CellMeta", {}]]
     |> &@*Map[excelValueAssociationFormat]
  ]


(* ::Subsubsection::Closed:: *)
(*excelValuesUpdate*)


Clear[excelValuesUpdate];
excelValuesUpdate[
   file_String?(FileExistsQ[#] && FileExtension[#] === "xml" &), 
   vals : _List | _Rule, overwrite : True | False : False] :=
  
  With[{xml = Import[file]},
   Export[file,
    excelValuesUpdate[xml, vals, overwrite],
    "XML",
    "ElementFormatting" -> False
    ]
   ];
excelValuesUpdate[
   xml : XMLObject[dec___][spec_, els_, r___],
   vals_,
   overwrite : True | False : False
   ] :=
  With[{vs =
     excelValuesFormat@
      Normal@
       excelValuesMerge[
        If[overwrite,
         vals,
         {excelValuesExtract[els], vals}
         ]
        ]
    },
   XMLObject[dec][spec,
    els /.
     XMLElement["sheetData", __] :>
      vs,
    r
    ]
   ];
excelValuesUpdate[dir_String?DirectoryQ, ws_String,
   vals_,
   overwrite : True | False : False
   ] :=
  
  With[{f = 
     FileNameJoin@{dir, "xl", "worksheets", 
       StringTrim[ws, ".xml"] <> ".xml"}},
   If[FileExistsQ[f],
    excelValuesUpdate[f, vals, overwrite],
    $Failed
    ]
   ];
excelValuesUpdate[dir_String?DirectoryQ, ws : _Integer : 1,
   vals_,
   overwrite : True | False : False
   ] :=
  
  With[{f = 
     FileNames["*.xml", FileNameJoin@{dir, "xl", "worksheets"}]},
   If[Length[f] >= ws,
    excelValuesUpdate[f[[ws]],
     vals,
     overwrite
     ],
    $Failed
    ]
   ];
excelValuesUpdate[file_, ws : _String | _Integer : 1,
  vals_,
  overwrite : True | False : False
  ] :=
 With[
  {eDir = 
    FileNameJoin@{$TemporaryDirectory, "_excel_extractor", 
      FileBaseName[file]}},
  If[! DirectoryQ[eDir], excelExtract[file]];
  excelValuesUpdate[eDir, ws, vals, overwrite]
  ]


(* ::Subsubsection::Closed:: *)
(*excelCompress*)


Clear[excelCompress];
excelCompress[file_, path_] :=
  
  With[{eDir = 
     FileNameJoin@{$TemporaryDirectory, "_excel_extractor", 
       FileBaseName[path]}},
   If[DirectoryQ[eDir],
    Quiet@DeleteFile[file];
    Switch[$OperatingSystem,
     "MacOSX",
     RunProcess[{"zip", "-r", ExpandFileName@file,
       Sequence @@
        Map[
         FileNameDrop[#, FileNameDepth[eDir]] &,
         FileNames["*", eDir, \[Infinity]]
         ]},
      ProcessDirectory -> eDir
      ],
     _,
     RunProcess[{"zip", "-r", ExpandFileName@file,
       Sequence @@
        Map[
         FileNameDrop[#, FileNameDepth[eDir]] &,
         FileNames["*", eDir, \[Infinity]]
         ]},
      ProcessDirectory -> eDir
      ]
     ];
    DeleteDirectory[eDir, DeleteContents -> True];
    file,
    $Failed
    ]
   ];
excelCompress[file_] :=
 excelCompress[file, file]


(* ::Subsubsection::Closed:: *)
(*excelUpdate*)


excelUpdate[file_, worksheet : _String | _Integer : 1,
  vals_, overwrite : True | False : False] :=
 (
  excelExtract[file];
  excelValuesUpdate[file,
   worksheet,
   vals,
   overwrite
   ];
  excelCompress[file]
  )


(* ::Subsubsection:: *)
(*ExcelUpdate*)


ExcelUpdate[file_, worksheet : _String | _Integer : 1,
  vals_, overwrite : True | False : False] :=
 
 excelUpdate[file, worksheet, vals, overwrite]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
