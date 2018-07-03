(* ::Package:: *)

(* ::Section:: *)
(*$Name Loader*)


BeginPackage["$Name`"]


$Name::usage="A head for the package (useful for messages)";


(* ::Subsection:: *)
(*Package Level Symbols*)


BeginPackage["`PackagePrivate`"]


$PackageName::usage="The name of the package";
$PackageDirectory::usage="The directory for the package";


$PackageListing::usage="The listing of packages";
$PackageContexts::usage="The list of contexts exposed to all packages";
$PackageDeclared::usage="Whether the package has been auto-loaded or not";


$PackageFileContexts::usage="The contexts for files in the package";
$DeclaredPackages::usage="The set of packages found and declared via the autoloader";
$LoadedPackages::usage="The set of loaded packages";


PackageExecute::usage="Executes something with the package contexts exposed";
PackageLoadPackage::usage="Loads a package via PackageExecute";
PackageLoadDeclare::usage="Declares a package";


PackageAppLoad::usage="Loads the entire package";
PackageAppGet::usage="";


(* ::Subsubsection::Closed:: *)
(*Begin*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*Constants*)


$PackageName="$Name";
$Name["Directory"]:=
  $PackageDirectory;
$PackageDirectory=
  DirectoryName@$InputFileName;


$PackagePackagesDirectory="Packages"


$Name["PackageListing"]:=$PackageListing;
$PackageListing=<||>;
$Name["Contexts"]:=$PackageContexts;
If[!ListQ@$PackageContexts,
  $PackageContexts=
    {
      "$Name`",
      "$Name`PackagePrivate`"
      }
  ];
$PackageDeclared=TrueQ[$PackageDeclared];


$Name["FileContexts"]:=$PackageFileContexts;
If[Not@AssociationQ@$PackageFileContexts,
  $PackageFileContexts=
    <||>
  ];
$Name["DeclaredPackages"]:=$DeclaredPackages;
If[Not@AssociationQ@$DeclaredPackages,
  $DeclaredPackages=
    <||>
  ];
$Name["LoadedPackages"]:=$LoadedPackages;
If[Not@ListQ@$LoadedPackages,
  $LoadedPackages={}
  ];


(* ::Subsubsection::Closed:: *)
(*PackageFilePath*)


PackageFilePath[p__]:=
  FileNameJoin[Flatten@{
    $PackageDirectory,
    p
    }];


(* ::Subsubsection::Closed:: *)
(*PackageExecute*)


PackageExecute[expr_]:=
  Internal`WithLocalSettings[
    Begin[$PackageContexts[[1]]];
    System`Private`NewContextPath@
      Prepend[
        $PackageContexts,
        "System`"
        ];,
    expr,
    System`Private`RestoreContextPath[];
    End[];
    ];
PackageExecute~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*PackagePullDeclarations*)


PackagePullDeclarationsAction//Clear
PackagePullDeclarationsAction[
  Hold[
    _Begin|_BeginPackage|
      CompoundExpression[_Begin|_BeginPackage,___]
    ]
  ]:=
  Throw[Begin];
PackagePullDeclarationsAction[e:Except[Hold[Expression]]]:=
  Sow@e;


PackagePullDeclarations[pkgFile_]:=
  pkgFile->
    Cases[
        Reap[
          With[{f=OpenRead[pkgFile]},
            Catch@
              Do[
                If[
                  Length[
                    ReadList[
                      f,
                      PackagePullDeclarationsAction@Hold[Expression],
                      1
                      ]
                    ]===0,
                    Throw[EndOfFile]
                  ],
                Infinity
                ];
            Close[f]
            ]
        ][[2,1]],
      s_Symbol?(
        Function[Null,
          Quiet[Context[#]===$Context],
          HoldAllComplete
          ]
          ):>
          HoldPattern[s],
      Infinity
      ]


(* ::Subsubsection::Closed:: *)
(*PackageLoadPackage*)


PackageLoadPackage[heldSym_,context_,pkgFile_->syms_]:=
  Block[{
    $loadingChain=
      If[ListQ@$loadingChain,$loadingChain,{}],
    $inLoad=TrueQ[$inLoad]
    },
    Internal`WithLocalSettings[
      System`Private`NewContextPath@$ContextPath,
      If[!MemberQ[$loadingChain,pkgFile],
        AppendTo[$loadingChain, pkgFile];
        With[{$$inLoad=$inLoad},
          $inLoad=True;
          Replace[Thread[syms,HoldPattern],
            Verbatim[HoldPattern][{s__}]:>Clear[s]
            ];
          If[Not@MemberQ[$ContextPath,context],
            $ContextPath=Prepend[$ContextPath,context];
            ];
          PackageAppGet[context,pkgFile];
          Unprotect[$LoadedPackages];
          AppendTo[$LoadedPackages, pkgFile];
          Protect[$LoadedPackages];
          ReleaseHold[heldSym]
          ]
        ],
      System`Private`RestoreContextPath[]
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*PackageDeclarePackage*)


PackageDeclarePackage[pkgFile_->syms_]:=
  With[{c=$Context},
    $DeclaredPackages[pkgFile]=syms;
    $PackageFileContexts[pkgFile]=c;
    Map[
      If[True,
        #:=PackageLoadPackage[#,c,pkgFile->syms]
        ]&,
      syms
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*PackageLoadDeclare*)


PackageLoadDeclare[pkgFile_String]:=
  If[!MemberQ[$LoadedPackages,pkgFile],
    If[!KeyMemberQ[$DeclaredPackages,pkgFile],
      PackageDeclarePackage@
        PackagePullDeclarations[pkgFile]
        ],
      PackageAppGet[pkgFile]
    ];


(* ::Subsubsection::Closed:: *)
(*PackageAppLoad*)


$Name["Load", args___]:=
  PackageAppLoad[args]


packageAppLoad[dir_, listing_]:=
  With[
    {
      fileNames=
        Select[
          FileNames["*", dir],
          DirectoryQ@#||MatchQ[FileExtension[#], "m"|"wl"]&
          ]
      },
    Replace[
      Select[fileNames, 
        StringMatchQ[
          ToLowerCase@FileNameTake[#],
          "__pre__."~~("m"|"wl")
          ]&
        ],
      {f_}:>Get[f]
      ];
    PackageAppLoad[
      $PackageListing[listing]=
        Select[fileNames, StringFreeQ["__"]@*FileBaseName]
      ];
    Replace[
      Select[fileNames, 
        StringMatchQ[
          ToLowerCase@FileNameTake[#], 
          "__Post__."~~("m"|"wl")
          ]&
        ],
      {f_}:>Get[f]
      ];
    ];


PackageAppLoad[dir_String?DirectoryQ]:=
  If[StringMatchQ[FileBaseName@dir,(WordCharacter|"$")..],
    Internal`WithLocalSettings[
      Begin["`"<>FileBaseName[dir]<>"`"],
      AppendTo[$PackageContexts, $Context];
      packageAppLoad[dir, FileNameDrop[dir, FileNameDepth[$PackageDirectory]+1]],
      End[]
      ]
    ];
PackageAppLoad[file_String?FileExistsQ]:=
  PackageLoadDeclare[file];
PackageAppLoad[]:=
  PackageExecute@
    packageAppLoad[
      FileNameJoin@{$PackageDirectory, $PackagePackagesDirectory}, 
      $PackageName
      ];
PackageAppLoad~SetAttributes~Listable;


(* ::Subsubsection::Closed:: *)
(*PackageAppGet*)


$Name["Get", f__]:=
  PackageAppGet[f];
PackageAppGet[f_]:=
  PackageExecute@
    With[{fBase = 
      If[FileExistsQ@f,
        f,
        PackageFilePath[$PackagePackagesDirectory, f<>".m"]
        ]
      },
      With[{cont = 
        Most@
          FileNameSplit[
            FileNameDrop[fBase, 
              FileNameDepth[PackageFilePath[$PackagePackagesDirectory]]
              ]
            ]},
        If[Length[cont]>0,
          Begin[StringRiffle[Append[""]@Prepend[""]@cont, "`"]];
          (End[];#)&@Get[fBase],
          Get[fBase]
        ]
      ]
    ];
PackageAppGet[c_,f_]:=
  PackageExecute[
    Begin[c];
    (End[];#)&@
      If[FileExistsQ@f,
        Get@f;,
        Get@PackageFilePath[$PackagePackagesDirectory, f<>".m"]
        ]
    ];


(* ::Subsubsection::Closed:: *)
(*End*)


End[]


(* ::Subsubsection::Closed:: *)
(*EndPackage*)


EndPackage[]


(* ::Subsection:: *)
(*Load*)


If[!TrueQ[`PackagePrivate`$PackageDeclared],
  `PackagePrivate`PackageAppLoad[];
  `PackagePrivate`$PackageDeclared
  ]


EndPackage[]
