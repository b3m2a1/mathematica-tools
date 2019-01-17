(* ::Package:: *)

(* ::Section:: *)
(*WLFiddle*)


BeginPackage["WLFiddle`"]


MakeWLFiddle::usage=
  "MakeWLFiddle[nb] turns nb or its current selection into a fiddle URL
MakeWLFiddle[cells] turns a set of cells into a fiddle URL";


Begin["`Private`"]


cellToString[c:(Cell[b_BoxData, ___]|Cell[_, "Input", ___])]:=
  First@FrontEndExecute@
      ExportPacket[c, "InputText"];
cellToString[c:Cell[_,s_String, ___]]:=
  ExportString[
    <|"style"->s,
      "content"->
        First@FrontEndExecute@
            ExportPacket[c, "PlainText"]
      |>,
    "JSON",
    "Compact"->True
    ];


(* ::Text:: *)
(*WLFiddle is the real powerhouse here. It uses wlfiddle.js and wlfiddle.css to make an embedded fiddle nb.*)


$fiddleURL="https://www.wolframcloud.com/objects/b3m2a1/WLFiddle";
$embedURL="https://www.wolframcloud.com/objects/b3m2a1/WLFiddleEmbed";


Options[MakeWLFiddle]=
  {
    "ShortenURL"->True,
    "BaseURL"->Automatic
    };
MakeWLFiddle[cells:{__Cell}, ops:OptionsPattern[]]:=
  With[
    {
      cc=NotebookTools`FlattenCellGroups[cells],
      key=StringJoin[ToString/@RandomInteger[10, 15]],
      url,
      base
      },
    base=
      Replace[OptionValue["BaseURL"],
        {
          "Embed":>$embedURL,
          Except[_String]:>$fiddleURL
          }
        ];
    url = StringReplace[
      URLBuild[
        base,
        MapIndexed[
          "cell"<>ToString[#2[[1]]]->
            Developer`EncodeBase64[cellToString[#]]&,
          cc
          ]
        ],
      key->" "
      ];
    If[TrueQ@OptionValue["ShortenURL"],
      URLShorten[url],
      url
      ]
    ];
MakeWLFiddle[notebook_NotebookObject, ops:OptionsPattern[]]:=
  Module[
    {
      cells=Flatten@{NotebookRead[notebook]},
      cc
      },
    If[Length@cells==0,
      cells=First@NotebookGet[notebook]
      ];
    MakeWLFiddle[cells, ops]
    ];


End[]


EndPackage[]
