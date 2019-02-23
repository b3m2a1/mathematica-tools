(* ::Package:: *)

BeginPackage["FrontEndNaming`"];
FENamed::usage = "A named object for display in the front end";


BeginPackage["`Package`"];


nameBoxApply::usage="";
getNamedObject::usage="";
selectNamedObject::usage="";
readNamedObjectContents::usage="";
replaceNamedObjectContents::usage="";
getNamedObjectOptions::usage="";
setNamedObjectOptions::usage="";


EndPackage[];


Begin["`Private`"];


feObjectPattern =
  _CellObject | _BoxObject | _NotebookObject |
   _FrontEnd`InputNotebook | _FrontEnd`EvaluationNotebook | _FrontEnd`ButtonNotebook |
   _FrontEnd`EvaluationCell | _FrontEnd`EvaluationBox |_FrontEnd`Self | _FrontEnd`Parent;


FENamed//Clear


(* Constructor DownValues *)
FENamed[
   FENamed[expr_, objOld : feObjectPattern | Automatic : Automatic, 
    name_String, ops : OptionsPattern[TagBox]
    ],
   objNew : feObjectPattern | Automatic : Automatic,
   name_String,
   ops2 : OptionsPattern[]
   ] := FENamed[expr, objNew, name, Flatten[{ops2, ops}]];
FENamed[
   expr_,
   obj : feObjectPattern | Automatic : Automatic,
   ops : OptionsPattern[]
   ] := FENamed[expr, obj, CreateUUID[], ops];


(* FormatValues *)
Format[n : 
    FENamed[expr_, obj : feObjectPattern | Automatic : Automatic, 
     name_String, ops : OptionsPattern[]]] :=
  RawBoxes@
   TemplateBox[
    {
     TagBox[ToBoxes[expr], name, BoxID -> name, ops],
     ToBoxes[obj], ToBoxes[name], ToBoxes[Flatten@{ops}]
     },
    "FENamed",
    DisplayFunction -> Function[#],
    InterpretationFunction ->
     Function[
      RowBox[{"FENamed", "[", #[[1]], ",", #2, ",", #3, "," , #4, 
        "]"}]],
    Editable -> True
    ];


Clear[
  nameBoxApply,
  nameBoxApply,
  getNamedObject,
  selectNamedObject,
  readNamedObjectContents,
  replaceNamedObjectContents,
  getNamedObjectOptions,
  setNamedObjectOptions
  ]


parseRelSpec[stuff_]:=
  Fold


nameBoxApply[
   obj : feObjectPattern | Automatic : Automatic, 
   sibling: _Down|_Up|_Left|_Right|None : Down[1],
   spec : Next | Previous : Next,
   refOps : _?OptionQ : {},
   name_String, 
   fn_, 
   args___
   ] :=
  With[{fob=Replace[obj, Automatic -> FrontEnd`InputNotebook[]]},
    MathLink`CallFrontEnd@
     fn[
      FE`BoxReference[
       FE`Evaluate@fob,
       {{name}},
       Sequence @@
        DeleteDuplicatesBy[First]@
         Flatten@{
           refOps,
           FE`BoxOffset -> 
            {
              Replace[sibling, 
                {
                  None->Nothing,
                  deeep_[n_]:>
                    Replace[deeep,
                      {
                        Up->FE`BoxParent,
                        Down->FE`BoxChild,
                        Left->FE`BoxLeftSibling,
                        Right->FE`BoxRightSibling
                        }
                      ][n]
                  }
                ]
              },
           Switch[spec,
             First,
               FE`SearchStart -> 
                 "StartFromBeginning",
             Next,
               FE`SearchStart -> 
                 "StartFromBeginning"(*Sequence@@{}*),
             Previous,
               FE`SearchStart -> 
                 "StartFromBeginning"(*Sequence@@{}*)
             ],
           FE`SearchStop->None,
           Switch[
             spec,
             First,
               FE`SearchDirection -> Backward,
             Next,
               FE`SearchDirection -> Forward,
             Previous,
               FE`SearchDirection -> Backward
             ]
           }
       ],
      args
      ]
    ];


getNamedObject[
  obj : feObjectPattern | Automatic : Automatic, 
  spec:Next|Previous:Next,
  name_String,
  ops:OptionsPattern[]
  ] :=
  nameBoxApply[obj, spec, {ops}, name, FrontEnd`BoxReferenceBoxObject];


selectNamedObject[
   obj : feObjectPattern | Automatic : Automatic, 
   spec:Next|Previous:Next,
   name_String,
   ops:OptionsPattern[]
   ] :=
  nameBoxApply[obj, spec, {ops}, 
    name, 
    FrontEnd`BoxReferenceFind
    ]; 


readNamedObjectContents[
   obj : feObjectPattern | Automatic : Automatic, 
   spec:Next|Previous:Next,
   name_String,
   ops:OptionsPattern[]
   ] :=
  nameBoxApply[obj, spec, {ops}, 
    name, 
    FrontEnd`BoxReferenceRead
    ];


replaceNamedObjectContents[
   obj : feObjectPattern | Automatic : Automatic, 
   spec:Next|Previous:Next,
   name_String, 
   boxes_,
   ops:OptionsPattern[]
   ] :=
  nameBoxApply[obj, spec, {ops}, 
    name, 
    FrontEnd`BoxReferenceReplace, 
    boxes
    ];


getNamedObjectOptions[
   obj : feObjectPattern | Automatic : Automatic, 
   spec:Next|Previous:Next,
   name_String,
   All,
   ops:OptionsPattern[]
   ] :=
  nameBoxApply[
    obj, 
    spec,
    {
      ops,
      FE`BoxOffset -> {FE`BoxParent[1]}
      }, 
    name,
    FrontEnd`BoxReferenceGetOptions
    ];
getNamedObjectOptions[
  obj : feObjectPattern | Automatic : Automatic, 
  spec:Next|Previous:Next,
  name_String, 
  opt_?OptionQ,
  ops:OptionsPattern[]
  ] :=
 nameBoxApply[obj, spec, {ops}, 
    name,
    FrontEnd`BoxReferenceGetOptions, 
    opt
    ]


setNamedObjectOptions[
   obj : feObjectPattern | Automatic : Automatic, 
   spec:Next|Previous:Next,
   name_String, 
   options_,
   ops:OptionsPattern[]
   ] :=
  nameBoxApply[obj, spec, {ops}, 
    name, 
    FrontEnd`BoxReferenceSetOptions, 
    options
    ];


(* UpValues interface on this *)
FENamed /:
  BoxObject[
   FENamed[_, obj : feObjectPattern | Automatic : Automatic, 
    name_String, ___],
   spec:Next|Previous:Next,
   ops:OptionsPattern[]
   ] :=
  getNamedObject[obj, spec, name, ops];
FENamed /:
  NotebookLocate[
   FENamed[_, obj : feObjectPattern | Automatic : Automatic, 
    name_String, ___],
   spec:Next|Previous:Next,
   ops:OptionsPattern[]
   ] :=
  selectNamedObject[obj, spec, name, ops];
FENamed /:
  NotebookRead[
   FENamed[_, obj : feObjectPattern | Automatic : Automatic, 
    name_String, ___],
   spec:Next|Previous:Next,
   ops:OptionsPattern[]
   ] :=
  readNamedObjectContents[obj, spec, name, ops];
FENamed /:
  NotebookWrite[
   FENamed[_, 
    obj : feObjectPattern | Automatic : Automatic, 
    name_String, 
    ___
    ],
   spec:Next|Previous:Next,
   cnts_,
   ops:OptionsPattern[]
   ] :=
  replaceNamedObjectContents[obj, spec, name, cnts, ops];
FENamed /:
  HoldPattern[
   Options[
    FENamed[_, 
     obj : feObjectPattern | Automatic : Automatic, 
     name_String, ___
     ], 
    spec:Next|Previous:Next,
    opt:_?OptionQ|All:All,
    ops:OptionsPattern[]
    ]
   ] :=
  getNamedObjectOptions[obj, spec, name, If[Length@opt==0, All, opt], ops];
FENamed /:
  SetOptions[
   FENamed[_, obj : feObjectPattern | Automatic : Automatic, 
    name_String, ___], 
   spec:Next|Previous:Next,
   opt_,
   ops:OptionsPattern[]
   ] :=
  setNamedObjectOptions[obj, spec, name, opt, ops];


End[];
EndPackage[];
