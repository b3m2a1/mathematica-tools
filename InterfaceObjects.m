(* ::Package:: *)



(* ::Section:: *)
(*Interfaces*)



(* ::Text:: *)
(*
	The idea for this package is that it will be used inside other packages to provide an easy, standard way to get all the nice OOP-like boilerplate in place that one generally would for an object based on an Association
*)



BeginPackage["InterfaceObjects`"];


RegisterInterface::usage=
  "Registers an object interface";


$InterfacePropertyStore::usage=
  "ExpressionStore for cached object properties";
InterfaceSetProperty::usage=
  "Sets a property value";
InterfacePropertyValue::usage=
  "Gets a property value";  
InterfaceRemoveProperty::usage=
  "Removes a property value";
InterfacePropertyList::usage=
  "Lists the properties on an object";
InterfaceCopyProperties::usage=
  "Copies properties from one object to another";


InterfaceMethod::usage=
  "Alias for defining object methods";
InterfaceAttribute::usage=
  "Alias for defining object attributes";


Begin["`Private`"];


(* ::Subsection:: *)
(*Properties*)



If[!ValueQ@$InterfacePropertyStore,
  $InterfacePropertyStore=Language`NewExpressionStore["<ObjectPropertyStore>"]
  ];


(* ::Subsubsection::Closed:: *)
(*get*)



propGet[spec_, prop_]:=
  Replace[$InterfacePropertyStore@"get"[spec, prop],
    {
      Null->Missing["KeyAbsent", prop],
      $$hold[x_]:>x
      }
    ];


(* ::Subsubsection::Closed:: *)
(*set*)



propSet[spec_, prop_, val_]:=
  (
    $InterfacePropertyStore@"put"[spec, prop, val];
    val
    )


(* ::Subsubsection::Closed:: *)
(*set*)



$$hold~SetAttributes~HoldAllComplete


propSetDelayed~SetAttributes~HoldRest


propSetDelayed[spec_, prop_, val_]:=
  (
    $InterfacePropertyStore@"put"[spec, prop, $$hold@val];
    )


(* ::Subsubsection::Closed:: *)
(*remove*)



propRemove[spec_, prop_]:=
  $InterfacePropertyStore@"remove"[spec, prop];


(* ::Subsubsection::Closed:: *)
(*keys*)



propKeys[spec__]:=
  $InterfacePropertyStore@"getKeys"[spec]


(* ::Subsubsection::Closed:: *)
(*computeProp*)



computeProp[spec_, prop_, func_, args___]:=
  Replace[propGet[spec, prop],
    Missing["KeyAbsent", prop]:>
      With[{v=func[args]},
        propSet[spec, prop, v];
        v
        ]
    ]
computeProp~SetAttributes~HoldAllComplete


(* ::Subsubsection::Closed:: *)
(*copyProps*)



copyProps[obj1_, obj2_, keyTest_:(True&)]:=
  Scan[If[keyTest[#], propSet[obj2, #, propGet[obj1, #]]]&, propKeys[obj1]]


(* ::Subsubsection::Closed:: *)
(*Properties*)



InterfaceSetProperty[spec_, prop_->val_]:=
  propSet[spec, prop, val];
InterfaceSetProperty[spec_, prop_:>val_]:=
  propSetDelayed[spec, prop, val];
InterfacePropertyValue[spec_, prop_]:=
  propGet[spec, prop];
InterfaceRemoveProperty[spec_, prop_]:=
  propRemove[spec, prop];
InterfacePropertyList[spec_]:=
  propKeys[spec]


(* ::Subsection:: *)
(*RegisterInterface*)



(* ::Text:: *)
(*
	This function makes it really easy to create a new object and avoid the boilerplate
*)



(* ::Subsubsection::Closed:: *)
(*RegisterInterface*)



Options[RegisterInterface]=
  {
    "Version"->1,
    "Atomic"->True,
    "Validator"->Automatic,
    "Constructor"->Automatic,
    "MutationHandler"->Automatic,
    "MutationFunctions"->None,
    "AccessorFunctions"->None,
    "NormalFunction"->Automatic,
    "Formatted"->True,
    "Icon"->None
    };
RegisterInterface[
  head_,
  keys_,
  ops:OptionsPattern[]
  ]:=
  Module[
    {
      entryQ=True@OptionValue["Atomic"],
      ctor=OptionValue["Constructor"],
      constructor,
      vdtor=OptionValue["Validator"],
      objQ,
      version=OptionValue["Version"],
      muh=OptionValue["MutationHandler"],
      mutationHandler,
      mud=OptionValue["MutationFunctions"],
      acc=OptionValue["AccessorFunctions"],
      norm=
        Replace[OptionValue["NormalFunction"],
          Automatic:>(KeyDrop[#, "Version"]&)
          ],
      format=OptionValue["Formatted"],
      icon=OptionValue["Icon"]
      },
    iRegisterInterfaceEntryQ[
      head,
      !entryQ
      ];
    If[vdtor===Automatic, vdtor=objQ];
    iRegisterInterfaceValidator[head, keys, vdtor];
    If[ctor===Automatic,
      createConstructor[constructor, keys];
      ctor=constructor;
      ];
    iRegisterInterfaceConstructor[
      head,
      version,
      ctor
      ];
    If[mud=!=None,
      If[muh===Automatic,
        muh=mutationHandler
        ];
      iRegisterInterfaceMutationHandler[
        head,
        mutationHandler,
        mud
        ]
      ];
    If[acc=!=None,
      iRegisterInterfaceAccessor[head, acc]
      ];
    If[norm=!=None,
      iRegisterInterfaceNormalForm[head, norm]
      ];
    If[format,
      iRegisterInterfaceFormatting[head, icon]
      ];
    ];


(* ::Subsubsubsection::Closed:: *)
(*createConstructor*)



createConstructor[constructor_, keys_]:=
  (
    constructor[a_Association]:=
      If[Length@keys>1||Length@keys===1&&KeyExistsQ[a, keys[[1]]],
        a,
        Quiet[AssociationThread[keys, {a}], AssociationThread::idim]
        ];
    constructor[args__]:=
      Quiet[AssociationThread[keys, {args}], AssociationThread::idim]
    )


(* ::Subsubsection::Closed:: *)
(*$InterfaceData*)



(* ::Subsubsubsection::Closed:: *)
(*$InterfaceData*)



If[!ValueQ@$InterfaceData,
  $InterfaceData=Language`NewExpressionStore["<ObjectDataStore>"]
  ];


(* ::Subsubsubsection::Closed:: *)
(*InterfaceConstructor*)



InterfaceConstructor/:
  (InterfaceConstructor[head_]=fn_):=
    $InterfaceData@"put"[head, "Constructor", fn];
InterfaceConstructor[head_]:=
  $InterfaceData@"get"[head, "Constructor"];


(* ::Subsubsubsection::Closed:: *)
(*InterfaceValidator*)



InterfaceValidator/:
  (InterfaceValidator[head_]=fn_):=
    $InterfaceData@"put"[head, "Validator", fn];
InterfaceValidator[head_]:=
  $InterfaceData@"get"[head, "Validator"];


(* ::Subsubsubsection::Closed:: *)
(*InterfaceCheckValid*)



InterfaceCheckValid/:
  (InterfaceCheckValid[head_]=fn_):=
    $InterfaceData@"put"[head, "CheckValid", fn];
InterfaceCheckValid[head_]:=
  $InterfaceData@"get"[head, "CheckValid"];


(* ::Subsubsubsection::Closed:: *)
(*InterfaceCheckInvalid*)



InterfaceCheckInvalid/:
  (InterfaceCheckInvalid[head_]=fn_):=
    $InterfaceData@"put"[head, "CheckInvalid", fn];
InterfaceCheckInvalid[head_]:=
  $InterfaceData@"get"[head, "CheckInvalid"];


(* ::Subsubsubsection::Closed:: *)
(*InterfaceSetValid*)



InterfaceSetValid/:
  (InterfaceSetValid[head_]=fn_):=
    $InterfaceData@"put"[head, "SetValid", fn];
InterfaceSetValid[head_]:=
  $InterfaceData@"get"[head, "SetValid"];


(* ::Subsubsubsection::Closed:: *)
(*InterfaceMethods*)



InterfaceMethods/:
  (InterfaceMethods[head_]=meths_):=
    $InterfaceData@"put"[head, "Methods", meths];
InterfaceMethods[head_]:=
  If[#===Null, <||>, #]&@$InterfaceData@"get"[head, "Methods"];


(* ::Subsubsubsection::Closed:: *)
(*InterfaceAttributes*)



InterfaceAttributes/:
  (InterfaceAttributes[head_]=attrs_):=
    $InterfaceData@"put"[head, "Attributes", attrs];
InterfaceAttributes[head_]:=
  $InterfaceData@"get"[head, "Attributes"];


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceEntryQ*)



iRegisterInterfaceEntryQ[head_, entryQ_]:=
  With[
    {
      checkInvalid=
        If[TrueQ@entryQ,
          System`Private`HoldNotValidQ,
          System`Private`HoldEntryQ
          ],
      checkValid=
        If[TrueQ@entryQ,
          System`Private`HoldValidQ,
          System`Private`HoldNoEntryQ
          ],
      setValid=
        If[TrueQ@entryQ,
          System`Private`HoldSetValid,
          System`Private`HoldSetNoEntry
          ]
      },
    InterfaceCheckInvalid[head]=checkInvalid;
    InterfaceCheckValid[head]=checkValid;
    InterfaceSetValid[head]=setValid;
    ]


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceValidator*)



iRegisterInterfaceValidator[
  head_,
  keys_,
  testFunction_
  ]:=
  With[
    {
      checkValid=InterfaceCheckValid[head]
      },
    (* Test if object is object *)
    InterfaceValidator[head]=testFunction;
    testFunction[d_Association?AssociationQ]:=
      AllTrue[keys, KeyExistsQ[d, #]&];
    testFunction/:
      HoldPattern[testFunction[head[a_Association]?checkValid]]:=
      testFunction[a];
    testFunction[___]:=False;
    ]


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceConstructor*)



iRegisterInterfaceConstructor[
  head_,
  version_,
  constructor_
  ]:=
  With[
    {
      validator=InterfaceValidator[head],
      checkInvalid=InterfaceCheckInvalid[head],
      setValid=InterfaceSetValid[head]
      },
    InterfaceConstructor[head]=constructor;
    (* Constructor DownValue on the object *)
    head//ClearAll;
    head~SetAttributes~HoldAllComplete;
    head[args___]?checkInvalid:=
      With[{a=constructor[args]},
        With[{a2=Append[a, "Version"->version]},
          If[TrueQ@validator@a2,
            setValid@head[a2],
            Failure["BuildFailure",
              <|
                "MessageTemplate"->"Failed to build `` object from data ``",
                "MessageParameters"->{head, HoldForm[{args}]}
                |>
              ]
            ]
          ]/;AssociationQ[a]
        ];
    ]


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceMutationHandler*)



$noArgMutations=
  Alternatives@@
    {Unset, Increment, Decrement};


$oneArgMutations=
  Alternatives@@
    {
        Set, SetDelayed,
        AddTo, SubtractFrom, TimesBy, DivideBy,
        AppendTo, PrependTo,
        AssociateTo, KeyDropFrom
        };


iRegisterInterfaceMutationHandler[
  head_,
  func_,
  dispatcher_
  ]:=
  With[
    {
      oQ=InterfaceValidator[head],
      symQ=Unique[symbolQ],
      oA=$oneArgMutations,
      nA=$noArgMutations,
      d=
        If[!AssociationQ@dispatcher, 
          <|"Keys"->dispatcher|>,
          dispatcher
          ]
      },
    symQ~SetAttributes~{Temporary, HoldFirst};
    symQ[sym_]:=
      MatchQ[OwnValues[sym], {_:>_head?oQ}];
    func // ClearAll;
    func~SetAttributes~HoldAllComplete;
    If[KeyExistsQ[d, "Self"],
      func[
        (h: oA)[obj_Symbol?symQ, val_]
        ] :=
        With[{mut=dispatcher["Self"][h]},
          mut[obj, val]/;Head[mut]===Symbol
          ];
      func[
        (h : nA)[obj_Symbol?symQ]
        ] :=
        With[{mut=dispatcher["Self"][h]},
          mut[obj]/;Head[mut]===Symbol
          ];
      ];
    If[KeyExistsQ[d, "Keys"],
      func[
        (h: oA)[obj_Symbol?symQ[attr__], val_]
        ] :=
        With[{mut=dispatcher["Keys"][h]},
          mut[obj, attr, val]/;Head[mut]===Symbol
          ];
      func[
        (h : nA)[obj_Symbol?symQ[attr__]]
        ] :=
        With[{mut=dispatcher["Keys"][h]},
          mut[obj, attr]/;Head[mut]===Symbol
          ];
      ];
    If[KeyExistsQ[d, "Parts"],
      func[
        (h: oA)[obj_Symbol?symQ[[part__]], val_]
        ] :=
        With[{mut=dispatcher["Parts"][h]},
          mut[obj, part, val]/;Head[mut]===Symbol
          ];
      func[
        (h : nA)[obj_Symbol?symQ[[part__]]]
        ] :=
        With[{mut=dispatcher["Parts"][h]},
          mut[obj, part]/;Head[mut]===Symbol
          ];
      ];
    (* fallthrough to get normal behavior back *)
    func[___] :=
      Language`MutationFallthrough;
    Language`SetMutationHandler[head, func]
    ]


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceMethod*)



iRegisterInterfaceMethod~SetAttributes~HoldRest;
iRegisterInterfaceMethod[
  head_,
  methodName_,
  name_,
  args___,
  def_
  ]:=
  With[
    {
      mn=methodName,
      valid=InterfaceValidator[head],
      meths=InterfaceMethods[head],
      noop=Unevaluated@name
      },
    InterfaceMethods[head]=
      If[meths===Null,
        <|mn->True|>,
        Append[meths, mn->True]
        ]; 
    noop_head?valid[mn[args]]:=
      def;
    noop_head?valid[mn][args]:=
      def;
    ];


InterfaceMethod/:
  (
    InterfaceMethod[
      Verbatim[Pattern][name_, Verbatim[Blank][head_]][method_][args___]
      ]:=def_
    ):=
    iRegisterInterfaceMethod[head, method, name, args, def];


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceAttribute*)



iRegisterInterfaceAttribute~SetAttributes~HoldRest;
iRegisterInterfaceAttribute[
  head_,
  attr_,
  name_,
  def_
  ]:=
  With[
    {
      mn=attr,
      valid=InterfaceValidator[head],
      meths=InterfaceAttributes[head],
      noop=Unevaluated@name
      },
    InterfaceAttributes[head]=
      If[meths===Null,
        <|mn->True|>,
        Append[meths, mn->True]
        ]; 
    noop_head?valid[mn]:=
      def;
    ];


InterfaceAttribute/:
  (
    InterfaceAttribute[
      Verbatim[Pattern][name_, Verbatim[Blank][head_]][attr_]
      ]:=def_
    ):=
    iRegisterInterfaceAttribute[head, attr, name, def];


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceAccessor*)



iRegisterInterfaceAccessor[head_, dispatcher_]:=
  With[
    {
      ea=If[!AssociationQ@dispatcher, <|"Keys"->dispatcher|>, dispatcher],
      valid=InterfaceValidator[head]
      },
    If[KeyExistsQ[ea, "Keys"],
      With[{lookup=dispatcher["Keys"]},
        obj_head?valid[attr_?(!KeyExistsQ[InterfaceMethods[head], #]&)]:=
          lookup[obj, attr];
        obj_head?valid[attr1_, attrs__]:=
          lookup[obj, attr1, attrs];
        ];
      ];
    If[KeyExistsQ[ea, "Parts"],
      With[{part=dispatcher["Parts"]},
        head/:obj_head?valid[[p__]]:=
          part[obj, p];
        ];
      ];
    ];


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceNormalForm*)



iRegisterInterfaceNormalForm[head_, norm_]:=
  With[
    {
      valid=InterfaceValidator[head]
      },
    head/:HoldPattern[Normal[head[a_]?valid]]:=norm@a
    ];


(* ::Subsubsection::Closed:: *)
(*iRegisterInterfaceFormatting*)



iRegisterInterfaceFormatting[
  head_,
  icon_(*,
	keys_ I'll add this in the future...
	*)
  ]:=
  With[{valid=InterfaceValidator[head]},
    Format[HoldPattern[obj:head[a_]?valid]]:=
      RawBoxes@
        BoxForm`ArrangeSummaryBox[
          head,
          Unevaluated[Head@@{a}],
          icon,
          {
            BoxForm`MakeSummaryItem[
              {
                "Keys: ", 
                DeleteCases[Keys@a, "Version"]
                },
              StandardForm
              ]
            },
          {
            },
          StandardForm
          ]
    ]


End[];


EndPackage[];



