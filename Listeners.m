(* ::Package:: *)



(* ::Chapter:: *)
(*Listener Pattern*)



BeginPackage["Listeners`"];


Listener::usage="Core listener object to work with";
Listeners::usage="Lists the listener objects";


BeginPackage["`Package`"];


$Listeners::usage="";


createListener::usage="";
updateListener::usage="";
removeListener::usage="";
getListener::usage="";


addListener::usage="";
dropListener::usage="";


listenerExists::usage="";
listenerObject::usage="";
listenerData::usage="";


EndPackage[];


ListenerDynamic::usage="Dynamic-esque block but with a set of named listeners";


Begin["`Private`"];


(* ::Subsection:: *)
(*Listener cache*)



(* ::Text:: *)
(*
	Using a weak hash map so that we can store data but still have it nicely cleaned up
*)



If[Head@$Listeners=!=Language`ExpressionStore,
  $Listeners=Language`NewExpressionStore["Listeners"];
  Scan[$Listeners@"remove"[#[[1]]]&, $Listeners@"listTable"[]];
  ]


(* ::Subsection:: *)
(*OOP*)



(* ::Subsubsection::Closed:: *)
(*MutationHandler*)



listenerMutationHandler~SetAttributes~HoldAllComplete


(* ::Subsubsubsection::Closed:: *)
(*l[\[OpenCurlyDoubleQuote]Callback\[CloseCurlyDoubleQuote]] =*)



listenerMutationHandler[s_Symbol?listenerSymbol["Callback"]=val_]:=
  setListenerCallback[s, val];
listenerMutationHandler[s_Symbol?listenerSymbol["Callback"]:=val_]:=
  setListenerCallbackDelayed[s, val];
listenerMutationHandler[li_Listener["Callback"]=val_]:=
  setListenerCallback[li, val];
listenerMutationHandler[li_Listener["Callback"]:=val_]:=
  setListenerCallbackDelayed[li, val];


(* ::Subsubsubsection::Closed:: *)
(*l[\[OpenCurlyDoubleQuote]Value\[CloseCurlyDoubleQuote]] =*)



listenerMutationHandler[s_Symbol?listenerSymbol["Value"]=val_]:=
  updateListener[s, val];
listenerMutationHandler[s_Symbol?listenerSymbol["Value"]:=val_]:=
  updateListenerDelayed[s, val];
listenerMutationHandler[li_Listener["Value"]=val_]:=
  updateListener[li, val];
listenerMutationHandler[li_Listener["Value"]:=val_]:=
    updateListenerDelayed[li, val];


(* ::Subsubsubsection::Closed:: *)
(*l[\[OpenCurlyDoubleQuote]Value\[CloseCurlyDoubleQuote]] :=*)



listenerMutationHandler[s_Symbol?listenerSymbol["Value"]=val_]:=
  updateListener[s, val];
listenerMutationHandler[s_Symbol?listenerSymbol["Value"]:=val_]:=
  updateListenerDelayed[s, val];
listenerMutationHandler[li_Listener["Value"]=val_]:=
  updateListener[li, val];
listenerMutationHandler[li_Listener["Value"]:=val_]:=
  updateListenerDelayed[li, val];


(* ::Subsubsubsection::Closed:: *)
(*l =*)



listenerMutationHandler[s_Symbol?listenerSymbol=val_]:=
  updateListener[s, val];
listenerMutationHandler[s_Symbol?listenerSymbol:=val_]:=
  updateListenerDelayed[s, val];
listenerMutationHandler[li_Listener=val_]:=
  updateListener[li, val];
listenerMutationHandler[li_Listener:=val_]:=
    updateListenerDelayed[li, val];


(* ::Subsubsubsection::Closed:: *)
(*l[__] = ...*)



listenerMutationHandler[s_Symbol?listenerSymbol[key__]=val_]:=
  Quiet@
    Check[
      updateListenerKey[s, key, val],
      Language`MutationFallthrough
      ];
listenerMutationHandler[s_Symbol?listenerSymbol[key__]:=val_]:=
  Quiet@
    Check[
      updateListenerKeyDelayed[s, key, val],
      Language`MutationFallthrough
      ];
listenerMutationHandler[li_Listener[key__]=val_]:=
  Quiet@
    Check[
      updateListenerKey[li, key, val],
      Language`MutationFallthrough
      ];
listenerMutationHandler[li_Listener[key__]:=val_]:=
  Quiet@
    Check[
      updateListenerKeyDelayed[li, key, val],
      Language`MutationFallthrough
      ];


(* ::Subsubsubsection::Closed:: *)
(*l[[__]] = ...*)



listenerMutationHandler[s_Symbol?listenerSymbol[[key__]]=val_]:=
  Quiet@
    Check[
      updateListenerPart[s, key, val],
      Language`MutationFallthrough
      ];
listenerMutationHandler[s_Symbol?listenerSymbol[[key__]]:=val_]:=
  Quiet@
    Check[
      updateListenerPartDelayed[s, key, val],
      Language`MutationFallthrough
      ];
listenerMutationHandler[li_Listener[[key__]]=val_]:=
  Quiet@
    Check[
      updateListenerPart[li, key, val],
      Language`MutationFallthrough
      ];
listenerMutationHandler[li_Listener[[key__]]:=val_]:=
  Quiet@
    Check[
      updateListenerPartDelayed[li, key, val],
      Language`MutationFallthrough
      ];


(* ::Subsubsubsection::Closed:: *)
(*l =.*)



listenerMutationHandler[s_Symbol?listenerSymbol=.]:=
  Quiet@
    Check[
      removeListener[s];s=.,
      Language`MutationFallthrough
      ];
Listener/:Unset@li_Listener:=
  removeListener[li];


(* ::Subsubsubsection::Closed:: *)
(*Clear@l*)



listenerMutationHandler[s_Symbol?listenerSymbol//Clear]:=
  Quiet@
    Check[
      removeListener[s];s//Clear,
      Language`MutationFallthrough
      ];
Listener/:Clear@li_Listener?listenerSymbol:=
  removeListener[li];


(* ::Subsubsubsection::Closed:: *)
(*Remove@l*)



listenerMutationHandler[s_Symbol?listenerSymbol//Remove]:=
  Quiet@
    Check[
      removeListener[s];s//Remove,
      Language`MutationFallthrough
      ];
Listener/:Remove@li_Listener:=
  removeListener[li];


(* ::Subsubsubsection::Closed:: *)
(*Fallback*)



listenerMutationHandler[e___]:=
  Language`MutationFallthrough;


Language`SetMutationHandler[Listener, listenerMutationHandler];


(* ::Subsubsection::Closed:: *)
(*Constructor*)



Listener[name_String]?System`Private`HoldEntryQ:=
  Replace[listenerObject[name],
    _Missing:>createListener[name]
    ];
Listener[]:=
  Listener[CreateUUID[]]


(* ::Subsubsection::Closed:: *)
(*Value*)



li_Listener?listenerExists["Value"]:=
  getListener[li]


(* ::Subsubsection::Closed:: *)
(*Name*)



li_Listener?listenerExists["Name"]:=
  li


(* ::Subsubsection::Closed:: *)
(*Variable*)



li_Listener?listenerExists["Variable"]:=
  getListenerVariable[li]


(* ::Subsubsection::Closed:: *)
(*ID*)



li_Listener?listenerExists["ID"]:=
  getListenerID[li]


(* ::Subsubsection::Closed:: *)
(*Expression*)



li_Listener?listenerExists["Expression"]:=
  getListenerExpression[li]


(* ::Subsubsection::Closed:: *)
(*Callback*)



li_Listener?listenerExists["Callback"]:=
  getListenerCallback[li]


(* ::Subsubsection::Closed:: *)
(*Others*)



li_Listener?listenerExists[key__]:=
  getListener[li][key]


Listener/:li_Listener?listenerExists[[key__]]:=
  getListener[li][[key]]


(* ::Subsection:: *)
(*API*)



(* ::Subsubsection::Closed:: *)
(*Listeners*)



Listeners[]:=
  Listener/@$Listeners@"getKeys"[$listeners];


(* ::Subsubsection::Closed:: *)
(*listenerObject*)



listenerObject[name_]:=
  SelectFirst[
    ($Listeners@"listTable"[])[[All, 1]], 
    #["Name"]==name&, 
    Missing["KeyAbsent", name]
    ]


(* ::Subsubsection::Closed:: *)
(*listenerData*)



listenerData[li_Listener, key_]:=
  Replace[
    $Listeners@"get"[li, key],
    Null:>
      Missing["KeyAbset", key]
    ];
listenerData[name_String, key_]:=
  With[{listener=listenerObject[name]},
    listenerData[listener, key]
    ];
listenerData[li_Listener]:=
  AssociationMap[
    $Listeners@"get"[li, #]&,
    {
      "ID",
      "Track",
      "Variable",
      "Callback"
      }
    ];
listenerData[name_]:=
  With[{listener=listenerObject[name]},
    listenerData@listener
    ]


(* ::Subsubsection::Closed:: *)
(*listenerExists*)



listenerSymbol[s_Symbol]:=
  MatchQ[OwnValues[s], {_:>_Listener}];
listenerSymbol~SetAttributes~HoldFirst;


listenerExists[name_String]:=
  !MissingQ@listenerObject[name];
listenerExists[li_Listener]:=
  System`Private`HoldNoEntryQ@li&&
    IntegerQ@listenerData[li, "ID"];
listenerExists[s_Symbol]:=
  MatchQ[OwnValues[s], {_:>_Listener?listenerExists}];
listenerExists[e_]:=
  With[{eval=Evaluate@e},
    MatchQ[eval, _Listener|_String]&&
      listenerExists@eval
    ];
listenerExists~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*addListener*)



addListener[obj_Listener, data_]:=
  (
    KeyValueMap[$Listeners@"put"[obj, ##]&, data];
    obj
    )


addListener[name_String, data_]:=
  addListener[
    System`Private`HoldSetNoEntry[Listener[name]],
    data
    ]


(* ::Subsubsection::Closed:: *)
(*dropListener*)



dropListener[obj_Listener]:=
  $Listeners@"remove"[obj];


dropListener[name_]:=
  dropListener@listenerObject[name];


(* ::Subsubsection::Closed:: *)
(*Create*)



createListener[name_String]:=
  Module[
    {
      listener, 
      id=$ModuleNumber,
      callback
      },
    listener=Null;
    callback=True&;
    addListener[
      name,
      <|
        "ID"->id,
        "Track"->Internal`TrackExpression[listener, id],
        "Variable"->Hold[listener],
        "Callback"->Hold[callback]
        |>
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*Callback*)



setListenerCallback[li_Listener, val_]:=
  Replace[listenerData[li, "Callback"],
    Verbatim[Hold][var_]:>
      Set[var, val]
      ];
setListenerCallback~SetAttributes~HoldRest;


setListenerCallbackDelayed[li_Listener, val_]:=
  Replace[listenerData[li,"Callback"],
    Verbatim[Hold][var_]:>
      SetDelayed[var, val]
      ];
setListenerCallbackDelayed~SetAttributes~HoldRest;


callbackFailure~SetAttributes~HoldRest
callbackFailure[key_, name_, args__]:=
  Failure["CallbackRejected",
    <|
      "MessageTemplate"->"Callback rejected change `` to variable `` with args ``",
      "MessageParameters"->{key, name, HoldForm[{args}]}
      |>
    ]


callbackFailure~SetAttributes~HoldRest
callbackFailure[key_, name_]:=
  Failure["CallbackRejected",
    <|
      "MessageTemplate"->"Callback rejected change `` to variable ``",
      "MessageParameters"->{key, name}
      |>
    ]


(* ::Subsubsection::Closed:: *)
(*Update*)



(* ::Subsubsubsection::Closed:: *)
(*Template*)



updateListenerTemplate[li_Listener, method_, fn_, args__]:=
  With[
    {
      sym=listenerData[li, "Variable"],
      cb =listenerData[li,"Callback"][[1]]
      },
    If[cb[method, li["Name"], args]=!=False,
      Replace[sym,
        Verbatim[Hold][var_]:>
          With[{v=fn[var, args]},
            v
            ]
        ],
      callbackFailure[method, li["Name"], args]
      ]
    ];
updateListenerTemplate[name_String, method_, fn_, args__]:=
  updateListenerTemplate[listenerObject[name], method, fn, args];
updateListenerTemplate~SetAttributes~HoldAllComplete


(* ::Subsubsubsection::Closed:: *)
(*Value*)



updateListener[li_Listener, val_]:=
  updateListenerTemplate[li, "Update", Set, val];
updateListener~SetAttributes~HoldRest


updateListenerDelayed[li_Listener, val_]:=
  updateListenerTemplate[li, "UpdateDelayed", SetDelayed, val];
updateListenerDelayed~SetAttributes~HoldRest


(* ::Subsubsubsection::Closed:: *)
(*Key*)



setKey[var_, key__, val_]:=
  var[key]=val;
setKey~SetAttributes~HoldAllComplete


updateListenerKey[li_Listener, key__, val_]:=
  updateListenerTemplate[li, "UpdateKey", setKey, key, val];
updateListenerKey~SetAttributes~HoldRest


setKeyD[var_, key__, val_]:=
  var[key]:=val;
setKeyD~SetAttributes~HoldAllComplete


updateListenerKeyDelayed[li_Listener, key__, val_]:=
  updateListenerTemplate[li, "UpdateKeyDelayed", setKeyD, key, val];
updateListenerKeyDelayed~SetAttributes~HoldRest


(* ::Subsubsubsection::Closed:: *)
(*Part*)



setPart[var_, key__, val_]:=
  var[[key]]=val;
setPart~SetAttributes~HoldAllComplete


updateListenerPart[li_Listener, key__, val_]:=
  updateListenerTemplate[li, "UpdatePart", setPart, key, val];
updateListenerPart~SetAttributes~HoldRest


setPartD[var_, key__, val_]:=
  var[[key]]:=val;
setPartD~SetAttributes~HoldAllComplete


updateListenerPartDelayed[li_Listener, key__, val_]:=
  updateListenerTemplate[li, "UpdatePart", setPartD, key, val];;
updateListenerPartDelayed~SetAttributes~HoldRest


(* ::Subsubsection::Closed:: *)
(*Remove*)



removeListener[li_Listener]:=
  With[
    {
      data=listenerData[li]
      },
    If[data["Callback"][[1]]["Remove", li["Name"]]=!=False,
      (*Clear@@Evaluate[data["Variable"]];*)
      Internal`SetValueNoTrack@@Append[data["Variable"], data["ID"]];
      dropListener[li];,
      callbackFailure["Remove", li["Name"]]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*Get*)



getListener[li_]:=
  ReleaseHold@listenerData[li,"Variable"]


(* ::Subsubsection::Closed:: *)
(*Name*)



getListenerName[Listener[name_]]:=
  name;


(* ::Subsubsection::Closed:: *)
(*Variable*)



getListenerVariable[li_]:=
  listenerData[li,"Variable"]


(* ::Subsubsection::Closed:: *)
(*ID*)



getListenerID[li_]:=
  listenerData[li,"ID"]


(* ::Subsubsection::Closed:: *)
(*Expression*)



getListenerExpression[li_]:=
  listenerData[li,"Track"]


(* ::Subsubsection::Closed:: *)
(*Callback*)



getListenerCallback[li_]:=
  ReleaseHold@listenerData[li, "Callback"]


(* ::Subsection:: *)
(*Formatting*)



(* ::Subsubsection::Closed:: *)
(*Format*)



MakeBoxes[
  li_Listener?listenerExists,
  fmt:StandardForm
  ]:=
  With[
    {
      boxes=
        Replace[getListenerVariable[li],
          Verbatim[Hold][var_]:>
            DynamicBox[ToBoxes@var]
          ]
      },
    InterpretationBox[
      boxes,
      li
      ]
    ]


(* ::Subsection:: *)
(*Dynamic*)



(* ::Subsubsection::Closed:: *)
(*ListenerDynamic*)



MakeBoxes[
  ld:ListenerDynamic[
    expr_, 
    lis:{__Listener?listenerExists}, 
    args:___?(Not@*OptionQ),
    ops:OptionsPattern[Dynamic]
    ],
  fmt:StandardForm
  ]:=
  With[
    {
      boxes=
        Replace[
          Thread[
            Replace[
              Map[listenerData[#, "Variable"]&, lis], 
              Except[_Hold]->Nothing, 
              1
              ],
            Hold
            ],
          {
            Verbatim[Hold][s_]:>
              DynamicBox[
                s;
                ToBoxes@expr,
                args,
                TrackedSymbols:>s,
                ops
                ],
            _:>
              DynamicBox[
                ToBoxes@expr,
                args,
                ops
                ]
            }
          ]
      },
    InterpretationBox[boxes, ld]
    ];
ListenerDynamic[expr_, 
    blech:Except[{__Listener}, _Listener|_String|{(_String|_Listener)..}], 
    args:___?(Not@*OptionQ),
    ops:OptionsPattern[Dynamic]
    ]:=
  ListenerDynamic[
    expr, 
    If[StringQ@#, Listener[#], #]&/@Flatten@{blech},
    args, 
    ops
    ];
ListenerDynamic~SetAttributes~HoldFirst;


End[];


EndPackage[];



