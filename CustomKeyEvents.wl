(* ::Package:: *)

BeginPackage["CustomKeyEvents`"]


AddKeyEvent::usage="Adds a key event";
RemoveKeyEvent::usage="Removes a key event";


Begin["`Private`"]


doInContext[ex_]:=
  Internal`WithLocalSettings[
    System`Private`NewContextPath@{"System`", "FrontEnd`"};
    cachedContext=$Context;
    $Context="FrontEnd`",
    ex,
    System`Private`RestoreContextPath[];
    $Context=cachedContext;
    cachedContext=.
    ];
doInContext~SetAttributes~HoldAllComplete


doInContext[
  ToExpression@
    {
      "System`LinkedItems", "System`AlternateItems",
      "System`AlignmentGuidesEnabled","System`HelpMenu",
      "System`MenuAnchor", "System`MenuKey",
      "System`RawInputForm","System`RemoveMask",
      "System`Scope","System`ToggleOptionListElement",
      "System`Modifiers", "System`CellClass", "System`BoxClass"
      }
  ]


$menu=.


loadMenuSetup[]:=
  $menu=
    doInContext[
      Get@FrontEndExecute@
        FrontEnd`FindFileOnPath["MenuSetup.tr", "PrivatePathsTextResources"]
      ];


updateMenuSetup[]:=
  doInContext[
    FrontEndExecute@
      FrontEnd`ResetMenusPacket[{$menu}]
    ];


convertOptions[ops_]:=
  doInContext[
    ops/.s_String:>ToExpression[s]
    ]


makeKeyEventMenuItem[key_String, event_, ops_]:=
  MenuItem[
    Lookup[
      ops,
      FrontEnd`Name,
      StringRiffle[
        Flatten@{
          key,
          Replace[
            Flatten@{Lookup[ops, System`Modifiers, Nothing]}, 
            s_Symbol:>SymbolName[s], 
            1
            ]
          },
        "+"
        ]
      ],
    event,
    Evaluate@MenuKey[
      key,
      Apply[
        Sequence,
        FilterRules[{ops}, 
          Alternatives[
            System`Modifiers,
            System`BoxClass,
            System`CellClass
            ]
          ]
        ]
      ],
    Evaluate@
      Apply[
        Sequence,
        FilterRules[{ops}, 
          Except@
            Alternatives[
              FrontEnd`Name,
              System`Modifiers,
              System`BoxClass,
              System`CellClass
              ]
          ]
        ]
    ];
makeKeyEventMenuItem~SetAttributes~HoldRest


$keyEventsMenuPos=-2;


addKeyEventMenuItem[key_, event_, ops___]:=
  Module[
    {
      mi=makeKeyEventMenuItem[key, event, ops],
      kepos,
      kem
      },
    If[$menu[[2, $keyEventsMenuPos, 1]]==="KeyEvents",
      kem=$menu[[2, $keyEventsMenuPos]];
      kepos=FirstPosition[kem[[2, All, 1]], mi[[1]], None];
      If[kepos===None,
        $menu[[2, $keyEventsMenuPos]]=Insert[kem, mi, {2, -1}],
        $menu[[2, $keyEventsMenuPos, 2, kepos[[1]]]]=mi
        ],
      $menu=Insert[$menu, Menu["KeyEvents", {mi}], {2, $keyEventsMenuPos}]
      ];
    updateMenuSetup[];
    ];
addKeyEventMenuItem~SetAttributes~HoldRest


removeKeyEventMenuItem[key_, ops___]:=
  Module[
    {
      mi=makeKeyEventMenuItem[key, None, ops],
      kepos,
      kem
      },
    If[$menu[[2, $keyEventsMenuPos, 1]]==="KeyEvents",
      kem=$menu[[2, $keyEventsMenuPos]];
      kepos=FirstPosition[kem[[2, All, 1]], mi[[1]], None];
      If[kepos=!=None, 
        kem=Delete[kem, {2, kepos[[1]]}];
        $menu[[2, $keyEventsMenuPos]]=kem;
        If[Length@kem[[2]]==0, 
          $menu=Delete[$menu, {2, $keyEventsMenuPos}]
          ];
        updateMenuSetup[]
        ]
      ];
    ]


Options[AddKeyEvent]=
  {
    "Modifiers"->{},
    "CellClass"->"BoxFormData",
    "BoxClass"->"GraphEdit2D"
    };
AddKeyEvent[key_String, event_, ops___?OptionQ]:=
  Module[
    {
      ev=HoldComplete[event]
      },
    If[!MatchQ[$menu, _Menu], loadMenuSetup[]];
    ev=
      Replace[ev,
        {
          HoldComplete[e:Except[_FrontEndExecute|_String|_Rule|_RuleDelayed]]:>
            HoldComplete[KernelExecute[e]]
          }
        ];
    Replace[ev,
      HoldComplete[e_]:>
        addKeyEventMenuItem[key, e, convertOptions@Flatten@{ops}]
      ]
    ]


RemoveKeyEvent[key_String, ops___?OptionQ]:=
  removeKeyEventMenuItem[key, Flatten@{ops}]


End[]


EndPackage[]
