(* ::Package:: *)

BeginPackage["CustomKeyEvents`"]


AddKeyEvent::usage="Adds a key event";
RemoveKeyEvent::usage="Removes a key event";
SetKeyEventsMenuName::usage="Sets the menu name for the key events";
SetKeyEventsMenuPosition::usage="Sets the menu position for the key events";


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


ensureMenu[]:=
  If[!MatchQ[$menu, _Menu], loadMenuSetup[]];


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
  Module[
    {
      isPreemptive=True,
      ev=HoldComplete[event]
      },
     ev=
      Replace[ev,
        {
          HoldComplete[e:Except[_FrontEndExecute|_String|_Rule|_RuleDelayed]]:>
            (isPreemptive=False;HoldComplete[KernelExecute[e]])
          }
        ];
    Replace[ev,
      HoldComplete[e_]:>
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
          e,
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
              FilterRules[{ops, If[!isPreemptive, MenuEvaluator->Automatic, Nothing]}, 
                Except@
                  Alternatives[
                    FrontEnd`Name,
                    System`Modifiers,
                    System`BoxClass,
                    System`CellClass
                    ]
                ]
              ]
          ]
       ]
    ];
makeKeyEventMenuItem~SetAttributes~HoldRest


If[!IntegerQ@$keyEventsMenuPos, $keyEventsMenuPos=-2];
If[!StringQ@$keyEventsMenuName, $keyEventsMenuName="KeyEvents"];


addKeyEventMenuItem[key_, event_, ops___]:=
  Module[
    {
      mi=makeKeyEventMenuItem[key, event, ops],
      kepos,
      kem
      },
    If[$menu[[2, $keyEventsMenuPos, 1]]===$keyEventsMenuName,
      kem=$menu[[2, $keyEventsMenuPos]];
      kepos=FirstPosition[kem[[2, All, 1]], mi[[1]], None];
      If[kepos===None,
        $menu[[2, $keyEventsMenuPos]]=Insert[kem, mi, {2, -1}],
        $menu[[2, $keyEventsMenuPos, 2, kepos[[1]]]]=mi
        ],
      $menu=
        Insert[$menu, Menu[$keyEventsMenuName, {mi}], {2, $keyEventsMenuPos}]
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
    If[$menu[[2, $keyEventsMenuPos, 1]]===$keyEventsMenuName,
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
  (
    ensureMenu[];
    addKeyEventMenuItem[key, event, convertOptions@Flatten@{ops}]
    );
AddKeyEvent~SetAttributes~HoldRest


RemoveKeyEvent[key_String, ops___?OptionQ]:=
  (
    ensureMenu[];
    removeKeyEventMenuItem[key, convertOptions@Flatten@{ops}]
    )


SetKeyEventsMenuName[newName_String]:=
  With[
    {
      oldName=$keyEventsMenuName
      },
    ensureMenu[];
    If[$menu[[2, $keyEventsMenuPos, 1]]===oldName, 
      $menu[[2, $keyEventsMenuPos, 1]]=newName;
      updateMenuSetup[]
      ];
    $keyEventsMenuName=newName
    ];


SetKeyEventsMenuPosition[newPos_Integer]:=
  Module[
    {
      oldPos=$keyEventsMenuPos,
      men
      },
    ensureMenu[];
    If[$menu[[2, oldPos, 1]]===$keyEventsMenuName, 
      men=$menu[[2, oldPos]];
      $menu=Delete[$menu, {2, oldPos}];
      Insert[$menu, men, newPos];
      updateMenuSetup[]
      ];
    $keyEventsMenuPos=newPos
    ];


End[]


EndPackage[]
