(* ::Package:: *)

(* ::Section:: *)
(*KernelTunnels*)


BeginPackage["KernelTunnels`"]


KernelTunnels::usage="";
TunnelCreate::usage="";
TunnelConnect::usage="";
TunnelClose::usage="";
TunnelWrite::usage="";
TunnelRead::usage="";
TunnelRemove::usage="";
SetTunnelReadHandler::usage="";
RemoveTunnelReadHandler::usage="";


(* ::Subsection:: *)
(*Package*)


BeginPackage["`Package`"]


TunnelDataLockedQ::usage="";
TunnelDataWait::usage="";
TunnelDataRead::usage="";
TunnelDataWrite::usage="";
TunnelInitiatorQ::usage="";
TunnelConnectorQ::usage="";
TunnelAttachedQ::usage="";
TunnelLink::usage="";
LinkAliveQ::usage="";
TunnelCheck::usage="";
TunnelConnectedQ::usage="";
TunnelRemoveInitiator::usage="";
TunnelRemoveConnector::usage="";
$DefaultTunnelReadHandler::usage="";
GetTunnelReadHandler::usage="";


EndPackage[]


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*KernelTunnels*)


$TunnelFile=FileNameJoin@{$TemporaryDirectory, "KernelTunnels.mx"};
$TunnelFileLock=$TunnelFile<>".lock";
KernelTunnels[]:=
  If[!FileExistsQ@$TunnelFile, 
    TunnelDataWrite[<||>];<||>,
    Replace[Import[$TunnelFile],
      {
        a_Association?AssociationQ:>
          Select[a, AssociationQ],
        _:>(TunnelDataWrite[<||>];<||>)
        }
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*TunnelDataLockedQ*)


TunnelDataLockedQ[]:=
  FileExistsQ[$TunnelFileLock];


(* ::Subsubsection::Closed:: *)
(*TunnelDataWait*)


TunnelDataWait[]:=
  While[TunnelDataLockedQ[], Pause[.001]];


(* ::Subsubsection::Closed:: *)
(*TunnelDataRead*)


TunnelDataRead[]:=
  (
    TunnelDataWait[];
    Import[$TunnelFile]
    );


(* ::Subsubsection::Closed:: *)
(*TunnelDataWrite*)


TunnelDataWrite[a_]:=
  (
    TunnelDataWait[];
    Internal`WithLocalSettings[
      Close@OpenWrite[$TunnelFileLock],
      Export[$TunnelFile, a],
      DeleteFile[$TunnelFileLock]
      ]
    )


(* ::Subsubsection::Closed:: *)
(*TunnelCreateAssociation*)


TunnelCreateAssociation[name_, link_, ops___]:=
  <|
    "Name"->name, "Options"->{ops},
    "Initiator"->$SessionID, "Mouth"->link, 
    "Connector"->None, "Tail"->None
    |>


(* ::Subsubsection::Closed:: *)
(*TunnelCreateError*)


TunnelCreateError[name_, ops_]:=
  Failure["TunnelUnkown",
    <|
      "MessageTemplate"->
        "Failed to create tunnel with name `` and options ``",
      "MessageParameters"->
        {name, ops}
      |>
    ]


(* ::Subsubsection::Closed:: *)
(*iTunnelCreate*)


iTunnelCreate[tuns_, name_, ops:OptionsPattern[]]:=
  Module[{link=LinkCreate[name, ops], newtuns, new},
    If[MatchQ[link, $Failed],
      TunnelCreateError[name, {ops}],
      new=TunnelCreateAssociation[name, link, ops];
      newtuns=Append[tuns, name->new];
      TunnelDataWrite[newtuns];
      new
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*TunnelInitiatorQ*)


TunnelInitiatorQ[a_Association]:=
  a["Initiator"]===$SessionID


(* ::Subsubsection::Closed:: *)
(*TunnelConnectorQ*)


TunnelConnectorQ[a_Association]:=
  a["Connector"]===$SessionID


(* ::Subsubsection::Closed:: *)
(*TunnelAttachedQ*)


TunnelAttachedQ[a_Association]:=
  TunnelInitiatorQ[a]||TunnelConnectorQ[a];


(* ::Subsubsection::Closed:: *)
(*TunnelLink*)


TunnelLink[a_Association]:=
  Which[
    TunnelInitiatorQ[a], a["Mouth"],
    TunnelConnectorQ[a], a["Tail"],
    True, None
    ]


(* ::Subsubsection::Closed:: *)
(*LinkAliveQ*)


LinkAliveQ[link_]:=
  Quiet[MatchQ[MathLink`LinkDeviceInformation[link], {__Rule}]];


(* ::Subsubsection::Closed:: *)
(*TunnelCheck*)


TunnelCheck[a_Association]:=
  Module[{link=TunnelLink[a]},
    If[link===None,
      Indeterminate,
      If[!LinkAliveQ[link],
        TunnelRemove[a];
        False,
        True
        ]
      ]
   ];


(* ::Subsubsection::Closed:: *)
(*TunnelRemove*)


TunnelRemove[name_String]:=
  Module[{tuns=KernelTunnels[]},
    If[KeyExistsQ[tuns, name],
      TunnelDataWrite[KeyDrop[tuns, name]];,
      UnknownTunnelError[name]
      ]
    ];
TunnelRemove[a_Association]:=
  TunnelRemove[a["Name"]]


(* ::Subsubsection::Closed:: *)
(*TunnelCreate*)


TunnelCreate[name_, ops:OptionsPattern[]]:=
  Module[{link, tuns=KernelTunnels[], check},
    If[!KeyExistsQ[tuns, name],
      iTunnelCreate[tuns, name, ops],
      check=TunnelCheck[tuns[name]];
      If[check===False,
        iTunnelCreate[tuns, name, ops],
        Failure["TunnelExists",
          <|
            "MessageTemplate"->
              "A tunnel with name `` is open, potentially on another kernel",
            "MessageParameters"->
              {name}
            |>
          ]
        ]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*TunnelConnectedQ*)


TunnelConnectedQ[a_]:=
  Lookup[a, "Connector"]=!=None&&
    Lookup[a, "Tail"]=!=None;


(* ::Subsubsection::Closed:: *)
(*iTunnelConnect*)


iTunnelConnect[a_]:=
  Module[
    {name=a["Name"], link, new, tuns},
    link=LinkConnect[name, a["Options"]];
    new=
      ReplacePart[
        a,
        {
          "Tail"->link,
          "Connector"->$SessionID
          }
        ];
    tuns=KernelTunnels[];
    tuns[name]=new;
    TunnelDataWrite@tuns;
    new
    ]


(* ::Subsubsection::Closed:: *)
(*TunnelConnect*)


TunnelConnect[a_Association]:=
  Which[
    TunnelInitiatorQ[a],
      InitiateConnectError[a],
    TunnelConnectedQ[a],
      AlreadyConnectedError[a],
    True,
      iTunnelConnect[a]
    ];
TunnelConnect[name_String]:=
  Module[{baseTunnel=KernelTunnels[][name]},
    If[!MissingQ@baseTunnel, 
      TunnelConnect[baseTunnel],
      UnknownTunnelError[name]
     ]
    ];


(* ::Subsubsection::Closed:: *)
(*TunnelRemoveInitiator*)


TunnelRemoveInitiator[name_]:=
  Module[{tuns=KernelTunnels[], data},
    data=tuns[name];
    If[!MissingQ@data,
      data=ReplacePart[data, {"Initiator"->None, "Mouth"->None}];
      If[data["Initiator"]===None&&data["Connector"]===None,
        TunnelRemove[name],
        tuns[name]=data;
        TunnelDataWrite@tuns
        ];
      UnknownTunnelError[name]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*TunnelRemoveConnector*)


TunnelRemoveConnector[name_]:=
  Module[{tuns=KernelTunnels[], data},
    data=tuns[name];
    If[!MissingQ@data,
      data=ReplacePart[data, {"Connector"->None, "Tail"->None}];
      If[data["Initiator"]===None&&data["Connector"]===None,
        TunnelRemove[name],
        tuns[name]=data;
        TunnelDataWrite@tuns
        ];
      UnknownTunnelError[name]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*iTunnelClose*)


iTunnelClose[a_, link_]:=
  (
    LinkClose[link];
    If[TunnelInitiatorQ[a],
      TunnelRemoveInitiator[a["Name"]],
      TunnelRemoveConnector[a["Name"]]
      ];
    );


(* ::Subsubsection::Closed:: *)
(*TunnelClose*)


TunnelClose[a_Association]:=
  Module[{l=TunnelLink[a]},
    If[l===None,
      DetachedTunnelError[a],
      iTunnelClose[a, l]
      ]
    ];
TunnelClose[name_String]:=
  Module[{baseTunnel=KernelTunnels[][name]},
    If[!MissingQ@baseTunnel, 
      TunnelClose[baseTunnel],
      UnknownTunnelError[name]
     ]
    ];


(* ::Subsubsection::Closed:: *)
(*InitiateConnectError*)


InitiateConnectError[a_]:=
  Failure["TunnelInitateConnect",
   <|
    "MessageTemplate"->
      "Can't both initiate and connect to tunnel ``",
    "MessageParameters"->
      {a}
    |>
   ]


(* ::Subsubsection::Closed:: *)
(*AlreadyConnectedError*)


AlreadyConnectedError[a_]:=
  Failure["TunnelConnected",
   <|
    "MessageTemplate"->
      "Can't connect to tunnel `` that is already connected",
    "MessageParameters"->
      {a}
    |>
   ]


(* ::Subsubsection::Closed:: *)
(*DetachedTunnelError*)


DetachedTunnelError[a_]:=
  Failure["TunnelDetached",
   <|
    "MessageTemplate"->
      "Kernel isn't attached to tunnel ``",
    "MessageParameters"->
      {a}
    |>
   ]


(* ::Subsubsection::Closed:: *)
(*UnknownTunnelError*)


UnknownTunnelError[name_]:=
  Failure["TunnelUnkown",
    <|
      "MessageTemplate"->
        "Tunnel with name `` isn't known",
      "MessageParameters"->
        {name}
      |>
    ]


(* ::Subsubsection::Closed:: *)
(*DeadTunnelError*)


DeadTunnelError[a_]:=
  Failure["TunnelDead",
    <|
      "MessageTemplate"->
        "Tunnel `` is dead",
      "MessageParameters"->
        {a}
      |>
    ]


(* ::Subsubsection::Closed:: *)
(*TunnelWrite*)


TunnelWrite[a_Association, expr__]:=
  Module[{link=TunnelLink[a]},
    If[link===None,
      DetachedTunnelError[a],
      If[TunnelCheck[a]===False,
        DeadTunnelError[a],
        {Function[
          Null,
          LinkWrite[link, Unevaluated[#]],
          HoldAllComplete
          ]/@Hold[expr]}//ReleaseHold;
        ]
      ]
    ];
TunnelWrite[name_String, expr__]:=
  Module[{baseTunnel=KernelTunnels[][name]},
    If[!MissingQ@baseTunnel, 
      TunnelWrite[baseTunnel, expr],
      UnknownTunnelError[name]
     ]
    ];
TunnelWrite~SetAttributes~HoldRest;


(* ::Subsubsection::Closed:: *)
(*SetTunnelReadHandler*)


SetTunnelReadHandler[a_Association, function_]:=
  Which[
    TunnelInitiatorQ[a],
      TunnelDataWrite@
        Append[KernelTunnels[], a["Name"]->Append[a, "InitiatorReadHandler"->function]],
    TunnelConnectorQ[a],
      TunnelDataWrite@
        Append[KernelTunnels[], a["Name"]->Append[a, "ConnectorReadHandler"->function]],
    True,
      DetachedTunnelError[a]
    ];
SetTunnelReadHandler[name_String, function_]:=
  Module[{tun=KernelTunnels[][name]},
    If[MissingQ@tun,
      UnknownTunnelError[name],
      SetTunnelReadHandler[tun, function]
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*RemoveTunnelReadHandler*)


RemoveTunnelReadHandler[a_Association]:=
  Which[
    TunnelInitiatorQ[a],
      TunnelDataWrite@
        Append[KernelTunnels[], a["Name"]->KeyDrop[a, "InitiatorReadHandler"]],
    TunnelConnectorQ[a],
      TunnelDataWrite@
        Append[KernelTunnels[], a["Name"]->KeyDrop[a, "ConnectorReadHandler"]],
    True,
      DetachedTunnelError[a]
    ];
RemoveTunnelReadHandler[name_String]:=
  Module[{tun=KernelTunnels[][name]},
    If[MissingQ@tun,
      UnknownTunnelError[name],
      RemoveTunnelReadHandler[tun]
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*$DefaultTunnelReadHandler*)


TunnelReadHandler[tunnel_, expr_]:=
  Replace[expr, 
    EvaluatePacket[e_]:>TunnelWrite[tunnel, e]
    ];
$DefaultTunnelReadHandler=TunnelReadHandler;


(* ::Subsubsection::Closed:: *)
(*GetTunnelReadHandler*)


GetTunnelReadHandler[a_Association]:=
  Which[
    TunnelInitiatorQ[a],
      Lookup[a, "InitiatorReadHandler", $DefaultTunnelReadHandler],
    TunnelConnectorQ[a],
      Lookup[a, "ConnectorReadHandler", $DefaultTunnelReadHandler],
    True,
      DetachedTunnelError[a]
    ]


(* ::Subsubsection::Closed:: *)
(*TunnelRead*)


TunnelRead[a_Association, head_:Identity]:=
  Module[{link=TunnelLink[a], handler},
    If[link===None,
      DetachedTunnelError[a],
      If[TunnelCheck[a]===False,
        DeadTunnelError[a],
        handler=GetTunnelReadHandler[a];
        Replace[
          Reap[
            While[LinkReadyQ[link], 
              Sow@handler[a, LinkRead[link, head]]
              ]
            ][[2]],
          {{e_}:>e}
          ]
        ]
      ]
    ];
TunnelRead[name_String, head_:Identity]:=
  Module[{baseTunnel=KernelTunnels[][name]},
    If[!MissingQ@baseTunnel, 
      TunnelRead[baseTunnel, head],
      UnknownTunnelError[name]
     ]
    ];


(* ::Subsubsection:: *)
(*End*)


End[]


(* ::Subsection:: *)
(*EndPackage*)


EndPackage[]
