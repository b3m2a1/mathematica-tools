(* ::Package:: *)

BeginPackage["SubprocessKernel`"]


(* ::Text:: *)
(*In the eventual paclet there needs to be a way for this to manage multiple links at once (maybe even to multiple FEs) and basically act as a packet broker.*)


$SubprocessFrontEnd::usage="The front end link for the kernel";
$SubprocessKernel::usage="The kernel link for the kernel";
$SubprocessKernelName::usage="The basic kernel name for the subprocess";
$SubprocessKernelExtension::usage="The extension for the kernel name";
OpenSubprocessNotebook::usage="Opens an a subprocess notebook";
StartSubprocessREPL::usage="Starts the subprocess REPL";
$SubprocessREPLSettings::usage="Settings for the subprocess REPL";


Begin["`Private`"]


startSubprocessFrontEnd[]:=
  If[Quiet[$FrontEnd[[1]]=!=$SubprocessFrontEnd||!BooleanQ@LinkReadyQ[$SubprocessFrontEnd]],
    Quiet@LinkClose[$SubprocessFrontEnd];
    UsingFrontEnd;
    launchFECommand=
      System`UseFrontEndDump`Command["Server"->False]<>
        Map[{" ",#1}&, 
          Join[System`UseFrontEndDump`Flags["Server"->False],{"-wstp"}]
          ];
    
    $SubprocessFrontEnd=LinkLaunch[launchFECommand];
  
    MathLink`SetFrontEnd[$SubprocessFrontEnd]
    ];


If[!StringQ@$SubprocessKernelName, 
  $SubprocessKernelName="Subprocess"];
If[!StringQ@$SubprocessKernelExtension, 
  $SubprocessKernelExtension="+"<>IntegerString[RandomInteger[50000], 16, 4]];


startSubprocessKernel[]:= 
  If[!MatchQ[$SubprocessKernel, _LinkObject?(Quiet[BooleanQ@LinkReadyQ[#]]&)],
    $subprocessKernelName=
      $SubprocessKernelName<>$SubprocessKernelExtension;
    $SubprocessKernel=Quiet@LinkCreate[$subprocessKernelName, "LinkMode"->Listen],
    $SubprocessKernel
    ];


configureSubprocessFrontEnd[]:=
  (
    $cachedKernelConfiguration=
      CurrentValue[$FrontEndSession, {EvaluatorNames, $SubprocessKernelName}];
    CurrentValue[$FrontEndSession, {EvaluatorNames, $SubprocessKernelName}]=
     {
      "AutoStartOnLaunch"->False,
      "MLOpenArguments"->
        TemplateApply[
          "-LinkMode Connect -LinkProtocol SharedMemory -LinkName \"``\" -LinkOptions 256",
          $subprocessKernelName
          ]
      };
    )


$SubprocessREPLSettings=
  <|
    "InitializationMessage"->"Starting kernel...",
    "ProcessPacket"->processPacket,
    "ProcessInput"->Function[{text}, processPacket@HoldComplete[EnterTextPacket[text]]],
    "LinkWrite"->
      Function[{link, response, packet},
        (*Print@{packet, response};*)
        LinkWrite[link, #]&/@response
        ],
    "STDOUTWrite"->
      Function[
        {output, input},
        Print[">> ------ Output: `` ------ >>"~TemplateApply~$Line];
        Print[ToString[#, InputForm]<>"\n"]&/@output;
        Print["<< ------ Output: `` ------ <<"~TemplateApply~$Line]
        ],
    "Links"->
      Automatic,
    "RouteOutput"->
      <|
        "STDIN"->"STDOUT",
        "Link"->Automatic
        |>,
    "LogPackets"->False,
    "PollTime"->.1,
    "Blocking"->False
    |>;


evaluateThings[e_]:=
  Reap[
    GeneralUtilities`WithMessageHandler[
      CheckAbort[ReleaseHold@e, $Aborted],
      With[
        {
          sym=Extract[#, {2, "MessageTemplate", 1}, Hold],
          name=Extract[#, {2, "MessageTemplate", 2}],
          params=Sequence@@Extract[#, {2, "MessageParameters"}]
          },
        Sow[MessagePacket[sym, name]/.Hold[s_]:>s];
        Sow[TextPacket@
          Replace[
            Internal`MessageButtonHandler[
              MessageName[Evaluate@ReleaseHold[sym], name],
              Hold[MessageName[sym, name]]/.Hold[s_Symbol]:>s,
              Hold[Message[MessageName[sym, name], params]]/.Hold[s_Symbol]:>s
              ],
            $Failed:>
              ToString[HoldForm[MessageName[sym, name]]/.Hold[s_Symbol]:>s]
                <>" "<>
              ToString@
                StringForm[
                  Replace[
                    Extract[#, {2, "MessageTemplate"}], 
                    Except[_String]:>MessageName[General, name]
                    ],
                  params
                  ]
            ]
          ]
        ]&
      ]
    ]


logEvent[packet_]:=
  With[
    {
      log=FileNameJoin@{$TemporaryDirectory, "subproc_repl_log.txt"}
      },
    If[!FileExistsQ@log, CreateFile@log];
    OpenAppend[log];
    WriteString[log, 
      StringRepeat["\n", 2]<>
        StringRepeat[" ", 5]<>
        StringRepeat["-", 25]<>
        StringRepeat["\n", 2]
      ];
    PutAppend[packet, log];
    Quiet@Close@log;
    ];
logEcho[event_]:=
  (logEvent[event];event)


exitSubprocessREPL[]:=
  (
    $ParentLink=Null;
    Quiet[LinkClose@$SubprocessKernel];
    Quiet[LinkClose@$SubprocessFrontEnd];
    )


doAsync:=
  If[$VersionNumber<11.2,
    Function[Null, RunScheduledTask[#, {.0001}], HoldFirst],
    SessionSubmit
    ]


manageKernelConnections[]:=
  Module[
    {
      conns=$SubprocessREPLSettings["Links"],
      lia=$SubprocessLinks,
      deadLinks,
      newLinks,
      newStdin
      },
    deadLinks=Complement[Keys@lia, conns];
    newLinks=Complement[conns, Keys@lia];
    Switch[Head[#],
      LinkObject, LinkClose[#], 
      OutputStream|InputStream, Close
      ]&/@KeyTake[lia, deadLinks];
    KeyDropFrom[lia, deadLinks];
    newStdin=MemberQ[newLinks, "STDIN"];
    If[newStdin, newLinks=DeleteCases[newLinks, "STDIN"]];
    newLinks=AssociationMap[LinkConnect, newLinks];
    If[newStdin, AppendTo[newLinks, "STDIN"->OpenRead["!cat", BinaryFormat->True]]];
    $SubprocessLinks=Join[lia, newLinks];
    $SubprocessLinks
    ]


getNextPacket[linkName_, link_LinkObject, timeout_]:=
  If[TrueQ@Quiet@TimeConstrained[LinkReadyQ@link, .01],
    TimeConstrained[
      LinkRead[link, HoldComplete],
      timeout
      ],
    None
    ];
getNextPacket[linkName_, link_InputStream, timeout_]:=
  TimeConstrained[
    If[StringQ@#&&StringLength@#>0,
      EnterTextPacket[#],
      None
      ]&@ReadString[link, EndOfBuffer],
    timeout
    ];


standardEvaluationProcedure[expr_, makePacket_]:=
  Module[{evRes=evaluateThings[expr], e, errs},
    $incrementLine=True;
    e=evRes[[1]];
    errs=evRes[[2]];
    {
      errs,
      Switch[e, 
        Null,
          {},
        _ErrorBox,
          {
            OutputNamePacket["Out[``]="~TemplateApply~$Line],
            makePacket@e
            },
        _,
          {
            OutputNamePacket["Out[``]="~TemplateApply~$Line],
            makePacket@e
            }
        ]
     }
    ]


processPacket//Clear
processPacket[p:HoldComplete@EvaluatePacket[res_]]:=
  If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEcho, Identity][ 
    If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEvent[p]];
    ReturnPacket[res]
    ];
processPacket[p:HoldComplete@EnterExpressionPacket[expr_]]:=
  If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEcho, Identity][
    If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEvent[p]];
    If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEcho, Identity]@
      standardEvaluationProcedure[
        expr,
        ReturnExpressionPacket[BoxData@If[Head@#===ErrorBox, #, ToBoxes@#]]&
        ]
    ];
processPacket[p:HoldComplete@EnterTextPacket[expr_]]:=
  If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEcho, Identity][
    If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEvent[p]];
    standardEvaluationProcedure[
      MakeExpression[expr, StandardForm],
      If[Head@#===ErrorBox,
        {},
        ReturnTextPacket[ToString@#]
        ]&
      ]
    ];
processPacket[p:HoldComplete@InputPacket[expr_]]:=
  If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEcho, Identity][
    If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEvent[p]];
    $incrementLine=True;
    ReturnTextPacket[expr]
    ];
processPacket[p:HoldComplete[packet_]]:=
  If[TrueQ@$SubprocessREPLSettings["LogPackets"], logEcho, Identity][
    logEvent[p];
    ReturnExpressionPacket[BoxData@ToBoxes@packet]
    ];
processPacket[___]:=None;


packetWrite[linkName_, response_, packet_, dest_, link_, linkWrite_, stdoutWrite_]:=
 If[response=!=None,
   If[dest==="STDOUT",
      Block[{$ParentLink=Null},
        stdoutWrite[
          Flatten[{response}], 
          packet
          ]
         ],
     With[
       {
         targetLink=
           Which[
             KeyExistsQ[$SubprocessLinks, dest],
               $SubprocessLinks[dest],
             dest==="FrontEnd",
               $SubprocessLinks[FrontEnd],
             True,
               link
             ]
         },
         TimeConstrained[
          linkWrite[
            targetLink, 
            Flatten[{response}], 
            packet
            ],
          .1
          ];
      If[Head@response=!=ReturnPacket&&linkName=!=PreemptiveLink,
        TimeConstrained[
          LinkWrite[
            targetLink, 
            InputNamePacket["In[``]:="~TemplateApply~$Line]
            ],
          .1
          ]
        ]
      ]
    ]
  ];
packetWrite[
  linkName_,
  response_,
  packet_
  ]:=
  packetWrite[
    linkName,
    response,
    packet,
    Lookup[
      $SubprocessREPLSettings["RouteOutput"],
      linkName,
      Lookup[
        $SubprocessREPLSettings["RouteOutput"], 
        "Link",
        Automatic
        ]
      ],
    $SubprocessREPLSettings["LinkWrite"],
    $SubprocessREPLSettings["STDOUTWrite"]
    ];


handleLinkPacketResponse//Clear;
handleLinkPacketResponse[linkName_, None, preemptive:_:False]:=
  None;
handleLinkPacketResponse[linkName_, packet_, preemptive:True]:=
  Block[{$incrementLine},
    With[{processPacket=$SubprocessREPLSettings["ProcessPacket"]},
      If[!AssociationQ@$SubprocessEvaluationStack, $SubprocessEvaluationStack=<||>];
      If[KeyFreeQ[$SubprocessEvaluationStack, linkName], 
        $SubprocessEvaluationStack[linkName]=<|"Evaluation"->1|>
        ];
      With[{n=$SubprocessEvaluationStack[linkName, "Evaluation"]},
        $SubprocessEvaluationStack[linkName, n]=packet;
        doAsync[
          $SubprocessEvaluationStack[linkName, n]=
            {
              processPacket@$SubprocessEvaluationStack[linkName, n], 
              $SubprocessEvaluationStack[linkName, n]
              };
          While[KeyExistsQ[$SubprocessEvaluationStack[linkName], n-1],
            Pause[.0001];
            ];
          packetWrite[
            linkName,
            $SubprocessEvaluationStack[linkName, n][[1]], 
            $SubprocessEvaluationStack[linkName, n][[2]]
            ];
          $SubprocessEvaluationStack[linkName, n]=.
          ]
        ];
      ]
    ];
handleLinkPacketResponse[linkName_, packet_, preemptive:False:False]:=
  With[
    {
      processPacket=
        If[linkName==="STDIN",
          $SubprocessREPLSettings["ProcessInput"],
          $SubprocessREPLSettings["ProcessPacket"]
          ]
      },
    If[linkName===PreemptiveLink,
      handleLinkPacketResponse[linkName, packet, True],
      With[{response=processPacket@packet},
        If[response===ReturnPacket[Quit],
          exitSubprocessREPL[];
          Quiet[
            Break[];
            If[$VersionNumber<11.2, 
              RemoveScheduledTask@$ScheduledTask, 
              TaskRemove@$CurrentTask
              ]
            ]
          ];
        packetWrite[
          linkName,
          response,
          packet
          ]
        ]
      ]
    ]


subprocessBlockingREPL[]:=
  Module[
    {
      links,
      packets,
      responses,
      pollTime:=$SubprocessREPLSettings["PollTime"]
      },
      
    While[True,
      links=
        Join[
          manageKernelConnections[], 
          <|
            FrontEnd->$SubprocessKernel, 
            PreemptiveLink->MathLink`$PreemptiveLink
            |>
          ];
      
      packets=MapIndexed[getNextPacket[#2[[1, 1]], #, .1]&, links];
      responses=MapIndexed[handleLinkPacketResponse[#2[[1, 1]], #]&, packets];
        
      If[TrueQ@$incrementLine, $Line++];
      $incrementLine=False;
      
      If[pollTime>0, Pause[pollTime]]
     ]
   ];


subprocessNonBlockingREPL[]:=
  Module[
    {
      links,
      packets,
      responses,
      pollTime:=$SubprocessREPLSettings["PollTime"]
      },
    If[$VersionNumber<11.2, 
      RunScheduledTask, 
      Function[Null, SessionSubmit[ScheduledTask[##]], HoldFirst]
      ][
        links=manageKernelConnections[];
        
        packets=MapIndexed[getNextPacket[#2[[1, 1]], #, .1]&, links];
        responses=MapIndexed[handleLinkPacketResponse[#2[[1, 1]], #]&, packets];
  
        If[TrueQ@$incrementLine, $Line++];
        $incrementLine=False;,
       
        pollTime
        ]
    ]


StartSubprocessREPL[]:=
  With[
    {
      blocking=
        $SubprocessREPLSettings["Blocking"]===True
      },
    $SubprocessLinks=<||>;
    $SubprocessREPLSettings["Links"]=
      Replace[$SubprocessREPLSettings["Links"],
        {
          s_String:>{s},
          Except[_List]:>{"STDIN"}
          }
        ];
    $Line=1;
    startSubprocessFrontEnd[];
    startSubprocessKernel[];
    FrontEndExecute@FrontEnd`EvaluatorStart[$SubprocessKernelName];
    If[blocking,
      TimeConstrained[
        LinkWrite[$SubprocessKernel, InputNamePacket["In[1]:="]],
        .1
        ]
      ];
    If[StringQ@$SubprocessREPLSettings["InitializationMessage"],
      Print@$SubprocessREPLSettings["InitializationMessage"]
      ];
    
    $ParentLink=$SubprocessKernel;
    
    If[blocking,
      subprocessBlockingREPL[];,
      subprocessNonBlockingREPL[]
     ];
    ]


OpenSubprocessNotebook[retry:True|False:True]:=
 Module[{res},
   startSubprocessFrontEnd[];
   startSubprocessKernel[];
   res=CreateDocument[{}, Evaluator->$SubprocessKernelName];
   Which[
     res===$Failed&&retry,
       Quiet@LinkClose@$SubprocessKernel;
       $SubprocessKernel=.;
       Quiet@LinkClose@$SubprocessFrontEnd;
       $SubprocessFrontEnd=.;
       OpenSubprocessNotebook[],
     res===$Failed,
       $Failed,
     True,
       configureSubprocessFrontEnd[];
       StartSubprocessREPL[];
       res
     ]
   ]


End[]


EndPackage[]
