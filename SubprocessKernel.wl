(* ::Package:: *)

(* ::Section:: *)
(*SubprocessKernel*)


BeginPackage["SubprocessKernel`"]


(* ::Text:: *)
(*In the eventual paclet there needs to be a way for this to manage multiple links at once (maybe even to multiple FEs) and basically act as a packet broker.*)


$SubprocessFrontEnd::usage="The front end link for the kernel";
$SubprocessKernel::usage="The kernel link for the kernel";
$SubprocessKernelName::usage="The basic kernel name for the subprocess";
$SubprocessKernelExtension::usage="The extension for the kernel name";
$SubprocessKernelAutoLaunch::usage="Whether the kernel should launch on start-up"
OpenSubprocessNotebook::usage="Opens an a subprocess notebook";
StartSubprocessREPL::usage="Starts the subprocess REPL";
QuitSubprocessREPL::usage="Quites the subprocess REPL and closes the FE";
$SubprocessREPLSettings::usage="Settings for the subprocess REPL";
$OriginalParentLink::usage="The original parent link for the kernel";
$UseCustomPreferencesPath::usage=
  "Whether to set up the kernels via a change to the UserBaseDirectory"


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"]


$UseCustomPreferencesPath=True;


$useCustomPreferencesPath:=TrueQ[$UseCustomPreferencesPath];


(* ::Subsubsection::Closed:: *)
(*createCleanFrontEndInit*)


createCleanFrontEndInit[currInit_]:=
  With[
    {
      arg=
        TemplateApply[
          "-LinkMode Connect -LinkProtocol SharedMemory -LinkName \"``\" -LinkOptions 256",
          $SubprocessKernelName<>$SubprocessKernelExtension
          ],
      skname=$SubprocessKernelName
      },
    Module[
      {
        prefsHold
        },
      prefsHold=
        Join[
          Hold[
            Evaluator -> skname,
            EvaluatorNames ->
             {
               skname->{
                 "AutoStartOnLaunch"->False,
                 "MLOpenArguments"->arg
                 }
              }
            ],
          currInit,
          Hold[
            PreferencesPath->
              {
                FrontEnd`FileName[{$UserBaseDirectory,"FrontEnd"}],
                FrontEnd`FileName[{$BaseDirectory,"FrontEnd"}],
                FrontEnd`FileName[{$InstallationDirectory,"Configuration","FrontEnd"}],
                FrontEnd`FileName[{$InstallationDirectory,"SystemFiles","FrontEnd"}]
                }
            ]
          ];
      Thread[
        DeleteDuplicatesBy[
          Function[Null, #[[1, 1]], HoldAllComplete]
          ]@Apply[List, Hold/@prefsHold],
        Hold
        ]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*createCustomPrefs*)


createCustomPrefs[fakePrefsDir_]:=
  With[
     {
      currPrefs=
        Quiet@Block[
          {  
            SetOptions=Hold, 
            $ContextPath={"System`", "FrontEnd`Private`"}, 
            $Context="FrontEnd`Private`"
            },
          Get@FileNameJoin@{$UserBaseDirectory, "FrontEnd", "init.m"}
          ][[2;;]]
      },
    CreateDirectory[FileNameJoin@{fakePrefsDir, "FrontEnd"}];
    CopyDirectory[
      FileNameJoin@{$UserBaseDirectory, "Licensing"},
      FileNameJoin@{fakePrefsDir, "Licensing"}
      ];
    Replace[
      createCleanFrontEndInit[currPrefs],
      Hold[specs__]:>
        Put[
          Unevaluated@
            SetOptions[
              $FrontEnd,
              {
                specs
                }
              ],
          FileNameJoin@{fakePrefsDir, "FrontEnd", "init.m"}
          ]
      ];
    Import[FileNameJoin@{fakePrefsDir, "FrontEnd", "init.m"}, "Text"];
    ]


(* ::Subsubsection::Closed:: *)
(*startSubprocessKernel*)


startSubprocessKernel[]:= 
  If[!MatchQ[$SubprocessKernel, _LinkObject?(Quiet[BooleanQ@LinkReadyQ[#]]&)],
    $subprocessKernelName=
      $SubprocessKernelName<>$SubprocessKernelExtension;
    $SubprocessKernel=
      Quiet@logDebugEcho@LinkCreate[$subprocessKernelName, "LinkMode"->Listen];,
    $SubprocessKernel
    ];


(* ::Subsubsection::Closed:: *)
(*startSubprocessFrontEnd*)


(* ::Text:: *)
(*Creates a front-end link*)


startSubprocessFrontEnd[]:=
  If[Quiet[$FrontEnd[[1]]=!=logDebugEcho@$SubprocessFrontEnd||!BooleanQ@LinkReadyQ[$SubprocessFrontEnd]],
    Module[
      {
        fakePrefsDir=CreateDirectory[]
        },
      If[$useCustomPreferencesPath,
        createCustomPrefs[fakePrefsDir],
        DeleteDirectory@fakePrefsDir
        ];
      Quiet@LinkClose[$SubprocessFrontEnd];
      UsingFrontEnd;
      launchFECommand=
        System`UseFrontEndDump`Command["Server"->False]<>
          Map[
            {" ", #1}&, 
            Join[
              System`UseFrontEndDump`Flags["Server"->False],
              {"-wstp", "-nogui", 
                If[$useCustomPreferencesPath, 
                  "-preferencesDirectory '"<>fakePrefsDir<>"'",
                  Nothing
                  ]
                }
              ]
            ]//logDebugEcho;
      
      $SubprocessFrontEnd=LinkLaunch[launchFECommand];
      
      MathLink`SetFrontEnd[$SubprocessFrontEnd];
      MathLink`CreateFrontEndLinks[];
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*$SubprocessKernelName / $SubprocessKernelExtension*)


If[!StringQ@$SubprocessKernelName, 
  $SubprocessKernelName=
    "Subprocess"
  ];
If[!StringQ@$SubprocessKernelExtension, 
  $SubprocessKernelExtension=
    IntegerString[RandomInteger[50000], 16, 4]
  ];


(* ::Subsubsection::Closed:: *)
(*configureSubprocessFrontEnd*)


(* ::Text:: *)
(*This creates a front-end that will attach to our current kernel*)


configureSubprocessFrontEnd[]:=
  With[
    {
      mlOpen=
        TemplateApply[
          "-LinkMode Connect -LinkProtocol SharedMemory\
 -LinkName \"``\" -LinkOptions 256",
          $subprocessKernelName
          ]
      },
    MathLink`FrontEndBlock[
      $cachedKernelConfiguration=
        logDebugEcho@
          CurrentValue[$FrontEndSession, {EvaluatorNames, $SubprocessKernelName}];
      If[
        CurrentValue[$FrontEndSession, 
          {EvaluatorNames, "Local", "MLOpenArguments"}]=!=mlOpen//logDebugEcho,
        FrontEndExecute@FrontEnd`EvaluatorQuit["Local"];
        CurrentValue[$FrontEndSession, Evaluator] = $SubprocessKernelName;
        CurrentValue[$FrontEndSession,  
          {EvaluatorNames, $SubprocessKernelName}
          ]=
         {
          "AutoStartOnLaunch"->False,
          "MLOpenArguments"->mlOpen
          };
         CurrentValue[$FrontEndSession, EvaluatorNames]
        ],
     $SubprocessFrontEnd
     ]
   ]


(* ::Subsubsection::Closed:: *)
(* $SubprocessREPLSettings*)


(* ::Text:: *)
(*These are all the settings used by default for the REPL... most of them are somewhat subtle and don't need to be changed*)


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


(* ::Subsubsection::Closed:: *)
(*evaluateThings*)


(* ::Text:: *)
(*Custom eval layer that will generally never be used...*)


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


(* ::Subsubsection::Closed:: *)
(*logEvent*)


$debugMode=False;
$logFile=FileNameJoin@{$TemporaryDirectory, "subproc_repl_log.txt"};


logEvent[packet_]:=
  With[
    {
      log=$logFile
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
logDebugEcho[event_]:=
  If[$debugMode, logEcho[event], event];
logEcho[event_]:=
  (logEvent[event];event)


(* ::Subsubsection::Closed:: *)
(*exitSubprocessREPL*)


exitSubprocessREPL[]:=
  (
    $ParentLink=$OriginalParentLink;
    Quiet[LinkClose@$SubprocessKernel];
    Quiet[LinkClose@$SubprocessFrontEnd];
    )


(* ::Subsubsection::Closed:: *)
(*doAsync*)


doAsync:=
  If[$VersionNumber<11.2,
    Function[Null, RunScheduledTask[#, {.0001}], HoldFirst],
    SessionSubmit
    ]


(* ::Subsubsection::Closed:: *)
(*manageKernelConnections*)


(* ::Text:: *)
(*Manages the set of kernel connections specified*)


manageKernelConnections[]:=
  Module[
    {
      conns=$SubprocessREPLSettings["Links"],
      lia=$SubprocessLinks,
      deadLinks,
      newLinks,
      newStdin
      },
    If[conns===None,
      Quiet[LinkClose[#]]&/@lia;
      $SubprocessLinks={},
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
      $SubprocessLinks=Join[lia, newLinks]
      ];
    (*logDebugEcho@*)$SubprocessLinks
    ]


(* ::Subsubsection::Closed:: *)
(*getNextPacket*)


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


(* ::Subsubsection::Closed:: *)
(*standardEvaluationProcedure*)


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


(* ::Subsubsection::Closed:: *)
(*processPacket / packetWrite / etc.*)


(* ::Text:: *)
(*This is for the custom layer. Processes all the packets, and writes stuff*)


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
      Block[{$ParentLink=$OriginalParentLink},
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


(* ::Subsubsection::Closed:: *)
(*subprocessBlockingREPL*)


(* ::Text:: *)
(*Creates a REPL that will block a kernel. Not very robust.*)


subprocessBlockingREPL[]:=
  Module[
    {
      links,
      packets,
      responses,
      pollTime:=$SubprocessREPLSettings["PollTime"],
      link1Init=False,
      link2Init=False,
      n=1
      },
      
    While[True,
      
      logEcho[{1, n}];
      links=
        Join[
          manageKernelConnections[], 
          <|
            FrontEnd->$SubprocessKernel(*, 
            PreemptiveLink:>
              (
                If[!link1Init, 
                  LinkWrite[MathLink`$PreemptiveLink, InputNamePacket["In[1]:="]]
                  ];
                MathLink`$PreemptiveLink
                ),
            ServiceLink:>
              (
                If[!link2Init, 
                  LinkWrite[MathLink`$ServiceLink, InputNamePacket["In[1]:="]]
                  ];
                MathLink`$ServiceLink
                )*)
            |>
          ];
      
      logEcho[{2, n}];
      packets=MapIndexed[getNextPacket[#2[[1, 1]], #, .1]&, links];
      responses=MapIndexed[handleLinkPacketResponse[#2[[1, 1]], #]&, packets];
      logEcho[{3, n}];
      If[TrueQ@$incrementLine, $Line++];
      $incrementLine=False;
      logEcho[{4, n}];
      logEcho[pollTime];
      If[pollTime>0, Pause[pollTime]];
     ]
   ];


(* ::Subsubsection::Closed:: *)
(*subprocessNonBlockingREPL*)


(* ::Text:: *)
(*Creates a REPL that definitely does not block. Quite robust.*)


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


(* ::Subsubsection::Closed:: *)
(*StartSubprocessREPL*)


(* ::Text:: *)
(*Starts the REPL based on the settings. Killing the automatically started Local kernel is probably the hardest part of this...*)


StartSubprocessREPL[]:=
  With[
    {
      blocking=
        $SubprocessREPLSettings["Blocking"]===True
      },
    $SubprocessLinks=<||>;
    $SubprocessREPLSettings["Links"]=
      logDebugEcho@Replace[$SubprocessREPLSettings["Links"],
        {
          s_String:>{s},
          None->{},
          Except[_List]:>{"STDIN"}
          }
        ];
    $Line=1;
    startSubprocessFrontEnd[];
    startSubprocessKernel[];
    If[ListQ@CurrentValue[$FrontEndSession, {EvaluatorNames, $SubprocessKernelName}],
      FrontEndExecute@
        FrontEnd`EvaluatorStart[$SubprocessKernelName]
      ];
    If[blocking,
      TimeConstrained[
        logDebugEcho@LinkWrite[$SubprocessKernel, InputNamePacket["In[1]:="]],
        2
        ]
      ];
    If[StringQ@$SubprocessREPLSettings["InitializationMessage"],
      Print@$SubprocessREPLSettings["InitializationMessage"]
      ];
    
    $OriginalParentLink = $ParentLink;
    $ParentLink=$SubprocessKernel;
    
    If[blocking,
      subprocessBlockingREPL[];,
      subprocessNonBlockingREPL[]
     ];
    ]


(* ::Subsubsection::Closed:: *)
(*QuitSubprocessREPL*)


QuitSubprocessREPL[]:=(
  If[Head[$OriginalParentLink]===LinkObject||
      $OriginalParentLink===Null,
    $ParentLink = $OriginalParentLink
    ];
  Quiet@LinkClose@$SubprocessKernel;
  $SubprocessKernel=.;
  Quiet@LinkClose@$SubprocessFrontEnd;
  $SubprocessFrontEnd=.;
  )


(* ::Subsubsection:: *)
(*OpenSubprocessNotebook*)


(* ::Text:: *)
(*Creates a Subprocess FE and opens a notebook in it*)


OpenSubprocessNotebook[retry:True|False:True]:=
 Module[{res},
   startSubprocessFrontEnd[];
   startSubprocessKernel[];
   res=
     CreateDocument[{}, 
      {
        If[ListQ@CurrentValue[$FrontEndSession, {EvaluatorNames, $SubprocessKernelName}],
          Evaluator->$SubprocessKernelName,
          Nothing
          ]
        }
      ];
   Which[
     res===$Failed&&retry,
       QuitSubprocessREPL[];
       OpenSubprocessNotebook[],
     res===$Failed,
       $Failed,
     True,
       logDebugEcho@configureSubprocessFrontEnd[];
       StartSubprocessREPL[];
       res
     ]
   ]


(* ::Subsubsection::Closed:: *)
(*End*)


End[]


(* ::Subsection::Closed:: *)
(*EndPackage*)


EndPackage[]
