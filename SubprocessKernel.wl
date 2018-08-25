(* ::Package:: *)

BeginPackage["SubprocessKernel`"]


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
    "RouteOutput"->
      <|
        "STDIN"->"STDOUT",
        "FrontEnd"->"Link"
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
    ]


exitSubprocessREPL[]:=
  (
    $ParentLink=Null;
    Quiet[LinkClose@$SubprocessKernel];
    Quiet[LinkClose@$SubprocessFrontEnd];
    )


packetWrite[response_, packet_, src_, link_, linkWrite_, stdoutWrite_]:=
 If[response=!=None,
   If[src==="STDOUT",
    Block[{$ParentLink=Null},
      stdoutWrite[
        Flatten[{response}], 
        packet
        ]
       ];,
    TimeConstrained[
      linkWrite[
        link, 
        Flatten[{response}], 
        packet
        ],
      .1
      ]
    ]
  ]


subprocessBlockingREPL[]:=
  Module[
    {
      
      kernelLink=$SubprocessKernel,
      packet,
      response,
      
      stdin=OpenRead["!cat", BinaryFormat->True],
      input,
      output,
      
      preemptiveLink=MathLink`$PreemptiveLink,
      ppacket,
      pevalStack=<||>,
      pevalNum=1,
      
      wroteToLink,
      
      linkWrite:=$SubprocessREPLSettings["LinkWrite"],
      processPacket:=$SubprocessREPLSettings["ProcessPacket"],
      processInput:=$SubprocessREPLSettings["ProcessInput"],
      stdoutWrite:=$SubprocessREPLSettings["STDOUTWrite"],
      stdinOutput:=$SubprocessREPLSettings["RouteOutput", "STDIN"],
      feOutput:=$SubprocessREPLSettings["RouteOutput", "FrontEnd"],
      pollTime:=$SubprocessREPLSettings["PollTime"]
      },
    (*logEvent@"Entering loop";*)
    While[True
      (*Quiet@TrueQ[LinkConnectedQ@$SubprocessFrontEnd]&&
        Quiet@TrueQ[LinkConnectedQ@$SubprocessKernel]*),
      output=None;
      response=None;
      input=ReadString[stdin, EndOfBuffer];
       
      (*logEvent["Link Ready: "<>ToString@LinkReadyQ@kernelLink];*)
      
      If[Quiet@LinkReadyQ@preemptiveLink,
        ppacket=TimeConstrained[LinkRead[preemptiveLink, HoldComplete], .5];
        With[{n=pevalNum},
          pevalStack[n]=ppacket;
          If[$VersionNumber<11.2,
            Function[Null, RunScheduledTask[#, {.0001}], HoldFirst],
            SessionSubmit
            ][
            pevalStack[n]={processPacket@pevalStack[n], pevalStack[n]};
            While[KeyExistsQ[pevalStack, n-1],
              Pause[.0001];
              ];
            packetWrite[
              pevalStack[n][[1]], pevalStack[n][[2]], "Link", preemptiveLink, 
              linkWrite, stdoutWrite
              ];
            pevalStack[n]=.
            ]
          ]
        ];
        
      If[StringQ@input&&StringLength@input>0,
        output=processInput@input;
        ];
      If[Quiet@LinkReadyQ@kernelLink,
        (*logEvent["Link Read"];*)
        packet=TimeConstrained[LinkRead[kernelLink, HoldComplete], .5];
        response=processPacket[packet];
        ];
        
      If[
        response===ReturnPacket[Quit]||
        output===ReturnPacket[Quit],
        exitSubprocessREPL[];
        Break[]
        ];
      
      packetWrite[
        output, input, stdinOutput, kernelLink, 
        Function[wroteToLink=True;linkWrite[##]], stdoutWrite
        ];
      
      packetWrite[
        response, packet, feOutput, kernelLink, 
        Function[wroteToLink=True;linkWrite[##]], stdoutWrite
        ];
        
      If[TrueQ@$incrementLine, $Line++];
      $incrementLine=False;
       
      If[Head@response=!=ReturnPacket&&wroteToLink,
        TimeConstrained[
          LinkWrite[
            $SubprocessKernel, 
            InputNamePacket["In[``]:="~TemplateApply~$Line]
            ],
          .1
          ]
        ];
      
      If[pollTime>0,
        Pause[pollTime]
        ]
     ]
   ];


subprocessNonBlockingREPL[]:=
  Module[
    {
      stdin=OpenRead["!cat", BinaryFormat->True],
      input,
      output,
      wroteToLink,
      linkWrite:=$SubprocessREPLSettings["LinkWrite"],
      processPacket:=$SubprocessREPLSettings["ProcessPacket"],
      processInput:=$SubprocessREPLSettings["ProcessInput"],
      stdoutWrite:=$SubprocessREPLSettings["STDOUTWrite"],
      stdinOutput:=$SubprocessREPLSettings["RouteOutput", "STDIN"],
      feOutput:=$SubprocessREPLSettings["RouteOutput", "FrontEnd"],
      pollTime:=$SubprocessREPLSettings["PollTime"]
      },
    If[$VersionNumber<11.2, 
      RunScheduledTask, 
      Function[Null, SessionSubmit[ScheduledTask[##]], HoldFirst]
      ][
      
        input=ReadString[stdin, EndOfBuffer];
        output=None;
        
        If[StringQ@input&&StringLength@input>0,
          output=$SubprocessREPLSettings["ProcessInput"]@input;
          ];
          
        If[output===ReturnPacket[Quit],
          exitSubprocessREPL[];
          If[$VersionNumber<11.2, 
            RemoveScheduledTask@$ScheduledTask, 
            TaskRemove@$CurrentTask
            ]
          ];
          
        packetWrite[
          output, input, stdinOutput, $SubprocessKernel, 
          Function[wroteToLink=True;linkWrite[##]], stdoutWrite
          ];
  
        If[TrueQ@$incrementLine, $Line++];
        If[wroteToLink&&Head@output=!=ReturnPacket,
         TimeConstrained[
           LinkWrite[
             $SubprocessKernel, 
             InputNamePacket["In[``]:="~TemplateApply~$Line]
             ],
           .1
           ]
         ];
       $incrementLine=False;,
       
       pollTime
       ]
    ]


StartSubprocessREPL[]:=
  With[
    {
      blocking=$SubprocessREPLSettings["Blocking"]=!=False
      },
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
      logEvent["REPL Started"];
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
