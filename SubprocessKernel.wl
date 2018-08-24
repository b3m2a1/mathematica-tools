(* ::Package:: *)

BeginPackage["SubprocessKernel`"]


$SubprocessFrontEnd::usage="The front end link for the kernel";
$SubprocessKernel::usage="The kernel link for the kernel";
$SubprocessKernelName::usage="The basic kernel name for the subprocess";
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


If[!StringQ@$SubprocessKernelName, $SubprocessKernelName="Subprocess"];


startSubprocessKernel[]:= 
  If[!MatchQ[$SubprocessKernel, _LinkObject?(Quiet[BooleanQ@LinkReadyQ[#]]&)],
    $subprocessKernelName=
      $SubprocessKernelName<>"+"<>IntegerString[RandomInteger[50000], 16, 4];
    $SubprocessKernel=Quiet@LinkCreate[$subprocessKernelName, "LinkMode"->Listen],
    $SubprocessKernel
    ];


configureSubprocessFrontEnd[]:=
  (
    CurrentValue[$FrontEnd, {EvaluatorNames, $SubprocessKernelName}]=
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
        "STDIN"->"FrontEnd",
        "FrontEnd"->"FrontEnd"
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


logPacket[packet_]:=
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
    ]


processPacket//Clear
processPacket[p:HoldComplete@EvaluatePacket[res_]]:=
  ( 
    If[$SubprocessREPLSettings["LogPackets"], logPacket[p]];
    ReturnPacket[res]
    );
processPacket[p:HoldComplete@EnterExpressionPacket[expr_]]:=
  (
    If[$SubprocessREPLSettings["LogPackets"], logPacket[p]];
    standardEvaluationProcedure[
      expr,
      ReturnExpressionPacket[BoxData@If[Head@#===ErrorBox, #, ToBoxes@#]]&
      ]
    );
processPacket[p:HoldComplete@EnterTextPacket[expr_]]:=
  (
    If[$SubprocessREPLSettings["LogPackets"], logPacket[p]];
    standardEvaluationProcedure[
      MakeExpression[expr, StandardForm],
      If[Head@#===ErrorBox,
        {},
        ReturnTextPacket[ToString@#]
        ]&
      ]
     );
processPacket[p:HoldComplete@InputPacket[expr_]]:=
  (
    If[$SubprocessREPLSettings["LogPackets"], logPacket[p]];
    $incrementLine=True;
    ReturnTextPacket[expr]
    );
processPacket[p:HoldComplete[packet_]]:=
  (
    logPacket[p];
    ReturnExpressionPacket[BoxData@ToBoxes@packet]
    )


exitSubprocessREPL[]:=
  (
    $ParentLink=Null;
    Quiet[LinkClose@$SubprocessKernel];
    Quiet[LinkClose@$SubprocessFrontEnd];
    )


subprocessBlockingREPL[]:=
  Module[
    {
      packet,
      response,
      stdin=OpenRead["!cat", BinaryFormat->True],
      input,
      output
      },
    While[True
      (*Quiet@TrueQ[LinkConnectedQ@$SubprocessFrontEnd]&&
        Quiet@TrueQ[LinkConnectedQ@$SubprocessKernel]*),
      output=None;
      response=None;
      input=ReadString[stdin, EndOfBuffer];
      If[Quiet@LinkReadyQ@$SubprocessKernel,
        packet=LinkRead[$SubprocessKernel, HoldComplete];
        (*If[packet===$Failed, Break[]];*)
        response=$SubprocessREPLSettings["ProcessPacket"][packet];
        ];
      If[StringQ@input&&StringLength@input>0,
        output=$SubprocessREPLSettings["ProcessInput"]@input;
        ];
      If[
        response===ReturnPacket[Quit]||
        output===ReturnPacket[Quit],
        exitSubprocessREPL[];
        Break[]
        ];
      If[output=!=None,
        If[$SubprocessREPLSettings["RouteOutput"]["STDIN"]==="STDOUT",
          Block[{$ParentLink=Null},
            $SubprocessREPLSettings["STDOUTWrite"][
              Flatten[{output}], 
              input
              ]
            ];,
          $SubprocessREPLSettings["LinkWrite"][
            $SubprocessKernel, 
            Flatten[{output}], 
            input
            ];
          ]
        ];
      If[response=!=None,
        If[$SubprocessREPLSettings["RouteOutput"]["FrontEnd"]==="STDOUT",
          Block[{$ParentLink=Null},
            $SubprocessREPLSettings["STDOUTWrite"][
              Flatten[{response}], 
              packet
              ]
             ];,
          $SubprocessREPLSettings["LinkWrite"][
            $SubprocessKernel, 
            Flatten[{response}], 
            packet
            ];
          ]
        ];
       If[TrueQ@$incrementLine, $Line++];
       If[Head@response=!=ReturnPacket,
        LinkWrite[
          $SubprocessKernel, 
          InputNamePacket["In[``]:="~TemplateApply~$Line]
          ]
        ];
      $incrementLine=False;
      
      If[$SubprocessREPLSettings["PollTime"]>0,
        Pause[$SubprocessREPLSettings["PollTime"]]
        ]
     ]
   ];


subprocessNonBlockingREPL[]:=
  Module[
    {
      stdin=OpenRead["!cat", BinaryFormat->True],
      input,
      output
      },
    If[$VersionNumber<11.2, RunScheduledTask, SessionSubmit]@
      ScheduledTask[
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
        If[output=!=None,
          If[$SubprocessREPLSettings["RouteOutput"]["STDIN"]==="STDOUT",
            Block[{$ParentLink=Null},
              $SubprocessREPLSettings["STDOUTWrite"][
                Flatten[{output}], 
                input
                ]
              ];,
            $SubprocessREPLSettings["LinkWrite"][
              $SubprocessKernel, 
              Flatten[{output}], 
              input
              ];
            ]
          ];
        If[TrueQ@$incrementLine, $Line++];
        If[Head@response=!=ReturnPacket,
         LinkWrite[
           $SubprocessKernel, 
           InputNamePacket["In[``]:="~TemplateApply~$Line]
           ]
         ];
         $incrementLine=False;,
         $SubprocessREPLSettings["PollTime"]
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
      LinkWrite[$SubprocessKernel, InputNamePacket["In[1]:="]]
      ];
    $ParentLink=$SubprocessKernel;
    If[blocking,
      subprocessBlockingREPL[],
      subprocessNonBlockingREPL[]
     ]
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
