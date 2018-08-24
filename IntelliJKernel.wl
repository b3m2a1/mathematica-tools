(* ::Package:: *)

BeginPackage["IntelliJ`"]


$IntelliJFrontEnd::usage="The front end link for IntelliJ";
$IntelliJKernel::usage="The kernel link for IntelliJ";
OpenIntelliJNotebook::usage="Opens an IntelliJ Notebook";
StartIntelliJREPL::usage="Starts the IntelliJ REPL";
$IntelliJREPLSettings::usage="Settings for the IntelliJ REPL";


Begin["`Private`"]


startIntelliJFrontEnd[]:=
  If[Quiet[$FrontEnd[[1]]=!=$IntelliJFrontEnd||!BooleanQ@LinkReadyQ[$IntelliJFrontEnd]],
    Quiet@LinkClose[$IntelliJFrontEnd];
    UsingFrontEnd;
    launchFECommand=
      System`UseFrontEndDump`Command["Server"->False]<>
        Map[{" ",#1}&, 
          Join[System`UseFrontEndDump`Flags["Server"->False],{"-wstp"}]
          ];
    
    $IntelliJFrontEnd=LinkLaunch[launchFECommand];
  
    MathLink`SetFrontEnd[$IntelliJFrontEnd]
    ];


startIntelliJKernel[]:= 
  If[!MatchQ[$IntelliJKernel, _LinkObject?(Quiet[BooleanQ@LinkReadyQ[#]]&)],
    $IntelliJKernelName=
      "IntelliJKernel"<>"+"<>IntegerString[RandomInteger[50000], 16, 4];
    $IntelliJKernel=Quiet@LinkCreate[$IntelliJKernelName, "LinkMode"->Listen],
    $IntelliJKernel
    ];


configureIntelliJFrontEnd[]:=
  (
    CurrentValue[$FrontEnd, {EvaluatorNames, "IntelliJ"}]=
     {
      "AutoStartOnLaunch"->False,
      "MLOpenArguments"->
        TemplateApply[
          "-LinkMode Connect -LinkProtocol SharedMemory -LinkName \"``\" -LinkOptions 256",
          $IntelliJKernelName
          ]
      };
    )


$IntelliJREPLSettings=
  <|
    "ProcessPacket"->processPacket,
    "LinkWrite"->
      Function[{link, response, packet},
        (*Print@{packet, response};*)
        LinkWrite[link, #]&/@response
        ],
    "PollTime"->.1
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


processPacket//Clear
processPacket[HoldComplete@EvaluatePacket[res_]]:=
  (ReturnPacket[res]);
processPacket[HoldComplete@EnterExpressionPacket[expr_]]:=
  standardEvaluationProcedure[
    expr,
    ReturnExpressionPacket[BoxData@If[Head@#===ErrorBox, #, ToBoxes@#]]&
    ];
processPacket[HoldComplete@EnterTextPacket[expr_]]:=
  standardEvaluationProcedure[
    expr,
    If[Head@#===ErrorBox,
      {},
      ReturnTextPacket[ToString@#]
      ]&
    ]
processPacket[HoldComplete@InputPacket[expr_]]:=
  (
    $incrementLine=True;
    ReturnTextPacket[expr]
    );
processPacket[HoldComplete[packet_]]:=
  (
    Print@packet;
    ReturnExpressionPacket[BoxData@ToBoxes@packet]
    )


StartIntelliJREPL[]:=
  Module[
    {
      packet,
      response
      },
      $Line=1;
      startIntelliJFrontEnd[];
      startIntelliJKernel[];
      FrontEndExecute@FrontEnd`EvaluatorStart["IntelliJ"];
      LinkWrite[$IntelliJKernel, InputNamePacket["In[1]:="]];
      $ParentLink=$IntelliJKernel;
      While[
        Quiet@TrueQ[LinkConnectedQ@$IntelliJFrontEnd]&&
        Quiet@TrueQ[LinkConnectedQ@$IntelliJKernel],
        If[LinkReadyQ@$IntelliJKernel,
          packet=LinkRead[$IntelliJKernel, HoldComplete];
          If[packet===$Failed, Break[]];
          response=$IntelliJREPLSettings["ProcessPacket"][packet];
          $IntelliJREPLSettings["LinkWrite"][
            $IntelliJKernel, 
            Flatten[{response}], 
            packet
            ];
          If[TrueQ@$incrementLine, $Line++];
          If[Head@response=!=ReturnPacket,
            LinkWrite[
              $IntelliJKernel, 
              InputNamePacket["In[``]:="~TemplateApply~$Line]
              ]
            ];
          $incrementLine=False
          ];
        If[$IntelliJREPLSettings["PollTime"]>0,
          Pause[$IntelliJREPLSettings["PollTime"]]
          ]
        ]
     ]


OpenIntelliJNotebook[retry:True|False:True]:=
 Module[{res},
   startIntelliJFrontEnd[];
   startIntelliJKernel[];
   res=CreateDocument[{}, Evaluator->"IntelliJ"];
   Which[
     res===$Failed&&retry,
       Quiet@LinkClose@$IntelliJKernel;
       $IntelliJKernel=.;
       Quiet@LinkClose@$IntelliJFrontEnd;
       $IntelliJFrontEnd=.;
       OpenIntelliJNotebook[],
     res===$Failed,
       $Failed,
     True,
       configureIntelliJFrontEnd[];
       StartIntelliJREPL[];
       res
     ]
   ]


End[]


EndPackage[]
