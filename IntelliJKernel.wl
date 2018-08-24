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


processPacket//Clear
processPacket[HoldComplete@EvaluatePacket[res_]]:=
  ($dontIncrementLine=True;ReturnPacket[res]);
processPacket[HoldComplete@EnterExpressionPacket[expr_]]:=
  With[{e=ReleaseHold@expr},
    If[e=!=Null,
      {
        OutputNamePacket["Out[``]="~TemplateApply~$Line++],
        ReturnExpressionPacket[BoxData@ToBoxes@e]
        },
      {}
      ]
    ];
processPacket[HoldComplete@EnterTextPacket[expr_]]:=
  With[{e=ReleaseHold@expr},
    If[e=!=Null,
      {
        OutputNamePacket["Out[``]="~TemplateApply~$Line++],
        ReturnTextPacket[ToString[ToExpression@e, InputForm]]
        },
      {}
      ]
    ];
processPacket[HoldComplete@InputPacket[expr_]]:=
  ReturnTextPacket[expr];
processPacket[HoldComplete[packet_]]:=
  (Print@packet;)


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
      AbortProtect@
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
            If[!TrueQ@$dontIncrementLine,
              LinkWrite[
                $IntelliJKernel, 
                InputNamePacket["In[``]:="~TemplateApply~++$Line]
                ]
              ];
            $dontIncrementLine=False
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
