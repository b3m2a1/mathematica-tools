(* ::Package:: *)

(* ::Input:: *)
(*(*MakeIndentable["IndentCharacter"->"  "]*)*)


(* ::Input:: *)
(*(*BatchIndentationEvent["Restore"]*)*)


(* ::Input:: *)
(*(*CurrentValue[EvaluationNotebook[],{TaggingRules, "IndentCharacter"}]="\t";*)*)


(* ::Input:: *)
(*(*BatchIndentationEvent["Replace"]*)*)


(* ::Input:: *)
(*MakeIndentable["IndentCharacter"->"\t"]*)


(* ::Input:: *)
(*CurrentValue[EvaluationNotebook[], *)
(*{StyleDefinitions, "Code", CellEventActions}*)
(*]*)


(* ::Section:: *)
(*ChatSystem*)


BeginPackage["ChatSystem`"];


(*Package Declarations*)
ChatObject::usage="ChatObject[\"New\"][ops] builds a new chat";


(* ::Subsubsection::Closed:: *)
(*Private Declarations*)


AppendTo[$ContextPath, $Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
chatNotebookLine::usage="chatNotebookLine[message, meta, o]
chatNotebookLine[message, meta, o]
chatNotebookLine[messages, meta, o]
chatNotebookLine[messageData, o]";
chatNotebookPut::usage="chatNotebookPut[chatLog, nb, o]
chatNotebookPut[chat, nb, o]";
chatNotebookWrite::usage="chatNotebookWrite[messages, nb, o]
chatNotebookWrite[message, nb, o]
chatNotebookWrite[chat, o]";
chatNotebookNewCells::usage="chatNotebookNewCells[nb, o]
chatNotebookNewCells[chat, o]";
chatNotebookSendCells::usage="chatNotebookSendCells[channel, cells, o]
chatNotebookSendCells[channel, nb, o]
chatNotebookSendCells[chat]";
chatObjectNotebookDockedCell::usage="chatObjectNotebookDockedCell[chat]";
chatObjectNotebookEventActions::usage="chatObjectNotebookEventActions[chat]";
chatObjectCreateChannel::usage="chatObjectCreateChannel[chat]";
chatObjectChannelHandlerFunction::usage="chatObjectChannelHandlerFunction[chat]";
chatObjectChannelListen::usage="chatObjectChannelListen[chat]";


End[];


(* ::Subsubsection::Closed:: *)
(*Object Load*)


Quiet[
	Check[
		Get["SymbolObjects`"], 
		Get["https://raw.githubusercontent.com/b3m2a1/mathematica-tools/master/ChatSystem.wl"],
		Get::noopen
		], 
	Get::noopen
	]


$ContextPath=
	Join[
		$ContextPath, 
		{"SymbolObjects`Package`"}
		];


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"]


(*Package Implementation*)


(* ::Subsubsection::Closed:: *)
(*chatNotebookUncompressMessage*)


chatNotebookUncompressMessage[message_String]:=
	Replace[
		Quiet@Uncompress[message, HoldComplete], 
		{
			HoldComplete[Cell[b:_BoxData|_TextData|_String, e___]]:>
				Cell[b, e],
			_ -> Cell[message, "Text"]
			}
		]


(* ::Subsubsection::Closed:: *)
(*chatNotebookLine*)


(* ::Text:: *)
(*chatNotebookLine :*)
(*     Create a Cell to insert from a Message*)


Options[chatNotebookLine] =
Join[
	{
		"NameMapping" -> {},
		"NameColoring" -> {},
		DynamicUpdating -> False
		},
	Options[Cell]
	];
chatNotebookLine[
	message_Cell, 
	meta_Association, 
	o : OptionsPattern[]
	] :=
With[
	{
		c = Replace[OptionValue["NameColoring"], 
			Except[_?OptionQ] -> {}],
		m = Replace[OptionValue["NameMapping"], Except[_?OptionQ] -> {}],
		co = Options[message],
		poster = 
		StringSplit[Lookup[meta, "RequesterWolframID"], "@"][[1]]
		}, 
	With[{
			labelCell =
			Cell[
				BoxData@
				ToBoxes@
				Pane[
					Tooltip[
						Column[{
							Short[poster /. m],
							Style[DateString[Lookup[meta, "Timestamp"], "Time"],
								GrayLevel[.6]
								]
							}],
						DateString@Lookup[meta, "Timestamp"],
						TooltipDelay->0.6
						],
					ImageSize -> {100, 25},
					Alignment -> Right
					],
				"Text",
				"Message",
				Sequence @@
				Flatten@{
					(poster /. Append[c, _ :> Sequence @@ {}]),
					FontWeight -> Plain,
					FontSlant -> None,
					Background -> None,
					CellMargins -> 0,
					CellFrameMargins -> 0,
					CellFrame -> None,
					CellFrameLabels -> None,
					LineIndent -> 0,
					InitializationCell -> False
					}
				]
			},
		Join[
			Replace[message,
				{
					Cell[e:_BoxData|_TextData, s___String, r:(_Rule|_RuleDelayed)...]:>
						Cell[e, s, DynamicUpdating->OptionValue[DynamicUpdating], r]
					}
				],
			Cell[
				Sequence @@
				FilterRules[
					Flatten@
					{
						o,
						DynamicUpdating->OptionValue[DynamicUpdating],
						{
							CellDingbat->None,
							CellMargins -> 
								Replace[
									Lookup[co, CellMargins,
										Replace[message,
											{
												Cell[_, s_String, ___]:>
													CurrentValue[EvaluationNotebook[],
														{StyleDefinitions, s}
														],
												_->
													{{0, Inherited}, {Inherited, Inherited}}
												}
											]
										],
									{
										i_?NumericQ:>
											{
												{Max@{i-66, 0}, i},
												{i, i}
												},
										{
											{l_, r_},
											{b_, t_}
											}:>
											{
												{Max@{If[NumericQ@l, l, 0]-66, 0}, r},
												{b, t}
												},
										_->
											{{0, Inherited}, {Inherited, Inherited}}
										}
									],
							Editable ->
								False,
							CellFrameLabels ->
								Replace[
									Lookup[co, CellFrameLabels],
									{
										{{_, r_}, {b_, t_}} :>
											{{labelCell, r}, {b, t}},
										_ ->
											{{labelCell, None}, {None, None}}
										}
									],
							CellTags ->
								Join[Lookup[co, CellTags, {}],
									{
										"ChatCellWritten",
										StringRiffle[
											{
												meta["RequesterWolframID"],
												DateString[meta["Timestamp"], "ISODateTime"],
												meta["MessageID"]
												},
											"|"
											]
										}
									],
							TaggingRules ->
								Join[
									Lookup[co, TaggingRules, {}],
									{
										"ChatSystem" ->
											{
											 "MessageData"-> meta
												}
										}
									]
							}
						}, 
					Options@Cell
					]
				]
			]
		]
	];
chatNotebookLine[
	message_String, 
	meta_, 
	o : OptionsPattern[]
	] :=
chatNotebookLine[
	chatNotebookUncompressMessage[message],
	meta,
	o
	];
chatNotebookLine[
	messages : {__String}, 
	meta_, 
	o : OptionsPattern[]
	] :=
Cell[
	CellGroupData[
		chatNotebookLine[#, meta, o] & /@ messages
		]
	];
chatNotebookLine[
	messageData_Association, 
	o : OptionsPattern[]
	] :=
chatNotebookLine[
	Lookup[messageData, 
		"Message", 
		Compress@Cell["Message lost in transit", "Message"]
		],
	KeyDrop[messageData, "Message"],
	o
	];


(* ::Subsubsection::Closed:: *)
(*chatNotebookPut*)


(* ::Text:: *)
(*chatNotebook :*)
(*    Generate a basic chat notebook. The SObj version will do most of the option passing.   *)


Options[chatNotebookPut] =
Append[
	Options@NotebookPut,
	"CellOptions" ->
	Complement[Options@chatNotebookLine, Options@Cell]
	];
chatNotebookPut[
	chatLog_List,
	nb : _NotebookObject?(NotebookInformation[#] =!= $Failed &) | 
	Automatic : Automatic,
	o : OptionsPattern[]
	] :=
With[{co = OptionValue["CellOptions"]},
	With[{nbNew =
			NotebookPut[
				Notebook[
					Flatten[chatNotebookLine[#, co] & /@ chatLog]
					],
				Replace[nb, Automatic :> Sequence @@ {}],
				Sequence @@
				FilterRules[
					{
						o, 
						WindowSize -> {650, 700},
						Saveable -> False
						}, 
					Options@NotebookPut
					]
				]
			},
		SelectionMove[nbNew, After, Notebook];
		nbNew
		]
	];
chatNotebookPut[
	chat_SObj,
	nb : _NotebookObject?(NotebookInformation[#] =!= $Failed &) | 
	Automatic : Automatic,
	o : OptionsPattern[]
	] :=
 With[
	{
		objData =
		Block[{$SObjGetWrap = False},
			chat[[{"ChatLog", "ChatNotebook", "ChatCellSettings"}]]
			]
		},
	Replace[
		chatNotebookPut[
			Replace[objData["ChatLog"], Except[{__Association}] -> {}],
			Replace[
				objData["ChatNotebook"],  
				Except[_NotebookObject?(NotebookInformation[#] =!= $Failed &)] ->
				Automatic
				],
			FilterRules[
				Flatten@{
					NotebookEventActions ->
						chat["ChatNotebookEventActions"],
					DockedCells ->
						chat["ChatNotebookDockedCell"],
					o, 
					WindowTitle ->
						chat["ChatName"],
					TaggingRules->
						{
							"ChatSystem"->
								{
									"ChatObject"->chat
									}
							},
					Normal@
						Replace[objData["ChatCellSettings"], Except[_?OptionQ] -> {}]
					},
				Options@chatNotebookPut
				]
			],
		{
			nb2_NotebookObject?(NotebookInformation[#] =!= $Failed && # =!= 
				objData["ChatNotebook"] &) :>
			Set[chat["ChatNotebook"], nb2],
			Except[_NotebookObject?(NotebookInformation[#] =!= $Failed &)] ->
			$Failed
			}
		]
	]


(* ::Subsubsection::Closed:: *)
(*chatNotebookWrite*)


(* ::Text:: *)
(*chatNotebookWrite :*)
(*    write a message to the end of the notebook*)


Options[chatNotebookWrite] =
Options@chatNotebookLine;
chatNotebookWrite[
	messages : {__Association}, 
	nb_NotebookObject?(NotebookInformation[#] =!= $Failed &),
	o : OptionsPattern[]
	] :=
With[{c =
		Cells[nb, CellTags -> {"ChatCellWritten"}]
		},
	FrontEndExecute@
	{
		(*FrontEnd`NotebookSuspendScreenUpdates[nb],
		*)
		If[Length@c > 0,
			FrontEnd`SelectionMove[
				Last@c,
				After, 
				Cell, 
				AutoScroll -> False
				],
			FrontEnd`SelectionMove[
				nb,
				After, 
				Notebook, 
				AutoScroll -> False
				]
			],
		FrontEnd`NotebookWrite[
			nb, 
			Map[
				chatNotebookLine[#, o] &,
				messages
				],
			AutoScroll -> False
			](* ,
		FrontEnd`NotebookResumeScreenUpdates[nb]*)
		}
	];
chatNotebookWrite[
	message : _Association, 
	nb_NotebookObject?(NotebookInformation[#] =!= $Failed &),
	o : OptionsPattern[]
	] :=
chatNotebookWrite[{message}, nb, o];
chatNotebookWrite[chat_SObj, o : OptionsPattern[]] :=
 
 With[{lm = chat["LastMessage"], lw = chat["LastWritten"]},
	If[AssociationQ@lm && lw =!= lm(*&&lm[
		"RequesterWolframUUID"]=!=$WolframUUID*),
	chat["LastWritten"] = lm;
	chatNotebookWrite[
		lm,
		SObjLookup[
			chat, 
			"ChatNotebook",
			chatNotebookPut[chat]
			]
		]
	]
]


(* ::Subsubsection::Closed:: *)
(*chatNotebookNewCells*)


(* ::Text:: *)
(*chatNotebookNewCells :*)
(*    get the new cells from a Notebook to be sent through the channel*)


Options[chatNotebookNewCells] =
Options@Cells;
chatNotebookNewCells[
	nb_NotebookObject?(NotebookInformation[#] =!= $Failed &),
	o : OptionsPattern[]
	] :=
With[{c = Cells[nb, Sequence@@Flatten@{o}]},
	With[{tags = CurrentValue[c, CellTags]},
		Pick[c, ! MemberQ[#, "ChatCellWritten"] & /@ tags]
		]
	];
chatNotebookNewCells[
	chat_SObj,
	o : OptionsPattern[]
	] :=
 chatNotebookNewCells[chat["ChatNotebook"], 
	FilterRules[
		Flatten@
		{
			o,
			Normal@Replace[chat["SendCellOptions"],
				Except[_?OptionQ] -> {}
				]
			},
		Options@Cells
		]
	]


(* ::Subsubsection::Closed:: *)
(*chatNotebookSendCells*)


(* ::Text:: *)
(*chatNotebookSendCells :*)
(*    send the new cells from a Notebook through a channel*)


Options[chatNotebookSendCells] =
Options@chatNotebookNewCells;
chatNotebookSendCells[
	channel_ChannelObject,
	cells : {__CellObject},
	o : OptionsPattern[]
	] :=
With[{content = Compress@*NotebookRead /@ cells},
	NotebookDelete[cells];
	ChannelSend[channel, content]
	];
chatNotebookSendCells[
	channel_ChannelObject,
	nb_NotebookObject?(NotebookInformation[#] =!= $Failed &),
	o : OptionsPattern[]
	] :=
With[{c = chatNotebookNewCells[nb, o]},
	If[Length@c > 0,
		chatNotebookSendCells[channel, c],
		$Failed
		]
	];
chatNotebookSendCells[chat_SObj] :=
 Replace[
  chat["ChatNotebook"],
	nb_NotebookObject?(NotebookInformation[#] =!= $Failed &) :> 
	 chatNotebookSendCells[
	  chat["ChannelObject"], 
	  nb,
	  chat["SendCellOptions"]
	  ]
	]


(* ::Subsubsection::Closed:: *)
(*chatObjectNotebookDockedCell*)


(* ::Text:: *)
(*chatObjectNotebookDockedCell:*)
(*	builds the DockedCell that manages most Dynamic interactivity*)


chatObjectNotebookDockedCell[chat : SObj[s_Symbol]] :=
 Cell[
	BoxData@ToBoxes@
	Grid[
		{
			{
				InputField[
					Dynamic[chat["ChatName"],
						Function[
							Set[chat["ChatName"], #];
							SetOptions[EvaluationNotebook[],
								WindowTitle -> #
								]
							],
						TrackedSymbols :> {s}
						], 
					String,
					Appearance -> "Frameless"
					],
				InputField[
					Dynamic[chat["ChannelPath"],
						Function[
							Set[chat["ChannelPath"], #];
							chat["StartChat"][]
							],
						TrackedSymbols :> {s}
						], 
					String,
					Appearance -> "Frameless"
					],
				SpanFromLeft
				},
			{
				Row@{
					"Listening: ",
					Button[
						Checkbox[
							Dynamic[
								chat["ChannelStatus"] === "Active",
								TrackedSymbols :> {s}
								]
							],
						If[chat["ChannelStatus"] =!= "Active",
							chat["ChannelListen"][],
							chat["ChannelMute"][]
							],
						Appearance -> None
						]
					}(*,
				Button["Send", 
					chat["ChatSend"][]
					]*)
				}
			},
		Alignment -> {Left, Center},
		Background -> White,
		Frame -> True,
		FrameStyle -> GrayLevel[.9],
		ItemSize -> Scaled[.5]
		],
	Background ->
	Dynamic[
		Replace[
			chat["ChannelStatus"],
			{
				"Active" ->
				Hue[.33, .5, .5],
				"Inactive" ->
				Hue[.16666, .3, 1],
				_ ->
				Hue[0, 1, .8]
				}
			],
		TrackedSymbols :> {s}
		],
	CellDynamicExpression ->
	Dynamic[
		chat["ChatNotebookWrite"][],
		TrackedSymbols :> {s}
		],
	ShowStringCharacters -> False
	]


(* ::Subsubsection::Closed:: *)
(*chatObjectNotebookEventActions*)


(* ::Text:: *)
(*chatObjectNotebookEventActions:*)
(*	adds in the EventActions that will handle sending a message through the chat*)


chatObjectNotebookEventActions[chat : SObj[s_Symbol]] :=
 {
	{"MenuCommand", "HandleShiftReturn"} :>
		Switch[
			Lookup[
				FrontEndExecute@
				FrontEnd`UndocumentedGetSelectionPacket@
				NotebookSelection[EvaluationNotebook[]],
				"CellSelectionType"
				],
			"BelowCell" | "AboveCell",
			chat["ChatSend"][]
			],
	{"MenuCommand", "SaveRename"} :>
		CurrentValue[EvaluationNotebook[], 
		 {TaggingRules, "ChatSystem", "Cache"}:>
		 	Compress[s]
		 ],
	PassEventsDown -> True
	}


(* ::Subsubsection::Closed:: *)
(*chatObjectCreateChannel*)


(* ::Text:: *)
(*chatObjectCreateChannel:*)
(*	creates the ChannelObject for the SObj*)


chatObjectCreateChannel[chat_SObj] :=
 With[
	{
		channelPath =
		ChannelObject[
			SObjLookup[chat, "ChannelPath", 
				"ChatRoom"]
			][[1, "DisplayURL"]]
			},
		Quiet[
			Replace[
				If[! MemberQ[URLParse[channelPath, "Path"], $WolframID],
					ChannelObject@channelPath,
					CreateChannel[
						channelPath,
						FilterRules[
							Flatten@{
								Normal@
									Replace[chat["ChannelOptions"],
									Except[_?OptionQ] -> {}
									]
								},
							Options@CreateChannel
							]
						]
					],
				c_ChannelObject :>
				(
					If[Length@$MessageList > 0,
						SetOptions[
							c,
							FilterRules[
								Flatten@{
									Normal@Replace[chat["ChannelOptions"],
										Except[_?OptionQ] -> {}
										]
									},
								Options@ChannelObject
								]
							]
						];
					Set[chat["ChannelObject"], c]
					)
				],
			CreateChannel::exst
			]
		]


(* ::Subsubsection::Closed:: *)
(*chatObjectChannelListen*)


(* ::Text:: *)
(*chatObjectChannelListen:*)
(*	creates the ChannelListener for the SObj*)


chatObjectChannelHandlerFunction[chat_SObj] :=
Function[
	AppendTo[chat["ChatLog"], #];
	chat["MessageHandler"][#]
	];
chatObjectChannelListen[chat_SObj] :=
chat["ChannelListener"] =
ChannelListen[
	chat["ChannelObject"],
	chatObjectChannelHandlerFunction[chat]
	];


(* ::Subsubsection:: *)
(*chatObjectClass*)


(* ::Text:: *)
(*ChatObject:*)
(*	the class that builds new chats*)


ChatObject =
	SObj[
		"Chat",
		{"Class"},
		<|
			"ObjectInstanceProperties" ->
			<|
				"CreateChannel" ->
					SObjMethod[chatObjectCreateChannel],
				"ChannelObject" ->
					SObjProperty[chatObjectCreateChannel],
				"ChannelOptions" ->
					{
						Permissions -> "Private"
						},
				"GetChannelOptions" -> 
					SObjMethod@
						Function[
							Replace[#["ChannelObject"],
								{
									c_ChannelObject:>Set[#["ChannelOptions"], Options[c]],
									_->$Failed
									}
								]
							],
				"SetChannelOptions" ->
					SObjMethod@
						Function[
							Replace[#["ChannelObject"],
								{
									c_ChannelObject:>SetOptions[c, #2],
									_->$Failed
									}
								]
							],
				"AddChannelMember" ->
					SObjMethod@
						Function[
							With[
								{
									pp=
										Replace[Flatten@{#2}, u_String:>(u->{"Read", "Write"}), 1]
									},
								Replace[#["ChannelObject"],
									{
										c_ChannelObject:>
											SetOptions[c, 
												Permissions->
													Merge[
														{
															#["GetChannelOptions"][][Permissions],
															pp
															},
														Last
														]
												],
										_->$Failed
										}
									]
							]
						],
				"RemoveChannelMember" ->
					SObjMethod@
						Function[
								Replace[#["ChannelObject"],
									{
										c_ChannelObject:>
											SetOptions[c, 
												Permissions->
													KeyDrop[#["GetChannelOptions"][][Permissions], #2]
												],
										_->$Failed
										}
									]
							],
				"ChannelListen" ->
					SObjMethod[chatObjectChannelListen],
				"ChannelMute" ->
					SObjMethod[
					RemoveChannelListener@#["ChannelListener"] &
					],
				"ChannelListener" -> None,
				"ChannelStatus" ->
				SObjProperty[
					Quiet@ChannelFramework`ChannelListenerStatus@
					#["ChannelListener"] &
					],
				"ChannelPath" -> 
					"ChatRoom",
				
				"StartChat" ->
					SObjMethod@
						Function[
							chatObjectCreateChannel[#];
							chatObjectChannelListen[#];
							#["OpenChat"][]
							],
				"OpenChat" ->
					SObjMethod[
						Replace[
							#["ChatNotebook"],
							Except[_NotebookObject?(NotebookInformation[#] =!= $Failed)] :>
								chatNotebookPut[#]
							] &
						],
				
				"ChatName" -> 
					"My chat",
				
				"ChatNotebook" -> 
					None,
				"ChatNotebookDockedCell" ->
					SObjProperty[
						chatObjectNotebookDockedCell
						],
				"ChatNotebookEventActions" ->
					SObjProperty[
						chatObjectNotebookEventActions
						],
				"ChatNotebookWrite" ->
					SObjMethod[chatNotebookWrite],
				"ChatCellSettings" ->
					{
						"NameMapping" -> {},
						"NameColoring" -> {}
						},
				
				"ChatNewCells" ->
					SObjProperty[chatNotebookNewCells],
				"ChatModified" ->
					SObjProperty[
						Length@chatNotebookNewCells@# > 0 &
						],
				"ChatSend" ->
					SObjMethod[chatNotebookSendCells],
				"SendCellOptions" ->
					{
						CellStyle -> 
							{
							 "Title", "Subtitle",
							 "Chapter", "Subchapter",
							 "Section", "Subsection", "Subsubsection",
								"Input", "Code", "Output", "Text",
								"Item", "ItemParagraph", "Subitem", "SubitemParagraph",
								"ItemNumbered", "SubitemNumbered"
								}
						},
				
				"ChatLog" -> {},
				"LastMessage" ->
					SObjProperty[
						Quiet[Check[#[["ChatLog", -1]], None, Part::partw], 
							Part::partw] &
						],
				"LastWritten" -> None
				|>,
			"StartChat" ->
				SObjMethod[
					With[{n = #["New"][##]},
						n["StartChat"][];
						n
						] &
					]
			|>
		];


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
