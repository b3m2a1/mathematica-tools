(* ::Package:: *)

BeginPackage["NotebookTabbing`"];


(* ::Subsection:: *)
(*Exposed Interface*)


(* ::Subsubsection:: *)
(*Formatting*)


$TabBackgroundActive::usage="Active background for tabs";
$TabBackgroundInactive::usage="Inactive tab background";
$TabBorderActive::usage="Active tab border color";
$TabBorderInactive::usage="Active tab border color";
$TabForegroundActive::usage="Tab active text color";
$TabForegroundInactive::usage="Tab inactive text color";
$TabShape::usage="The shape for new tabs";


(* ::Subsubsection:: *)
(*Caches*)


$NotebookCache::usage="The notebook cache";
$ActiveNotebookTab::usage="The active tab tag for a notebook";


(* ::Subsubsection:: *)
(*Tab Objects*)


TabObject::usage="Generates a tab without bindings applied";
TabButton::usage="Binds the switch commands to the tab";
SwitchTabs::usage="Switches to the given tab";


(* ::Subsubsection:: *)
(*Docked Cell Interface*)


TabDockedCell::usage="Configures a tab bar docked cell";
TabDockedCellFileInterface::usage="Configures a tab bar to work with opening and saving files";
SetDockedTabs::usage="Sets up a docked cell of tabs";


(* ::Subsubsection:: *)
(*Docked Tab Editing*)


AddDockedTab::usage="Adds a tab to the docked cell bar";
RemoveDockedTab::usage="Removes a tab from the docked cell bar";
ChangeTabName::usage="Changes the display name of the tab with the specified tag";
ChangeTabTag::usage="Changes the tag value of the tab with the specified tag";


(* ::Subsubsection:: *)
(*Saving and Opening*)


TabNotebookCachePages::usage="Sticks the tab pages into the NotebookDynamicExpression";
TabOpenFile::usage="Opens a file in a new tab";
TabFileSave::usage="Saves a given tab to a file. Only does saving as .nb right now.";
TabNotebookSave::usage="Saves a tab notebook to a folder with a subfolder for the tab files";
TabNotebookSaveBindings::usage="Sets up the save bindings for the notebook";


(* ::Subsubsection:: *)
(*Info*)


GetTabName::usage="Gets the name assigned to a tab in a given notebook";
GetTabInfo::usage="Gets the tabs and names for a notebook";


Begin["`Private`"];


(* ::Subsection:: *)
(*Internals*)


(* ::Subsubsection:: *)
(*Formatting*)


$TabBackgroundActive=GrayLevel[.7];
$TabBorderActive=Blue;
$TabForegroundActive=White;


$TabBackgroundInactive=GrayLevel[.9];
$TabBorderInactive=GrayLevel[.7];
$TabForegroundInactive=GrayLevel[.3];


$TabDockBackgroundColor=GrayLevel[.95];


$TabShape=
With[{height=10,flatspan=30},
	Polygon@Join[
		Table[
			{x,height*LogisticSigmoid[x]},
			{x,Range[-4,4,.5]}
			],
		Table[
			{flatspan+8+x,height*LogisticSigmoid[-x]},
			{x,Range[-4,4,.5]}
			]
		]
	];


(* ::Subsubsection:: *)
(*Caches*)


(*If[Not@MatchQ[$NotebookCache,_Association],$NotebookCache=<||>];If[Not@MatchQ[$ActiveNotebookTab,_Association],
	$ActiveNotebookTab=<||>
	];*)


$NotebookCache[nb_]:=
CurrentValue[nb,{TaggingRules,"NotebookTabs"},<||>];
$NotebookCache/:Set[$NotebookCache[nb_],assoc_]:=
(CurrentValue[nb,{TaggingRules,"NotebookTabs"}]=assoc)
$NotebookCache/:SetDelayed[$NotebookCache[nb_],assoc_]:=
(CurrentValue[nb,{TaggingRules,"NotebookTabs"}]:=assoc);
$NotebookCache/:Set[$NotebookCache[nb_][tab_],val_]:=
Lookup[
	CurrentValue[nb,{TaggingRules,"NotebookTabs"}]=
	Append[
		CurrentValue[nb,{TaggingRules,"NotebookTabs"},<||>],
		tab->val
		],
	tab
	];
$NotebookCache/:SetDelayed[$NotebookCache[nb_][tab_],val_]:=
(CurrentValue[nb,{TaggingRules,"NotebookTabs"}]=
	Append[
		CurrentValue[nb,{TaggingRules,"NotebookTabs"},<||>],
		tab:>val
		];)


$ActiveNotebookTab[nb_]:=
CurrentValue[nb,{TaggingRules,"ActiveTab"},Missing["KeyAbsent",nb]];
$ActiveNotebookTab/:Set[$ActiveNotebookTab[nb_],tab_]:=
(CurrentValue[nb,{TaggingRules,"ActiveTab"}]=tab);
$ActiveNotebookTab/:SetDelayed[$ActiveNotebookTab[nb_],tab_]:=
(CurrentValue[nb,{TaggingRules,"ActiveTab"}]:=tab)


(* ::Subsubsection:: *)
(*Tab Objects*)


TabObject[
	notebook:_NotebookObject|Automatic:Automatic,
	name_,tag_,ops___]:=
With[{nb:=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	Interpretation[
		DynamicWrapper[
			Graphics[{
					Dynamic@
					If[($ActiveNotebookTab[nb]===tag),
						$TabBackgroundActive,
						$TabBackgroundInactive
						],
					Dynamic@
					If[($ActiveNotebookTab[nb]===tag),
						EdgeForm@$TabBorderActive,
						EdgeForm@$TabBorderInactive
						], 
					$TabShape,{
						Dynamic@
						If[($ActiveNotebookTab[nb]===tag),
							$TabForegroundActive,
							$TabForegroundInactive
							],
						Inset[
							name,
							Scaled[{.2,0}],
							{Left,Bottom}
							]
						}
					},
				ImageMargins->{{0,0},{0,3}},
				ImagePadding->{{0,0},{0,3}},
				PlotRangePadding->None,
				ops
				],
			If[DownValues[SwitchTabs]==={},
				CloudGet@
				"https://www.wolframcloud.com/objects/user-affd7b1c-ecb6-4ccc-8cc4-4d107e2bf04a/NotebookTabbing.m"
				]
			],
		tag]
	];


SwitchTabs::nonb="NotebookObject `` does not exist";
SwitchTabs[notebook_NotebookObject,tab_]:=
If[$ActiveNotebookTab[notebook]=!=tab,
	(*Cache current state*)
	Replace[
		$NotebookCache[notebook],
		_Missing:>Set[$NotebookCache[notebook],<||>]
		];
	With[{nb=NotebookGet@notebook},
		If[nb=!=$Failed,
			Replace[
				$ActiveNotebookTab[notebook],
				Except[Missing["KeyAbsent",notebook]]:>
				Set[
					$NotebookCache[notebook][$ActiveNotebookTab[notebook]],
					Compress@First@nb
					]
				];
			(*Set new tab*)
			$ActiveNotebookTab[notebook]=tab;
			(*Insert cached notebook values into notebook*)
			NotebookPut[
				ReplacePart[NotebookGet@notebook,
					1->
					Uncompress@
					Replace[
						$NotebookCache[notebook][tab],
						_Missing:>
						Set[
							$NotebookCache[notebook][tab],
							Compress@{}
							]
						]
					],
				notebook
				],
			Message[SwitchTabs::nonb,notebook];$Failed
			]
		];
	tab
	];
SwitchTabs[Automatic,tab_]:=
SwitchTabs[EvaluationNotebook[],tab]


TabButton[
	notebook:_NotebookObject|Automatic:Automatic,
	name_,tag_,
	command_:SwitchTabs]:=
EventHandler[
	TabObject[notebook,name,tag],
	"MouseClicked":>
	(
		If[command===SwitchTabs&&DownValues[SwitchTabs]==={},
			CloudGet@"https://www.wolframcloud.com/objects/user-affd7b1c-ecb6-4ccc-8cc4-4d107e2bf04a/NotebookTabbing.m"
			];
		command[notebook,tag]
		)
	];


(* ::Subsubsection:: *)
(*Docked Cell Interface*)


TabDockedCell[notebook_,tabSpecs:{__Rule},ops___]:=
Cell[BoxData@
	TagBox[
		PaneBox[
			RowBox@
			Table[
				ToBoxes@
				TabButton[
					Automatic,
					Sequence@@t
					],
				{t,tabSpecs}],
			ImageSize->Scaled[1]
			],
		"Tabs"],
	ops,
	CellMargins->{{0, 0}, {0, 0}},
	CellFrame->{{0,0},{2,0}},
	CellFrameColor->Dynamic@$TabBorderActive,
	CellFrameMargins->{{0, 0}, {-2, 3}},
	Background->$TabDockBackgroundColor];


saveIcon=Image[CompressedData["
1:eJylll9IZFUcx+/8cRj/h4Y4/l2TCM1QC6SHSnpMxD8rPrQvi0smQeOCGy4Z
WIgyM65lD/7HgsCHetNeknqwzNxyiE0d161GakMhkCAV3Zm5d+bX93fnHL2N
bnRnf/DxzD2e8/ue3+/87jm37Nr1y69bFUW54cSfyx03X+7p6Xi77TE8tHff
eKOru/O1V7rf6uzq7Hn+mg2dl4AL2BXTZhHtU2AEfA/ugnlwBVgTxpk1m2jr
QRjQBXwBnEIrGR2r0PlR+LsDOkArmDLousV4s2mSOSgFD0AMPJ0wZkFofCWe
bYo5kxqXDBruBD+PK/E8PmPStzSLgOMPKPH1so4fvA9aQG6Svo0m1/wi2FPO
7/df4GOQY1hTMibncV46wWcgmKA1L8ZYz81+uE8jRh1p6eAl8CXQBGUJOjzH
Jp5lXSf6kc/si/eD69Yl+mQOn1TO9unZi2Kx2+0PwwIcVqs+vEw5y8mHIgbW
zwAeEcPfytn+G3M7rcTf0a/Bd2AN/AS2wD0lnvNNi8VSrMTrSOr8IcbeN/R5
EuJTMO/N7OxsGhwcJJ/PR16vV28lHo+HhoeHqaqqiuff5thg72Leb4AM3Afv
gMT8sg2VlJSoRBQCmoGoEb/fr7Ivm812C3OeA6+C98AHoF+Jnyl8VlaBQoMW
m6eoqIgODw9VGEUiEeLWSCjE8kQDAwNRp9NJKSkphHhYLwwegBDgZ+6Pwv8R
qDfkzMsaJycnHAvFYjGKRqPndFibLRgMRjc3N6NbW1sUCAROQZ/erq+vq6Wl
pZzX69Z4oaQAn1GD/T+iRSoqKrh+u0Qt2hI12DY2NmhqaopmZmZoenr6FO6b
nJy8kImJCb0dHx/X8vLyyOFwzCBv1cifU2ocHx/rGru7u5Sbm3vR/ZMU0PpB
avCes8bIyIj+v7m5OVpdXaXl5WVaWVkxxdLSEs+LjY2NSa1/5aqyspLKy8sf
dU906+/v5xqMsEZBQQHXk7q9va3rDg0N6XvPNatpmim4Brk9OjrS8vPzYzk5
OZ+wBn6zrNrX16dr7Ozs6OtIpsZYg+t/YWGBzy7Kysp6gTWKi4v1PS8sLKS6
urqk/bNxDDy9paVFP1/S09MdrMF7sLi4qLLu7Oysvg5ej1mT69rb21Phm5Cn
YfGO+Kqrq6m9vV1FndH+/r4+jnXMmlzX6Ogov4MaNOT3g4/fB5xDKsdniNe0
8bqAVltbG0M9fQuf7J8D8YkaVufn55POk1zX2tqaxudzWlraVRGDHfnSNVwu
l3pwcHBag5xbM4TDYT0Ut9vN6/0zNTU1S94jOFO80I319vbywRq7gOh/YLxv
NOiE8K7xXtwS94f8HtXvzpqaGmpubqaGhgZqbGz8XzQ1NVFrayvxPra1tVF9
fb08O54Q96BeVBkZGTeh/Tvu25/xeyczMzOI9lfU3i/4fQ+/74IA2EDfHeBH
rm+Db5T4ty1/B3wOPgUf4Zy9ItZ++k3yDwt0TNs=
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];
openIcon=Image[CompressedData["
1:eJyllm1IZFUYx++8aON7aJjvtuuHVjPUD0kfAtlvJeLLDgMVxKJkUqQLbris
hB/EYWbcCQt8x4hAqA990L4kBFlmVg5Ujpq77QwU6RJJhYoyM/fOPP2fO+eO
t2m2ZmYf+M25c849z/855zznnHuh59qVV4ySJF234OdK943Lg4PdN60P449t
4PqrfQO9Lz838EZvX+/g0z0mVD4GSoFZStmMonwCvAU2wV3wObgJiuPeS9f/
C0ABlIB74Mk0dfh9gxSNPyj8/QqGgA24wamovw1yxPuGFDRMonxP+PlD6Ont
JRAW7T2iLtnl0GLhxfaDCHhb1D0k/Gi+PKL9o7jY/s+0eb0gnc+HVdRrvs0i
lndE+3dJ+o7XqBcxso/LcXFqWiOi/Q7IEHXJrImmcQmEhI9nE2iwL7to35WS
zyuD7t0i8Lvw0S/aMkWbFjOvA4/1s7j4DCIeo3Seo/cb35oUzZ1vdHWanzLw
p4jBoRtfzMxm8/0wAIvRqLp6Xjrfb9PgUTGWS0Kf63l/1sbpPwLmwCdS9Ez4
SoqeET9I0XnlPcU56zUYDDUoP9Dp/CVF1zeoq3tT+I3lLfq9XlBQQGNjY+Ry
ucjpdKqlhsPhoPHxcaqvr+f+Gzw2mBv97gEShIEPvAZirnXTZK+qqpKJKAAU
HWE9Ho9HZn8mk+kW+jwFuoELTEnRnG0Dj0vR86pcp8XmqKiooOPjYxlGoVCI
uNQTCLA80ejoaNhisVBGRgZhPKwXAGei5P9cr8D/CWjRzZmTNc7OzngsFIlE
KBwO/0uHtdl8Pl94e3s7vLu7Szs7OzFQp5ZbW1tydXU1z+s1YzRROK9deg32
/4AWqq2t5T3SJ3LRFK/B5vV6aXZ2lubn52lubi4G183MzCRkenpaLaemppTi
4mLKzMycx7w1YP4smsbp6amqsb+/T0VFRYnun7SA1reaBq85a7jdbrVtcXGR
NjY2aG1tjdbX11NidXWV+0UmJyc1rX/MVV1dHdXU1Dzomqg2MjLCOcjnqKus
rIzzSd7b21N17Xa7uvacs4qipATnIJcnJydKSUlJpLCw8H3WwDPLysPDw6qG
3+9X40gnx1iD8395eVn9rsjPz3+GNSorK9U1Ly8vp+bm5rT9s/EYuHtnZyef
Mb/k5OTwmeniNVhZWZFZd2FhQY2D40nVtLgODg5k+CbM07jYI66Ghgay2Wwy
8owODw/V91gnVdPimpiY4D2oQEP71nLxfsA5JPP4dONN2TguoDQ1NUWQT1/C
p3aHuEQOy0tLS2nPkxbX5uYmn4mUnZ19VYzBjPlSNUpLS+Wjo6NYDvLcpkIw
GFSH0t/fz/H+lpWVla/dIzhTnNCNDA0N8cEaSUD4P9DfNwp0AthrvBa3xP2h
3ef8/U2NjY3U0dFBra2t1NbWlhTt7e3U1dVFvI5Wq5VaWlq0s+OipPumyc3N
vQHtn3Hf3sGzPy8vz4fyLnLvJzzfxvOPYAd4Ufc98GCuvwZfoPunUvQ74GPw
IXgX5+yLIvbY99XfExVSWA==
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];


TabDockedCellFileInterface[notebook_,tabSpecs:{__Rule},ops___]:=
With[{c=TabDockedCell[notebook,tabSpecs,ops]},
	Replace[
		c,
		Cell[BoxData[TagBox[
			PaneBox[
				RowBox@{
					tabs___
					},
				po___],
			"Tabs"
			]],
		co___]:>
	With[{addition=Row[{
			Button[
				Mouseover[
					Framed[saveIcon,
						RoundingRadius->5,
						FrameStyle->None
						],
					Framed[saveIcon,
						Background->GrayLevel[.98],
						RoundingRadius->5,
						FrameStyle->GrayLevel[.7]
						]
					],
				TabFileSave[notebook,$ActiveNotebookTab[notebook]],
				Method->"Queued",
				Appearance->"Frameless"
				],
			Button[
				Mouseover[
					Framed[openIcon,
						RoundingRadius->5,
						FrameStyle->None
						],
					Framed[openIcon,
						Background->GrayLevel[.98],
						RoundingRadius->5,
						FrameStyle->GrayLevel[.7]
						]
					],
				TabOpenFile[notebook],
				Method->"Queued",
				Appearance->"Frameless"
				],
			Spacer@5},
		(*Frame\[Rule]True,
		FrameStyle\[Rule]GrayLevel[.85],*)
		BaselinePosition->Scaled[.2]
		]},
	Cell[
		BoxData[
			RowBox@{
				TagBox[
					PaneBox[
						RowBox@{
							tabs
							},
						ImageSize->With[{w=First@ImageDimensions@Rasterize@addition},
							Dynamic[
								(First@(WindowSize/.AbsoluteOptions[EvaluationNotebook[],WindowSize]))-w]
							],
						po
						],
					"Tabs"
					],
				ToBoxes@addition
				}
			],
		co]
	]
]
];


tabRow[tabNum_Integer,topMost:_Integer:1]:=
With[{tab=Graphics[{EdgeForm@$TabBorderInactive,$TabBackgroundInactive,$TabShape},
		PlotRangePadding->None,
		Method->{"ShrinkWrap"->True},
		ImageSize->100,
		ImagePadding->None]},
With[{baseSize=ImageDimensions@tab},
	Graphics[
		Table[
			Inset[
				tab,
				{.85*(tabNum-i)*First@baseSize,0},
				{Left,Center},
				baseSize
				],
			{i,tabNum}]//Append[
			Delete[#,-topMost],
			#[[-topMost]]
			]&,
		ImageSize->{.85*tabNum*First@baseSize,Last@baseSize},
		PlotRangePadding->None,
		ImageMargins->None
		]
	]
]


SetDockedTabs[
	notebook:_NotebookObject|Automatic:Automatic,
	tabSpecs:{__Rule},
	cellFunction:(DockedCells->_):(DockedCells->TabDockedCell),
	ops___
	]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	SetOptions[nb,
		DockedCells->(Last@cellFunction)[notebook,tabSpecs,ops]
		];
	$ActiveNotebookTab[nb]=Last@First@tabSpecs;
	If[MatchQ[$NotebookCache[nb],_Missing],
		$NotebookCache[nb]=<||>
		];
	$NotebookCache[nb][Last@First@tabSpecs]=First@NotebookGet@nb;
	];
SetDockedTabs[
	notebook:_NotebookObject|Automatic:Automatic,
	t_Rule,
	ops___]:=
SetDockedTabs[notebook,{t},ops];


(* ::Subsubsection:: *)
(*Docked Tab Editing*)


AddDockedTab[
	notebook:_NotebookObject|Automatic:Automatic,
	name_,tag_,
	cachedNotebook:_List:{}]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	With[{b=TabButton[notebook,name,tag]},
		Replace[DockedCells/.Options[nb,DockedCells],
			Cell[
				stuff_,
				ops___
				]:>(
				SetOptions[nb,
					DockedCells->Cell[
						stuff/.TagBox[PaneBox[
							RowBox@tabs_List,
							pops___],
						"Tabs"]:>
					TagBox[PaneBox[
						RowBox@Append[tabs,ToBoxes@b],
						pops],
					"Tabs"],
				ops]
			];
		$NotebookCache[nb][tag]=cachedNotebook;
		)
	];
]
];
AddDockedTab[
	notebook:_NotebookObject|Automatic:Automatic,
	name_->tag_,
	cachedNotebook:_List:{}]:=
AddDockedTab[notebook,name,tag,cachedNotebook];


RemoveDockedTab[
	notebook:_NotebookObject|Automatic:Automatic,
	tag_]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	Replace[DockedCells/.Options[nb,DockedCells],
		Cell[
			stuff_,
			ops___
			]:>(
			SetOptions[nb,
				DockedCells->Cell[
					stuff/.TagBox[PaneBox[
						RowBox@tabs_List,
						pops___],
					"Tabs"]:>
				TagBox[
					PaneBox[
						RowBox@
						DeleteCases[tabs,
							TagBox[
								InterpretationBox[_,tag],
								_
								]
							],
						pops],
					"Tabs"],
				ops]
			];
		$NotebookCache[nb][tag]=.;
		)
	];
];


ChangeTabName[notebook:_NotebookObject|Automatic:Automatic,
	tag_,newName_]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	Replace[DockedCells/.Options[nb,DockedCells],
		Cell[
			stuff_,
			ops___
			]:>(SetOptions[nb,
				DockedCells->Cell[
					stuff/.InterpretationBox[o_,tag]:>
					With[{eval=
							o/.InsetBox[_,blah___]:>InsetBox[newName,blah]},
						InterpretationBox[eval,tag]
						],
					ops]
				];
			)
		];
	];


ChangeTabTag[notebook:_NotebookObject|Automatic:Automatic,tag_,newTag_]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	Replace[DockedCells/.Options[nb,DockedCells],
		Cell[
			stuff_,
			ops___
			]:>(SetOptions[nb,
				DockedCells->Cell[
					stuff/.TagBox[InterpretationBox[o_,tag],h_]:>
					With[{d=(o/.HoldPattern[e_===tag]:>(e===newTag))},
						TagBox[
							InterpretationBox[
								d,
								newTag],
							h/.HoldPattern[SwitchTabs[n_,tag]]:>SwitchTabs[n,newTag]
							]
						],
					ops]
				];
			$NotebookCache[nb][newTag]=$NotebookCache[nb][tag];
			$NotebookCache[nb][tag]=.;
			)
		];
	];


(* ::Subsubsection:: *)
(*Saving Opening*)


TabNotebookCachePages[notebook_]:=
With[{
		n=Extract[First@Options[notebook,NotebookDynamicExpression],2,Hold],
		cache=Replace[$NotebookCache[notebook],_Missing-><||>],
		active=Replace[$ActiveNotebookTab[notebook],_Missing->None],
		dc=DockedCells/.Options[notebook,DockedCells]},
	With[{nbDynamicExpr=
			Replace[n,{
					Hold[
						If[MatchQ[$NotebookCache[_],_Missing],__]|
						Null|
						None]:>
					(NotebookDynamicExpression:>$$cmd),
					Hold[CompoundExpression[
						If[MatchQ[$NotebookCache[_],_Missing],__],
						a__
						]
					]:>(
					NotebookDynamicExpression:>
					CompoundExpression[
						$$cmd,
						a
						]
					),
				Hold[a_]:>(
					NotebookDynamicExpression:>
					CompoundExpression[
						$$cmd,
						a
						]
					)
				}/.$$cmd:>
			With[{nb=EvaluationNotebook[]},
				If[
					MatchQ[$NotebookCache[nb],_Missing],
					$NotebookCache[nb]=cache;
					$ActiveNotebookTab[nb]=active;
					With[{d=(dc/._NotebookObject->nb)},
						SetOptions[nb,DockedCells->d]
						]
					]
				]
			]},
	SetOptions[notebook,nbDynamicExpr]
	]
];


TabOpenFile[notebook_,file_String?FileExistsQ]:=
With[{stuff=
		Switch[file,
			_?StringMatchQ[__~~".m"|".wl"],{Cell[BoxData@Import[file,"Text"],"Input"]},
			(*_?StringMatchQ[__~~".m"|".wl"],*)
			_,Import[file]
			]},
	AddDockedTab[notebook,
		FileNameTake@file,
		file,
		Replace[
			stuff,{
				Notebook[cellStuff_,___]:>cellStuff,
				e:Except[_List]:>{Cell[BoxData@ToBoxes@e,"Output"]},
				l:Except[{(_Cell|_CellGroup)...}]:>(Cell[BoxData@ToBoxes@#,"Output"]&/@l)
				}]
		]
	];
TabOpenFile[notebook_]:=
With[{f=SystemDialogInput["FileOpen",".nb"]},
	TabOpenFile[notebook,f]
	];


TabFileSave[notebook:_NotebookObject|Automatic:Automatic,
	tab_,file_String?(StringMatchQ["*.nb"])]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	Export[file,
		DeleteCases[
			If[$ActiveNotebookTab[nb]===tab,
				NotebookGet@nb,
				ReplacePart[
					NotebookGet@nb,
					1->$NotebookCache[nb][tab]
					]
				],
			(DockedCells->_)
			]
		]
	];
TabFileSave[
	notebook:_NotebookObject|Automatic:Automatic,
	tab_]:=
With[{f=SystemDialogInput["FileSave",".nb"]},
	If[f=!=$Canceled,
		TabFileSave[notebook,tab,f]
		]
	];


TabNotebookSave[
	notebook:_NotebookObject|Automatic:Automatic,
	fileBaseName_:Automatic]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	With[{baseName=
			Replace[fileBaseName,
				Automatic:>
				Replace[Quiet@NotebookFileName@nb,
					$Failed:>SystemDialogInput@"FileSave"
					]
				]},
		If[baseName=!=$Canceled,
			With[{folder=DirectoryName@baseName},
				NotebookSave[nb,
					FileNameJoin@{folder,
						FileNameTake@baseName
						}];
				Quiet@CreateDirectory@FileNameJoin@{folder,
					FileBaseName@baseName<>"_saved_tabs"};
				Do[
					TabFileSave[nb,
						First@pair,
						FileNameJoin@{folder,
							FileBaseName@baseName<>"_saved_tabs",
							{ToString@First@pair,"_",ToString@Last@pair}<>".nb"
							}
						],
					{pair,GetTabInfo[nb]}
					]
				]
			]
		]
	];


TabNotebookSaveBindings[notebook:_NotebookObject|Automatic:Automatic]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	With[{o=NotebookEventActions/.Options[nb,NotebookEventActions]},
		SetOptions[nb,
			NotebookEventActions->Join[
				DeleteCases[
					Replace[o,None->{}],
					(Rule|RuleDelayed)[{"MenuCommand","Save"|"SaveRename"},_]
					],{
					{"MenuCommand","Save"}:>(
						TabNotebookCachePages@nb;NotebookSave@nb
						),
					{"MenuCommand","SaveRename"}:>(
						TabNotebookCachePages@nb;NotebookSave@nb
						)
					}]
			]
		]
	];


(* ::Subsubsection:: *)
(*Info*)


GetTabName[notebook_,tag_]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	Replace[DockedCells/.Options[nb,DockedCells],{
			Cell[
				stuff_,
				ops___
				]:>Replace[
				Cases[stuff,
					InterpretationBox[o_,tag]:>
					Cases[o,InsetBox[n_,___]:>n,\[Infinity]],
					\[Infinity]],{
					{}:>None,
					{{n_,___},___}:>n
					}],
			_->None
			}]
	];
GetTabInfo[notebook_]:=
With[{nb=Replace[notebook,Automatic:>EvaluationNotebook[]]},
	Replace[DockedCells/.Options[nb,DockedCells],{
			Cell[
				stuff_,
				ops___
				]:>
			Cases[stuff,
				InterpretationBox[o_,t_]:>
				(t->
					Replace[
						Cases[o,InsetBox[n_,___]:>n,\[Infinity]],
						{
							{}:>None,
							{n_,___}:>n
							}]
					),
				\[Infinity]],
			_->None
			}]
	]


End[];


EndPackage[];


(*CopyFile[StringReplace[NotebookFileName[],".nb"\[Rule]".m"],CloudObject["NotebookTabbing.m",
	Permissions\[Rule]"Public"
	]
]//First*)
