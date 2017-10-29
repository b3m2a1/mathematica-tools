(* ::Package:: *)

FileBrowser::usage="A finder-like file browser";


Clear@FileBrowser;
Options[FileBrowser]=
Join[
	Options@Pane,
	Options@Framed,
	{
		"MaxLength"->20,
		"FrameWidth"->Automatic,
		"BarSize"->150,
		Function->SystemOpen,
		DeleteCases->".*",
		"TopBar"->True,
		"SideBar"->True,
		Show->All
		}
	];
FileBrowser[
	directory_,
	defaultDirectories:{__String}|Automatic:Automatic,
	ops:OptionsPattern[]]:=
DynamicModule[{
		browserMaxFileNameLength=
		Replace[OptionValue@"MaxLength",
			Except@_Integer->20
			],
		browserBasePaneNumber=OptionValue@Show,browserPaneNumber,
		browserActiveDirectory=ExpandFileName@directory,
		browserActiveFile={""},
		browserDirectoryFrameWidth=Replace[OptionValue@"FrameWidth",Except[_Integer]->250],
		browserSideBarWidth=Replace[OptionValue@"BarSize",Except[_Integer]->150],
		browserDeleteFilesPattern=Replace[OptionValue@DeleteCases,s_String:>{s}],
		browserFileFilterPattern="",
		browserOnClickFunction=OptionValue@Function,
		browserWidthHeight=Replace[OptionValue@ImageSize,{
				{w_,h_}:>{w,h},
				Automatic->{500,250},
				w_:>{w,250}
				}
			]},
	browserPaneNumber=browserBasePaneNumber;
	With[{
			topBar=
			Framed[
				Pane[
					Grid@{
						{
							(*Dynamic@browserActiveDirectory,*)
							Framed[
								InputField[
									Dynamic[browserFileFilterPattern,(
											If[#==="",
												browserPaneNumber=browserBasePaneNumber,
												browserPaneNumber=1
												];
											browserFileFilterPattern=#;)&
										],
									String,
									FieldHint->"Filter by...",
									FieldSize->8,
									Appearance->"Frameless"
									],
								RoundingRadius->5,
								FrameStyle->GrayLevel[.8],
								Background->GrayLevel[.98]
								]
							}
						},
					Alignment->Right,
					ImageSize->{
						Replace[
							First@browserWidthHeight,
							i_Integer:>
							(If[OptionValue@"SideBar"//TrueQ,
									browserSideBarWidth,
									0]+i)
							],Automatic}
					],
				Background->GrayLevel[.9],
				FrameStyle->GrayLevel[.4],
				RoundingRadius->3
				],
			sideBar=Framed[
				Pane[
					Deploy@Column[Table[
						With[{d=d},
							EventHandler[
								Framed[
									Pane[
										FileBaseName@d,
										Full
										],
									FrameStyle->None,
									Background->Dynamic[If[browserActiveDirectory===d,GrayLevel[.9],None]]
									],
								"MouseClicked":>(
									browserActiveDirectory=d;
									browserActiveFile={""}
									)
								]
							],
						{d,
							Replace[
								defaultDirectories,
								Automatic:>{
									$HomeDirectory,
									$UserDocumentsDirectory,
									$UserBaseDirectory}
								]}],
						ItemSize->Full
						],
					{browserSideBarWidth,Last@browserWidthHeight}
					],
				FrameMargins->None,
				FrameStyle->None,
				Background->GrayLevel[.95]
				],
			
			panes=
			Dynamic[
				Pane[
					(*Pane[*)
						Grid[{
								Table[
									With[{
											d=FileNameTake[browserActiveDirectory,i],
											filterPattern=Replace[browserFileFilterPattern,{
													_String?(StringLength@#>0&):>(___~~browserFileFilterPattern~~___),
													_:>Except[Alternatives@@browserDeleteFilesPattern]
													}]},
											ListPicker[Dynamic@browserActiveFile,
												Replace[
													(#->
														If[
															StringLength@FileNameTake@#>browserMaxFileNameLength,
															StringTake[FileNameTake@#,
																Floor[browserMaxFileNameLength/2]-2]
															<>"..."<>
															StringTake[FileNameTake@#,-Floor[browserMaxFileNameLength/2]-1],
															FileNameTake@#
															])&/@If[
														MatchQ[filterPattern,_Except],
														With[{fp=First@filterPattern},
															DeleteCases[Quiet@FileNames["*",d],_?(StringMatchQ[FileNameTake@#,fp]&)]
															],
														Cases[Quiet@FileNames["*",d],_?(StringMatchQ[FileNameTake@#,filterPattern]&)]
														],
													{}:>If[
														browserFileFilterPattern==="",
														browserActiveDirectory=DirectoryName@d,
														{"No files"}
														]
													],
												Multiselection->False,
												ImageSize->{browserDirectoryFrameWidth,Last@browserWidthHeight(*-20*)},
												Scrollbars->False(*{False,Automatic}*),
												FrameMargins->None,
												Appearance->"Frameless"
												]
											],
										{i,
											Replace[browserPaneNumber,{
													Except@_Integer->2,
													n_:>Max@{FileNameDepth@browserActiveDirectory-Max@{n,1}+1,2}
													}],FileNameDepth@browserActiveDirectory}
											]},Dividers->{
										Array[#->Gray&,
											Replace[browserPaneNumber,{
													Except@_Integer:>(FileNameDepth@browserActiveDirectory-2),
													_->Max@{browserPaneNumber-2,2}
													}],2],{}}]
											~EventHandler~
											{
												{
													"MouseClicked":>
													If[CurrentValue@"MouseClickCount">1,
														If[First@browserActiveFile=!=""&&FileExistsQ@First@browserActiveFile,
															browserOnClickFunction@First@browserActiveFile
															],
														If[First@browserActiveFile=!=""&&FileExistsQ@First@browserActiveFile,
															If[DirectoryQ@First@browserActiveFile,
																browserActiveDirectory=First@browserActiveFile,
																If[DirectoryName@First@browserActiveFile=!=browserActiveDirectory,
																	browserActiveDirectory=DirectoryName@First@browserActiveFile
																	]
																]
															]
														],
													"ReturnKeyDown":>
													If[First@browserActiveFile=!=""&&FileExistsQ@First@browserActiveFile,
														browserOnClickFunction@First@browserActiveFile
														],
													"BackspaceKeyDown":>(
														browserActiveDirectory=ParentDirectory@browserActiveDirectory;
														browserActiveFile={""}
														),
													PassEventsDown->True
													}
												},
											FilterRules[
												FilterRules[{ops},Except@ImageSize],
												Options@Pane
												],
											ImageSize->browserWidthHeight,
											AppearanceElements->None,
											ScrollPosition->
											With[{testPoint=
													Replace[
														First@browserWidthHeight,{
															Except@_Integer->0,
															w_:>(w/browserDirectoryFrameWidth)
															}]
													},
												If[
													(MatchQ[browserPaneNumber,Except@_Integer]||browserPaneNumber>testPoint)
													&&(FileNameDepth@browserActiveDirectory-1)>testPoint,
													{Scaled@1,0},
													{0,0}]
												]
											],
										TrackedSymbols:>{
											browserActiveDirectory,
											browserActiveFile,
											browserPaneNumber}
										(*,
										UpdateInterval\[Rule].01,
										TrackedSymbols\[RuleDelayed]{browserActiveDirectory}*)
										](*//Framed[#,
										FrameMargins\[Rule]{{2,0},{0,0}},
										FrameStyle\[Rule]Gray,
										RoundingRadius\[Rule]5,
										Background\[Rule]White
										]&*)
									},
								
								
								
								Grid[{
										If[OptionValue@"TopBar"//TrueQ,{
												topBar,SpanFromLeft},Nothing],
										{If[OptionValue@"SideBar"//TrueQ,sideBar,Nothing],panes}
										},
									Alignment->Right,
									Spacings->{0,.025}
									]//
								Framed[
									Framed[
										Style[#,
											"Text"
											],
										FrameStyle->None,
										FrameMargins->None,
										Background->White,
										RoundingRadius->5
										],
									FilterRules[
										FilterRules[{ops},Except@ImageSize],
										Options@Framed],
									FrameMargins->{{2,2},{2,2}},
									RoundingRadius->5,
									Background->Purple
									]&
								]
							]