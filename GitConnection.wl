(* ::Package:: *)

(* ::Section:: *)
(*Git Connections*)


BeginPackage["GitConnection`"]


Needs["EncodedCache`", 
	"https://raw.githubusercontent.com/b3m2a1/mathematica-tools/master/EncodedCache.wl"
	];


(* ::Subsection::Closed:: *)
(*Git*)


Git::usage=
	"A general head for all Git actions";


BeginPackage["`Package`Git`"];
	GitRun::usage="processRunDupe wrapper for git";
	GitCreate::usage="Creates a new repository";
	GitInit::usage="Initializes a git repository";
	GitClone::usage="Clones a repository";

	GitIgnore::usage="Adds to the .gitignore file for a directory";
	GitAdd::usage="Adds a file to the staging area of a  git repository";
	GitCommit::usage="Runs the commit command, using -a by default";
	GitStatus::usage="Gets the status of a repository";
	GitListTree::usage="Lists files on the tree";
	GitListTreeRecursive::usage="Lists files on the tree recursively";
	GitLog::usage="Gets the log of the git repo";
	GitConfig::usage="Sugar on the GitConfig tool";
	GitHelp::usage="Gets help from the git man pages";
	GitListRemotes::usage=
		"git remote -v show command";
	GitAddRemote::usage=
		"git remote add origin command";
	GitRemoveRemote::usage=
		"Removes remote";
	GitFetch::usage="git fetch";
	GitPush::usage="git push";
	GitPushOrigin::usage="git push origin master";
	GitPull::usage="git pull";
	GitPullOrigin::usage="git pull origin master";
	GitBranch::usage="git branch";

	GitRepositories::usage="Finds all the directories that support a git repo";
	$GitRepo::usage="The current git repo";
	GitRepo::usage=
		"Returns: 
the arg if it is a repo, 
a github URL if the arg is github:<repo>, 
else None";
	GitRepoQ::usage=
		"Returns true if the thing is a directory with a .git file";
	$GitActions::usage=
		"The known actions for Git";
EndPackage[];


(* ::Subsection::Closed:: *)
(*SVN*)


SVN::usage=
	"A general head for all SVN actions";


BeginPackage["`Package`SVN`"]
	SVNRun::usage="Runs SVN";
	SVNFileNames::usage="svn ls";
	SVNCheckOut::usage="Uses SVN to clone from a server";
	SVNExport::usage="Uses SVN to pull a single file from a server";
	$SVNActions::usage=
		"Known actions for SVN";
EndPackage[];


(* ::Subsection::Closed:: *)
(*GitHub*)


GitHub::usage=
	"A connection to the GitHub functionality";


BeginPackage["`Package`GitHub`"];
	$GitHubUserName::usage=
		"The user's github username";
	$GitHubPassword::usage=
		"The user's github password";
	FormatGitHubPath::usage="";
	GitHubPath::usage=
		"Represents a github path";
	GitHubRepoQ::usage=
		"Returns if the path could be a github repo";
	GitHubPathQ::usage="";
	GitHubCreate::usage="";
	GitHubDelete::usage="";
	GitHubDeployments::usage="";
	GitHubReleases::usage="";
	GitHubRepositories::usage="";
	$GitHubActions::usage=
		"A collection of known calls for the GitHub function";
	GitHubImport::usage=
		"Imports and converts GitHub JSON";
EndPackage[];


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Helpers*)


(* ::Subsubsection::Closed:: *)
(*processRunCopy*)


(* ::Text:: *)
(*An exact duplicate of ProcessRun to make this stand-alone*)


processRunDupe::pnfd="Program `` not found. 
$PATH = ``
$PWD = ``";
processRunDupe::fail="Process failed to run: ``"; 
processRunDupe::err="\nError in command \"``\":\n``";
processRunDupe~SetAttributes~HoldRest;


Options[processRunDupe]=
	Join[Options[RunProcess],{
		"ErrorHandler"->Automatic,
		"ParseFunction"->Identity
		}];
processRunDupe[cmds:{__},
	errorMessage:Except[_?OptionQ|_FilterRules]:processRunDupe::err,
	ops:OptionsPattern[]
	]:=
	Block[{capturedMessages={}},
		With[{r=
			GeneralUtilities`WithMessageHandler[
				RunProcess[cmds,
					FilterRules[{ops},Options@RunProcess]
					],
				AppendTo[capturedMessages,#]&
				],
			parseFunction=
				OptionValue["ParseFunction"],
			errorHander=
				Replace[OptionValue["ErrorHandler"],
					Automatic->
						Function[Null,
							Message[#,
								StringJoin@Riffle[cmds," "],
								#2
								],
							HoldFirst
							]
					]
			},
			If[r=!=$Failed,
				If[r["StandardError"]!="",
					errorHander[errorMessage,r["StandardError"]]
					];
				parseFunction@Replace[
					StringTrim@r["StandardOutput"],
					""->Null
					],
				Message[processRunDupe::fail,
					StringJoin@Riffle[
						Replace[cmds,
							(k_->v_):>
								(ToString@k<>"=="<>ToString@v),
							1],
						" "]
					];
				Replace[capturedMessages,
					Failure[RunProcess,<|
						"MessageTemplate":>
							RunProcess::pnfd,
						"MessageParameters" -> pars_
						|>]:>(
							Message[processRunDupe::pnfd,
								Sequence@@Join[pars,
									StringTrim@
										RunProcess[{$SystemShell,"-c","echo $PATH"},"StandardOutput"],
									StringTrim@
										RunProcess[{$SystemShell,"-c","echo $PWD"},"StandardOutput"]
									]
								];
							),
					1];
				$Failed
				]
			]
		];
processRunDupe[s_String,
	errorMessage:_MessageName:processRunDupe::err,
	ops:OptionsPattern[]]:=
	processRunDupe[{s},errorMessage,ops];


(* ::Subsubsection::Closed:: *)
(*packageAddAutocompletions*)


(* ::Text:: *)
(*Lazy dupe of the way my packages implement this*)


packageAddAutocompletions[s_, l_]:=
	If[$Notebooks&&
		Internal`CachedSystemInformation["FrontEnd","VersionNumber"]>10.0,
		FE`Evaluate[FEPrivate`AddSpecialArgCompletion[s->l]],
		$Failed
		];


(* ::Subsubsection::Closed:: *)
(*packageFilePath*)


(* ::Text:: *)
(*Something my packages implement. Obviously harder to have here...*)


packageFilePath[p__]:=
	FileNameJoin@{$UserBaseDirectory, "ApplicationData", "GitConnection", p};


(* ::Subsection:: *)
(*Constants*)


(* ::Subsubsection::Closed:: *)
(*GitRepo*)


If[Not@MatchQ[$GitRepo,_String?DirectoryQ],
	$GitRepo=None
	];


(* ::Subsection:: *)
(*Git*)


(* ::Subsubsection::Closed:: *)
(*GitRun*)


gitDoInDir[dir_String?DirectoryQ,cmd_]:=
	With[{d=ExpandFileName@dir},
			SetDirectory@d;
			With[{r=cmd},
				ResetDirectory[];
				r
				]
			];
gitDoInDir~SetAttributes~HoldRest;


processRunDupe;
Git::err=processRunDupe::err;


GitRun//Clear


GitRun[
	dir:_String?DirectoryQ|None|Automatic:None,
	cmd1_String?(Not@*DirectoryQ),
	cmd2___String
	]:=
	With[{d=Replace[dir,Automatic:>$GitRepo]},
		Replace[
			Git::err,
			_MessageName:>
				(Git::err=processRunDupe::err)
			];
		If[MatchQ[d,_String],
			processRunDupe[
				{"git",cmd1, cmd2}//
					Map[If[FileExistsQ@#, ExpandFileName@#, #]&],
				Git::err, 
				ProcessDirectory->ExpandFileName@d
				],
			processRunDupe[{"git",cmd1, cmd2}, 
				Git::err
				]
			]
		];
GitRun[
	dir:_String?DirectoryQ|None|Automatic:None,
	cmd1:_String?(Not@*DirectoryQ)|{__String},
	cmd2:_String|{__String}...
	]:=
	With[{
		d=Replace[dir,Automatic:>$GitRepo],
		cmdBits=
			Flatten[
				Riffle[Prepend["git"]@*Flatten@*List/@{cmd1, cmd2},"\n\n"],
				1
				]//
				Map[If[FileExistsQ@#, ExpandFileName@#, #]&]
		},
		Replace[
			Git::err,
			_MessageName:>
				(Git::err=processRunDupe::err)
			];
		If[MatchQ[d,_String],
			processRunDupe[
				cmdBits,
				Git::err,
				ProcessDirectory->ExpandFileName@d
				],
			processRunDupe[
				cmdBits,
				Git::err
				]
			]
		];
Git::nodir="`` is not a valid directory";
Git::nrepo="`` not a git repository";


(* ::Subsubsection::Closed:: *)
(*GitPrepParams*)


$GitParamMap=<||>;


GitPrepParamVals[ops_]:=
	Replace[
		ops,
		{
			(h:Rule|RuleDelayed)[s_String, n_?NumericQ]:>
				s->ToString[n],
			(h:Rule|RuleDelayed)[s_String, n_?DateObjectQ]:>
				s->ToString@UnixTime[n],
			(h:Rule|RuleDelayed)[s_String, q_Quantity]:>
					s->
					ToString@QuantityMagnitude@
						If[CompatibleUnitQ[q, "Seconds"], 
							UnitConvert[q, "Seconds"],
							q
							],
			(RuleDelayed[s_String, v_]):>
				Rule[s, v]
			},
		1
		];
GitPrepParams[ops_, map_]:=
	Replace[
		GitPrepParamVals@Flatten@Normal@{ops},
		{
			(Rule|RuleDelayed)[s_String, True]:>
				Replace[
					Lookup[map, s, None],
					{
						p_String?(StringLength[#]==1&):>
							"-"<>p,
						p_String:>
							"--"<>p,
						{p_String, join_String}:>
							"--"<>p<>join<>"true",
						_->Nothing
						}
					],
			(Rule|RuleDelayed)[s_String, v_String]:>
				Replace[
					Lookup[map, s, None],
					{
						p_String?(StringLength[#]==1&):>
							"-"<>p,
						p_String:>
							"--"<>p<>"="<>v,
						{p_String, join_String}:>
							"--"<>p<>join<>v,
						_->Nothing
						}
					],
			_->Nothing
			},
		1
		];
GitPrepParams[fn_, ops_, map_]:=
	GitPrepParams[Flatten[{Options@fn, ops}], map]


(* ::Subsubsection::Closed:: *)
(*GitRepoQ*)


GitRepoQ[d:(_String|_File)?DirectoryQ]:=
	DirectoryQ@FileNameJoin@{d,".git"};
GitRepoQ[_]:=False


(* ::Subsubsection::Closed:: *)
(*GitAddGitIgnore*)


GitAddGitIgnore[
	dirB:_String?DirectoryQ|Automatic:Automatic,
	patterns:_String|{__String}:
		{
			".DS_Store"
			}
	]:=
	With[
		{dir=Replace[dirB, Automatic:>Directory[]]},
		If[GitRepoQ@dir,
			With[{f=OpenWrite[FileNameJoin@{dir,".gitignore"}]},
				WriteLine[f,
					StringJoin@Riffle[Flatten@{patterns},"\n"]
					];
				Close@f
				],
			$Failed
			]
		];


(* ::Subsubsection::Closed:: *)
(*GitAddGitExclude*)


GitAddGitExclude[
	dirB:_String?DirectoryQ|Automatic:Automatic,
	patterns:_String|{__String}:{"*.DS_Store"}
	]:=
	With[
		{dir=Replace[dirB, Automatic:>Directory[]]},
		If[GitRepoQ@dir,
			If[Not@DirectoryQ@FileNameJoin@{dir,".git","info"},
				CreateDirectory[FileNameJoin@{dir,".git","info"}]
				];
			With[{f=OpenWrite[FileNameJoin@{dir,".git","info","exclude"}]},
				WriteLine[f,
					StringJoin@Riffle[Flatten@{patterns},"\n"]
					];
				Close@f
				],
			$Failed
			]
		]


(* ::Subsubsection::Closed:: *)
(*GitInit*)


GitCreate[dir_String?(DirectoryQ@*DirectoryName)]:=
	With[{d=ExpandFileName@dir},
		Quiet@CreateDirectory@d;
		GitInit[d]
		];


GitInit[
	dir:_String?DirectoryQ|Automatic:Automatic,
	ignorePats:{___String}|None:None,
	excludePats:{___String}|None:None
	]:=
	With[{r=GitRun[dir, "init"]},
		If[ignorePats=!=None, 
			GitAddGitIgnore[dir, ignorePats]
			];
		If[excludePats=!=None,
			GitAddGitExclude[dir, excludePats];
			];
		r
		]


(* ::Subsubsection::Closed:: *)
(*GitClone*)


GitClone//Clear


$GitParamMap["Clone"]=
	{
		"Local"->"local",
		"NoHardlinks"->"no-hardlinks",
		"Shared"->"shared",
		"Reference"->"reference",
		"Dissociate"->"dissociate",
		"Quiet"->"quiet",
		"Verbose"->"verbose",
		"Progress"->"progress",
		"NoCheckout"->"no-checkout",
		"Bare"->"bare",
		"Mirror"->"mirror",
		"Origin"->"origin",
		"Branch"->"branch",
		"UploadPack"->"upload-pack",
		"Template"->"template",
		"Config"->"config",
		"Depth"->"depth",
		"ShallowSince"->"shallow-since",
		"ShallowExclude"->"shallow-exclude",
		"SingleBranch"->"single-branch",
		"NoSingleBranch"->"no-single-branch",
		"NoTags"->"no-tags",
		"RecurseSubmodules"->"recurse-submodules",
		"ShallowSubmodules"->"shallow-submodules",
		"NoShallowSubmodules"->"no-shallow-submodules",
		"SeparateGitDir"->"separate-git-dir",
		"Jobs"->"jobs"
		};


Options[GitClone]=
	Thread[Keys[$GitParamMap["Clone"]]->Automatic];
GitClone[
	repo:_String|_File|_URL,
	dir:_String|_File|Automatic:Automatic,
	overrwriteTarget:True|False:False,
	o___?OptionQ
	]:=
	With[{
		r=
			Replace[repo,{
				File[d_]:>
					If[GitRepoQ@d,
						d,
						GitRepo
						],
				URL[d_]:>
					d
				}],
		d=
			Replace[dir,
				Automatic:>
					FileNameJoin@{
						$TemporaryDirectory,
						Switch[repo,
							_String|_File,
								FileBaseName@repo,
							_URL,
								URLParse[repo,"Path"][[-1]]
							]
						}
				]
			},
		If[overrwriteTarget,
			Quiet@DeleteDirectory[d,DeleteContents->True]
			];
		CreateDirectory[d, CreateIntermediateDirectories->True];
		GitRun[d, "clone", r, d,
			Sequence@@
				GitPrepParams[
					GitClone,
					{o}, 
					$GitParamMap["Clone"]
					]
			];
		d
		];


(* ::Subsubsection::Closed:: *)
(*GitIgnore*)


GitIgnore[dir:_String?DirectoryQ|Automatic:Automatic,filePatterns:{___}]:=
	With[{d=Replace[dir,Automatic:>$GitRepo]},
		If[MatchQ[d,_String?DirectoryQ],
			With[{file=OpenAppend@FileNameJoin@{d,".gitignore"}},
				Do[
					WriteLine[file,f],
					{f,filePatterns}
					];
				Close@file;
				GitRun[d,"add",".gitignore"]
				],
			Message[GitRun::nodir,d];$Failed
			]
		];


(* ::Subsubsection::Closed:: *)
(*GitAdd*)


GitAdd[dir:_String?DirectoryQ|Automatic:Automatic,files___]:=
	GitRun[dir,"add",files];


(* ::Subsubsection::Closed:: *)
(*GitRemove*)


GitRemove[dir:_String?DirectoryQ|Automatic:Automatic,files___]:=
	GitRun[dir,"rm",files];


(* ::Subsubsection::Closed:: *)
(*GitRemoveRecursive*)


GitRemoveRecursive[dir:_String?DirectoryQ|Automatic:Automatic,files___]:=
	GitRun[dir,"rm","-r",files];


(* ::Subsubsection::Closed:: *)
(*GitRemoveCached*)


GitRemoveCached[dir:_String?DirectoryQ|Automatic:Automatic,files___]:=
	GitRun[dir,"rm",
		"--cached",
		files];


(* ::Subsubsection::Closed:: *)
(*GitRemoveCachedRecursive*)


GitRemoveCachedRecursive[dir:_String?DirectoryQ|Automatic:Automatic,files___]:=
	GitRun[dir,"rm",
		"-r",
		"--cached",
		files
		];


(* ::Subsubsection::Closed:: *)
(*GitBranch*)


GitBranch[dir:_String?DirectoryQ, spec__]:=
	GitRun[dir,"branch", spec];


(* ::Subsubsection::Closed:: *)
(*GitCommit*)


Options[GitCommit]={Message->"Commited via Mathematica"};
GitCommit[dir:_String?DirectoryQ|Automatic:Automatic,
	opts___String,
	OptionsPattern[]]:=
	With[{squargs=
		If[
			Not@MemberQ[{opts},"-m"],
			Join[{opts},{"-m",OptionValue@Message}],
			{opts}]},
		GitRun[dir,"commit",Sequence@@squargs]
		];


(* ::Subsubsection::Closed:: *)
(*Git status*)


GitLog[dir:_String?DirectoryQ|Automatic:Automatic,
		pFlag_:"-p",entries_:"-2",opts___]:=
	GitRun[dir,"log",opts];


GitStatus[dir:_String?DirectoryQ|Automatic:Automatic]:=
	GitRun[dir,"status"];


(* ::Subsubsection::Closed:: *)
(*Git config and help*)


GitConfig[setting:_String:"--global",opts___String]:=
	GitRun["config",setting,opts];
GitConfig[setting:_String:"--global","Username"->name_]:=
	GitConfig[setting,"user.name",name];
GitConfig[setting:_String:"--global","UserEmail"->email_]:=
	GitConfig[setting,"user.email",email];
GitConfig[setting:_String:"--global","TextEditor"->editor_]:=
	GitConfig[setting,"core.editor",editor]
GitConfig[setting:_String:"--global",opts__Rule]:=
	StringJoin@
		Riffle[
			Cases[
				Table[
					GitConfig[setting,opt],
					{opt,{opts}}
					],
				_String
				],
			"\n"
			];


GitHelp[thing_String]:=
	GitRun["help",thing];


(* ::Subsubsection::Closed:: *)
(*Repository finding and stuff*)


GitRepositories[dirs:{(_String?DirectoryQ)..}|_String?DirectoryQ,depth:_Integer|\[Infinity]:2]:=
	ParentDirectory/@FileNames[".git",dirs,depth];


(* ::Subsubsection::Closed:: *)
(*GitRepo*)


(*Options declared later*)
GitRepo[repo_String?(StringMatchQ["github:*"]),ops:OptionsPattern[]]:=
	GitHubRepo[StringTrim[repo,"github:"],{"Username"->"",ops}];
GitRepo[repo:(_String|_File)?DirectoryQ]:=
	Replace[
		GitRepositories[repo,1],{
		{d_,___}:>d,
		_:>None
		}];
GitRepo[r:_String|_URL]:=
	With[{u=URLParse@r},
		If[u["Scheme"]===None,
			If[u["Domain"]===None,
				If[Length@u["Path"]<2||!StringMatchQ[u["Path"]//First,"*.*"],
					None,
					URLBuild@Append[u,"Scheme"->"https"]
					],
				URLBuild@Append[u,"Scheme"->"https"]
				],
			URLBuild@u
			]
		];
GitRepo[r_]:=None;
GitRepoQ[r:(_String|_File)?DirectoryQ]:=
	(GitRepo@r=!=None);


(* ::Subsubsection::Closed:: *)
(*ListRemotes*)


GitListRemotes[dir:_String?DirectoryQ|Automatic:Automatic]:=
	GitRun[dir,"remote","-v","show"]


(* ::Subsubsection::Closed:: *)
(*AddRemote*)


GitAddRemote//Clear


GitAddRemote[
	dir:_String?DirectoryQ|Automatic:Automatic,
	remoteName:_String?(Not@*DirectoryQ):"origin",
	remote:_String|_URL
	]:=
	GitRun[dir,
		"remote","add",remoteName,
		URLBuild@remote
		];


(* ::Subsubsection::Closed:: *)
(*RemoveRemote*)


GitRemoveRemote[
	dir:_String?DirectoryQ|Automatic:Automatic,
	remote:_String|_URL]:=
	GitRun[dir,
		"remote","rm","origin"
		];


(* ::Subsubsection::Closed:: *)
(*GitSetRemote*)


GitSetRemote[
	dir:_String?DirectoryQ|Automatic:Automatic,
	remoteName:_String?(Not@*DirectoryQ):"origin",
	origin:(_String|_GitHubPath)?GitHubRepoQ
	]:=
	Quiet@
		Check[
			GitAddRemote[dir, origin],
			GitRemoveRemote[dir, origin];
			GitAddRemote[dir, origin]
			];


(* ::Subsubsection::Closed:: *)
(*GitRealignRemotes*)


GitRealignRemotes[
	dir:_String?DirectoryQ|Automatic:Automatic,
	remoteName:_String?(Not@*DirectoryQ):"origin",
	branchName:_String?(Not@*DirectoryQ):"master"
	]:=
	(
		Git["Fetch", dir];
		Git["Reset", dir, URLBuild@{remoteName, branchName}];
		Git["Checkout", dir, URLBuild@{remoteName, branchName}];
		)


(* ::Subsubsection::Closed:: *)
(*ReattachHead*)


GitReattachHead[
	dir:_String?DirectoryQ|Automatic:Automatic
	]:=
	With[{uuid=CreateUUID[]},
		Git["Branch", dir, uuid];
		Git["Checkout", dir, uuid];
		Git["Branch", dir, "-f", "master", uuid];
		Git["Checkout", dir, "master"];
		Git["Branch", dir, "-d", uuid];
		]


(* ::Subsubsection::Closed:: *)
(*Push*)


Options[GitPush]={
	"Username"->
		None,
	"Password"->
		None,
	"Force"->False
	};
GitPush[
	dir:_String?DirectoryQ,
	loc_String,
	branch:_String:"master",
	ops:OptionsPattern[]]:=
	GitRun[dir,
		"push",
		If[TrueQ@OptionValue["Force"],
			"-f",
			Sequence@@{}
			],
		loc,
		branch];


(* ::Subsubsection::Closed:: *)
(*Fetch*)


GitFetch[
	dir:_String?DirectoryQ
	]:=
	GitRun[
		dir,
		"fetch"
		];


(* ::Subsubsection::Closed:: *)
(*Reset*)


GitReset[
	dir:_String?DirectoryQ,
	src___
	]:=
	GitRun[
		dir,
		"reset",
		src
		];


(* ::Subsubsection::Closed:: *)
(*Checkout*)


GitCheckout//Clear


$GitParamMap["Checkout"]=
	{
		"Quiet"->"q",
		"MakeBranch"->"b",
		"Track"->"t",
		"Progress"->"progress",
		"Force"->"f",
		"Ours"->"ours",
		"Theirs"->"theirs",
		"NoTrack"->"notrack",
		"Log"->"l",
		"Detach"->"Detach",
		"Orphan"->"orphan",
		"IgnoreSkipWorktreeBits"->
			"ignore-skip-worktree-bits",
		"Conflict"->"conflict",
		"Patch"->"p",
		"IgnoreOtherWorktrees"->"ignore-other-worktrees",
		"RecurseSubmodules"->"recurse-submodules"
		};


Options[GitCheckout]=
	{
		"Quiet"->Automatic,
		"MakeBranch"->Automatic,
		"Track"->Automatic,
		"Progress"->Automatic,
		"Force"->Automatic,
		"Ours"->Automatic,
		"Theirs"->Automatic,
		"NoTrack"->Automatic,
		"Log"->Automatic,
		"Detach"->Automatic,
		"Orphan"->Automatic,
		"Conflict"->Automatic,
		"Patch"->Automatic,
		"IgnoreSkipWorktreeBits"->Automatic,
		"IgnoreOtherWorktrees"->Automatic,
		"RecurseSubmodules"->Automatic
		};
GitCheckout[
	dir:_String?DirectoryQ,
	args__String,
	o:OptionsPattern[]
	]:=
	GitRun[dir,
		"checkout",
		Sequence@@
			GitPrepParams[
				GitCheckout,
				{o}, 
				$GitParamMap["Checkout"]
				],
		args
		]


(* ::Subsubsection::Closed:: *)
(*Pull*)


Options[GitPull]={
	"Username"->
		None,
	"Password"->
		None
	};
GitPull[
	dir:_String?DirectoryQ,
	loc_String,
	branch:_String:"master",
	ops:OptionsPattern[]]:=
	GitRun[dir,
		"pull",
		loc,
		branch];


(* ::Subsubsection::Closed:: *)
(*PullOrigin*)


GitPullOrigin[dir:_String?DirectoryQ|Automatic:Automatic]:=
	GitPull[dir,"origin","master"]


(* ::Subsubsection::Closed:: *)
(*PushOrigin*)


GitPushOrigin[dir:_String?DirectoryQ|Automatic:Automatic,
	force:True|False:False
	]:=
	GitPush[dir,
		If[force,"-f",Sequence@@{}],
		"origin",
		"master"
		];


(* ::Subsubsection::Closed:: *)
(*GetPushURL*)


GitGetPushURL[
	dir:_String?DirectoryQ|Automatic:Automatic,
	rem:_String?(Not@*DirectoryQ):"origin"
	]:=
	With[
		{
			rems=
				#[[1]]->#[[2]]&/@
					Cases[
						Partition[
							Append[""]@
							StringSplit[
								Replace[
									Git["ListRemotes", dir],
									Except[_String]->""
									]
								],
							3
							],
						{_, _, _?(StringContainsQ["push"])}
						]//Association
			},
		If[Length@rems>0,
			rem->rems[rem],
			URL@GitHubPath[FileBaseName@Replace[dir, Automatic:>rem]]
			]
		]


(* ::Subsubsection::Closed:: *)
(*GetFetchURL*)


GetFetchURL//Clear


GitGetFetchURL[
	dir:_String?DirectoryQ|Automatic:Automatic,
	rem:_String?(Not@*DirectoryQ):"origin"
	]:=
	With[
		{
			rems=
				#[[1]]->#[[2]]&/@
					Cases[
						Partition[
							Append[""]@
							StringSplit[
								Replace[Git["ListRemotes", dir],
									Except[_String]->""
									]
								],
							3
							],
						{_, _, _?(StringContainsQ["fetch"])}
						]//Association
			},
		If[Length@rems>0,
			rem->rems[rem],
			URL@GitHubPath[FileBaseName@Replace[dir, Automatic:>rem]]
			]
		]


(* ::Subsubsection::Closed:: *)
(*ListTree*)


Options[GitListTree]=
	{
		"NameOnly"->True
		};
GitListTree[
	dir:_String?DirectoryQ|Automatic:Automatic,
	branch:_String:"master",
	ops:OptionsPattern[]
	]:=
	StringSplit[
		GitRun[dir,"ls-tree",branch,
			If[OptionValue["NameOnly"]//TrueQ,
				"--name-only",
				Sequence@@{}
				]
			],
		"\n"
		];


(* ::Subsubsection::Closed:: *)
(*ListTreeRecursive*)


Options[GitListTreeRecursive]=
	Options@GitListTree;
GitListTreeRecursive[
	dir:_String?DirectoryQ|Automatic:Automatic,
	branch:_String:"master",
	ops:OptionsPattern[]
	]:=
	StringSplit[
		GitRun[dir,"ls-tree","-r",branch,
			If[OptionValue["NameOnly"]//TrueQ,
				"--name-only",
				Sequence@@{}
				]
			],
		"\n"
		]


(* ::Subsubsection::Closed:: *)
(*RefLog*)


GitRefLog[
	dir:_String?DirectoryQ|Automatic:Automatic,
	args___
	]:=
	GitRun[dir,"reflog"]


(* ::Subsubsection::Closed:: *)
(*RefLogExpire*)


GitRefLogExpire[
	dir:_String?DirectoryQ|Automatic:Automatic,
	expireTime_:"--all"
	]:=
	GitRun[dir,"reflog","expire",expireTime]


(* ::Subsubsection::Closed:: *)
(*Clean*)


Options[GitClean]=
	{
		"reflogExpire"->Automatic,
		"reflogExpireUnreachable"->Automatic,
		"rerereresolved"->Automatic,
		"rerereunresolved"->Automatic,
		"pruneExpire"->Automatic
		}
GitClean[
	dir:_String?DirectoryQ|Automatic:Automatic,
	args___String,
	ops:OptionsPattern[]
	]:=
	With[
		{
			conf=
				Map[
					StringJoin@
						Prepend[Insert[List@@ToString/@#," ",2],"--gc."]&, 
					Flatten@{ops}
					]
				},
			GitRun[dir,"gc",args,Sequence@@conf]
		]


(* ::Subsubsection::Closed:: *)
(*GitCleanEverything*)


(*(* Taken from here: https://stackoverflow.com/a/14729486 *)
GitCleanEverything[
	dir:_String?DirectoryQ|Automatic:Automatic,
	args___
	]:=
	GitClean[dir,"\"$@\"",args,
		{
			"reflogExpire"\[Rule]0,
			"reflogExpireUnreachable"\[Rule]0,
			"rerereresolved"\[Rule]0,
			"rerereunresolved"\[Rule]0,
			"pruneExpire"\[Rule]"now"
			}
		]*)


(* ::Subsubsection::Closed:: *)
(*GitFilterBranch*)


GitFilterBranch//Clear


GitFilterBranch[
	dir:_String?DirectoryQ|Automatic:Automatic,
	filterType_String?(StringStartsQ["--"]),
	filterCMD_String,
	args___
	]:=
	GitRun[dir,"filter-branch",
		If[StringQ@branch,branch,Sequence@@{}],
		filterType, 
		"''``''"~TemplateApply~filterCMD,
		args
		];


(* ::Subsubsection::Closed:: *)
(*GitFilterTree*)


GitFilterTree//Clear


GitFilterTree[
	dir:_String?DirectoryQ|Automatic:Automatic,
	filterCMD_String,
	args___
	]:=
	GitFilterBranch[dir, "--tree-filter",filterCMD, args];


(* ::Subsubsection::Closed:: *)
(*Prune*)


$GitParamMap["Branch"]=
	{
		"DryRun"->"dry-run",
		"Verbose"->"verbose",
		"Expire"->{"expire", " "}
		};


Options[GitPrune]=
	Thread[$GitParamMap["Branch"]->Automatic];
GitPrune[
	dir:_String?DirectoryQ|Automatic:Automatic,
	args___String,
	ops:OptionsPattern[]
	]:=
	GitRun[dir,"prune",
		Sequence@@
			GitPrepParams[
				GitPrune,
				{ops}, 
				$GitParamMap["Prune"]
				],
		args
		]


(* ::Subsubsection::Closed:: *)
(*Branch*)


$GitParamMap["Branch"]=
	{
		"Delete"->"delete",
		"ForceDelete"->"D",
		"CreateReflog"->"create-reflog",
		"Force"->"force",
		"Move"->"move",
		"ForceMOve"->"M",
		"Color"->"color",
		"NoColor"->"no-color",
		"IgnoreCase"->"ignore-case",
		"NoColumn"->"no-column",
		"Remotes"->"remotes",
		"All"->"all",
		"List"->"list",
		"Verbose"->"verbose",
		"Quiet"->"quiet",
		"Abbrev"->"abbrev",
		"NoAbbrev"->"no-abbrev",
		"Track"->"track",
		"NoTrack"->"no-track",
		"SetUpstream"->"set-upstream",
		"SetUpstreamTo"->"set-upstream-to",
		"UnsetUpstream"->"unset-upstream",
		"EditDescription"->"edit-description",
		"Contains"->"contains",
		"NoContains"->"no-contains",
		"Merged"->"merged",
		"NoMerged"->"no-merged",
		"Sort"->"sort",
		"PointsAt"->"points-at",
		"Format"->"format"
		};
Options[GitBranch]=
	Thread[Keys[$GitParamMap["Branch"]]->Automatic];
GitBranch[
	dir:_String?DirectoryQ|Automatic:Automatic,
	args___String,
	o___?OptionQ
	]:=
	GitRun[dir,"branch",
		Sequence@@
			GitPrepParams[
				GitBranch,
				{o}, 
				$GitParamMap["Branch"]
				],
		args
		]


(* ::Subsubsection::Closed:: *)
(*ShowBranch*)


$GitParamMap["ShowBranch"]=
	{
		"Remotes"->"remotes",
		"All"->"all",
		"Current"->"current",
		"TopoOrder"->"topo-order",
		"DateOrder"->"date-order",
		"Sparse"->"sparse",
		"More"->"more",
		"List"->"list",
		"MergeBase"->"merge-base",
		"Independent"->"independent",
		"NoName"->"no-name",
		"Sha1Name"->"sha1-name",
		"Topics"->"topics",
		"Reflog"->"reflog",
		"Color"->"color",
		"NoColor"->"no-color"
		};
Options[GitShowBranch]=
	Thread[Keys[$GitParamMap["ShowBranch"]]->Automatic];
GitShowBranch[
	dir:_String?DirectoryQ|Automatic:Automatic,
	args___String,
	o___?OptionQ
	]:=
	GitRun[dir,"show-branch",
		Sequence@@
			GitPrepParams[
				GitShowBranch,
				{o}, 
				$GitParamMap["ShowBranch"]
				],
		args
		]


(* ::Subsubsection::Closed:: *)
(*ListBranches*)


Options[GitListBranches]=
	Options[GitShowBranch];
GitListBranches[
	dir:_String?DirectoryQ|Automatic:Automatic,
	args___String,
	o___?OptionQ
	]:=
	GitShowBranch[dir, args, "All"->True]


(* ::Subsubsection::Closed:: *)
(*WipeTheSlate*)


GitWipeTheSlate//Clear


(* Take from here: https://stackoverflow.com/a/26000395 *)
GitWipeTheSlate[
	dir:_String?DirectoryQ|Automatic:Automatic
	]:=
	(
		GitCheckout[dir, "--orphan", "latest_branch"];
		GitAdd[dir, "-A"];
		GitCommit[dir, "-a",
			Message->"Wiped the slate clean"
			];
		GitBranch[dir, "-D", "master"];
		GitBranch[dir, "-m", "master"];
		)


(* ::Subsubsection::Closed:: *)
(*Help*)


GitHelp[cmd_String]:=
	With[{s=GitRun["help", cmd]},
		StringReplace[s, 
			{
				(* There are sneaky \\.08 chars in here! Beware! *)
				l:(a:WordCharacter~~"\.08"~~a_):>
					a,
				l:(Repeated["_\.08"~~Except[WhitespaceCharacter]]):>
					StringJoin@StringTake[l, List/@Range[3, StringLength[l], 3]]
				}
			]
		]


GitHelpPart[cmd_, delim1_, delim2_]:=
	With[{h=GitHelp[cmd]},
		Replace[
			StringCases[h, 
				Shortest[delim1~~t:__~~delim2]:>
				With[{ws=
						MinimalBy[
							StringCases[t, (StartOfString|StartOfLine)~~Except["\n", Whitespace]], 
							StringLength
							]},
					StringDelete[t,
						(StartOfString|StartOfLine)~~Apply[Alternatives, ws]
						]
					]
				],
			{
				{s_}:>
					StringTrim@s,
				_->None
				}
			]
		]


(* ::Subsubsection::Closed:: *)
(*Synopsis*)


GitHelpSynopsis[cmd_String]:=
	GitHelpPart[cmd, "SYNOPSIS", "DESCRIPTION"]


(* ::Subsubsection::Closed:: *)
(*Description*)


GitHelpDescription[cmd_String]:=
	GitHelpPart[cmd, "DESCRIPTION", "OPTIONS"]


(* ::Subsubsection::Closed:: *)
(*Options*)


GitHelpOptions[cmd_String]:=
	GitHelpPart[cmd, "OPTIONS", 
		("\n"~~Except[WhitespaceCharacter])|EndOfString
		]


(* ::Subsubsection::Closed:: *)
(*Flags*)


GitHelpFlags[cmd_String]:=
	StringCases[
		GitHelpOptions[cmd],
		Shortest[StartOfLine~~"-"~~__~~EndOfLine]
		]


(* ::Subsubsection::Closed:: *)
(*FlagMap*)


GitHelpFlagMap[cmd_]:=
	StringTrim[
		StringSplit[
			Map[
				If[StringContainsQ[#, "--["],
					Sequence@@{
						StringReplace[#, Shortest["--["~~__~~"]"]:>"--"],
						StringReplace[#, Shortest["--["~~t__~~"]"]:>"--"<>t]
						},
					#
					]&,
				Flatten@
				Map[
					Take[
						MaximalBy[
							Select[
								StringTrim@#, 
								StringStartsQ[
									Repeated["-", {1, 2}]~~
										Except["-"]
									]
								], 
							StringLength
							],
						UpTo[1]
						]&,
					StringSplit[GitHelpFlags[cmd],  ","]
					]
				],
			"["|" "|"="
			]//Map[First],
		"-"..|Whitespace
		]//Map[
		StringReplace[ToUpperCase[StringTake[#, 1]]<>StringDrop[#, 1],
			"-"~~s_:>ToUpperCase[s]
			]->#&
		]


(* ::Subsubsection::Closed:: *)
(*Git*)


$GitActions=
	<|
		"Repo"->
			GitRepo,
		"RepoQ"->
			GitRepoQ,
		"Create"->
			GitCreate,
		"Init"->
			GitInit,
		"Clone"->
			GitClone,
		"AddGitIgnore"->
			GitAddGitIgnore,
		"AddGitExclude"->
			GitAddGitExclude,
		"Add"->
			GitAdd,
		"Remove"->
			GitRemove,
		"RemoveCached"->
			GitRemoveCached,
		"RemoveRecursive"->
			GitRemoveRecursive,
		"RemoveCachedRecursive"->
			GitRemoveCachedRecursive,
		"Commit"->
			GitCommit,
		"Branch"->
			GitBranch,
		"ShowBranch"->
			GitShowBranch,
		"ListBranches"->
			GitListBranches,
		"ListRemotes"->
			GitListRemotes,
		"AddRemote"->
			GitAddRemote,
		"RemoveRemote"->
			GitRemoveRemote,
		"RealignRemotes"->
			GitRealignRemotes,
		"ReattachHead"->
			GitReattachHead,
		"Fetch"->
			GitFetch,
		"Reset"->
			GitReset,
		"Checkout"->
			GitCheckout,
		"Pull"->
			GitPull,
		"PullOrigin"->
			GitPullOrigin,
		"Push"->
			GitPush,
		"PushOrigin"->
			GitPushOrigin,
		"GetPushURL"->
			GitGetPushURL,
		"GetFetchURL"->
			GitGetFetchURL,
		"Repositories"->
			GitRepositories,
		"Log"->
			GitLog,
		"Status"->
			GitStatus,
		"ListTree"->
			GitListTree,
		"ListTreeRecursive"->
			GitListTreeRecursive,
		"RefLog"->
			GitRefLog,
		"RefLogExpire"->
			GitRefLogExpire,
		"Clean"->
			GitClean,
		"WipeTheSlate"->
			GitWipeTheSlate,
		"FilterBranch"->
			GitFilterBranch,
		"FilterTree"->
			GitFilterTree,
		"Config"->
			GitConfig,
		"Help"->
			GitHelp,
		"HelpSynopsis"->
			GitHelpSynopsis,
		"HelpDescription"->
			GitHelpDescription,
		"HelpOptions"->
			GitHelpOptions,
		"HelpFlags"->
			GitHelpFlags,
		"HelpFlagMap"->
			GitHelpFlagMap
		|>;


$gitactions:=
	KeyMap[ToLowerCase]@$GitActions;


packageAddAutocompletions[
	"Git",
	{
		Keys@$GitActions,
		{"Options", "Function"}
		}
	]


Git//Clear


Git[
	command_?(KeyMemberQ[$gitactions,ToLowerCase@#]&),
	"Options"
	]:=
	Options@$gitactions[ToLowerCase[command]];
Git[
	command_?(KeyMemberQ[$gitactions,ToLowerCase@#]&),
	"Function"
	]:=
	$gitactions[ToLowerCase[command]];
Git[
	command_?(KeyMemberQ[$gitactions,ToLowerCase@#]&),
	args___
	]:=
	With[{cmd=$gitactions[ToLowerCase[command]]},
		With[{r=cmd[args]},
			r/;Head[r]=!=cmd
			]
		];
Git::badcmd=
	"Couldn't execute command `` with parameters ``";
Git[
	cmd_String,
	args___
	]:=
	With[{r=GitRun[cmd,args]},
		If[Head[r]===GitRun,
			Message[Git::badcmd, cmd, {args}]
			];
		r/;Head[r]=!=GitRun
		];


(* ::Subsection:: *)
(*SVN*)


(* ::Subsubsection::Closed:: *)
(*Run*)


Options[SVNRun]=
	Normal@Merge[{
		Options@processRunDupe,
		"TrustServer"->False
		},
		First
		];
SVNRun[cmd_,
	kwargs:(_Rule|_RuleDelayed|_String)...,
	repo_String,
	others:(_Rule|_RuleDelayed|_String)...,
	ops:OptionsPattern[]]:=
	processRunDupe[
		{
			"svn",
			cmd,
			kwargs,
			If[OptionValue@"TrustServer","--trust-server-cert",Nothing],
			If[FileExistsQ@repo,ExpandFileName@repo,repo],
			others
			},
		Evaluate[
			Sequence@@
			FilterRules[{ops},
					Options@processRunDupe
					]
			]
		];


(* ::Subsubsection::Closed:: *)
(*FileNames*)


Options[SVNFileNames]=
	Options@SVNRun;
SVNFileNames[repo_,ops:OptionsPattern[]]:=
	Replace[SVNRun["ls",repo,ops],
		fn_String:>
			With[{lines=StringSplit[fn,"\n"]},
				If[FileExistsQ@repo,
					FileNameJoin@{repo,#}&/@lines,
					URLBuild@{repo,#}&/@lines
					] 
				]
		];


(* ::Subsubsection::Closed:: *)
(*CheckOut*)


Options[SVNCheckOut]=
	Options@SVNRun;
SVNCheckOut[
	repo_,
	dir:_String|Automatic:Automatic,
	ops:OptionsPattern[]]:=
	With[{pulldir=
		Replace[dir,{
			s_String?(Not@*FileExistsQ):>
				(If[FileExtension@s=="",
					CreateDirectory@s
					];
					s),
			Automatic:>
				With[{d=FileNameJoin@{$TemporaryDirectory,FileNameTake@repo}},
					If[FileExtension@d=="",
						Quiet@DeleteDirectory[d,DeleteContents->True];
						CreateDirectory@d
						];
					d
					]
			}]},
	SVNRun["checkout",
		repo,
		ExpandFileName@pulldir,
		ops
		];
	pulldir
	]


(* ::Subsubsection::Closed:: *)
(*Export*)


Options[SVNExport]=
	Normal@Merge[{
		Options@SVNRun,
		OverwriteTarget->False
		},
		Last];
SVNExport[repo_,file:_String|Automatic:Automatic,ops:OptionsPattern[]]:=
	With[{f=
		Replace[file,{
				Automatic:>
					FileNameJoin@{$TemporaryDirectory,FileNameTake@repo}
				}]},
		If[OptionValue@OverwriteTarget,Quiet@DeleteFile@f];
		SVNRun["export",
			repo,
			ExpandFileName@f,
			FilterRules[{ops},Options@SVNRun]
			];
		f
		]


(* ::Subsubsection::Closed:: *)
(*SVN*)


$SVNActions=
	<|
		"FileNames"->
			SVNFileNames,
		"CheckOut"->
			SVNCheckOut,
		"Export"->
			SVNExport
		|>;


$svnactions:=
	KeyMap[ToLowerCase]@$SVNActions


packageAddAutocompletions[
	"SVN",
	{
		Keys[$SVNActions]
		}
	]


SVN[
	command_?(KeyMemberQ[$svnactions,ToLowerCase@#]&),
	args___
	]:=
	With[{cmd=$svnactions[ToLowerCase[command]]},
		With[{r=cmd[args]},
			r/;Head[r]=!=cmd
			]
		];
SVN[
	cmd_String,
	args___
	]:=
	SVNRun[cmd,args];


(* ::Subsection:: *)
(*GitHub*)


(* ::Subsubsection::Closed:: *)
(*$GitHubConfig*)


If[Not@ValueQ@$GitHubConfig,
	$GitHubConfig:=
		Replace[
			Do[
				With[{f=packageFilePath["Private", d]},
					If[FileExistsQ@f,
						$GitHubConfig=
							Replace[Import@f,
								{ 
									o_?OptionQ:>Association@o,
									_-><||>
									}
								];
						Break[]
						]
					],
				{d,
					{
						"GitHubConfig.m",
						"GitHubConfig.wl"
						}
					}
				],
			Null-><||>
			]
	]


(* ::Subsubsection:: *)
(*$GitHubUserName*)


If[Not@ValueQ@$GitHubUserName,
	$GitHubUserName:=
		Replace[
			KeyChainGet[{"AccountData", "GitHub", "Username"}, True],
			_Missing:>
				$GitHubConfig["Username"]
			]
	];


(* ::Subsubsection::Closed:: *)
(*$GitHubPassword*)


$GitHubStorePassword:=
	Lookup[$GitHubConfig, "StorePassword", False]


GitHubPassword[s_String]:=
	With[
		{
			base=
				Replace[gitHubPasswordCache[s],
					Except[_String]:>KeyChainGet[{"github.com",	s}, False]
					]
			},
		If[StringQ@base,
			base,
			If[$GitHubStorePassword,
				KeyChainGet[{"github.com",	s}, True],
				AuthenticationDialog[
					Dynamic@$ghauth,
					"",
					None,
					{{"github.com",Automatic},s}
					];
				If[AssociationQ[$ghauth]&&StringQ@$ghauth["github.com"][[2]],
					gitHubPasswordCache[s]=
						$ghauth["github.com"][[2]],
					gitHubPasswordCache[s]=None
					];
				Clear@$ghauth;
				gitHubPasswordCache[s]
				]
			]
		];
GitHubPassword[Optional[Automatic,Automatic]]:=
	GitHubPassword[$GitHubUserName];
Clear@$GitHubPassword;
$GitHubPassword:=
	GitHubPassword[Automatic];


(*If[ValueQ@$GitHubUserName&&!KeyMemberQ[$gitHubPassCache,$GitHubUserName],
	$gitHubPassCache[$GitHubUserName]:=
		Do[
			With[{f=
				FileNameJoin@{
					$PackageDirectory,
					"Private",
					d}
				},
				If[FileExistsQ@f,
					Replace[Import@f,
						s_String:>
							($gitHubPassCache[$GitHubUserName]=s);
						];
					Return[True]
					]
				],
			{d,
				{
					"GitHubPassword.m",
					"GitHubPassword.wl"
					}
				}
			]
	];*)


(* ::Subsubsection::Closed:: *)
(*$GitHubSSHConnected*)


$GitHubSSHConnected:=
	($GitHubSSHConnected=
		Quiet[processRunDupe[{"ssh","-T","git@github.com"}];
			Length@$MessageList===0
			]
		);


(* ::Subsubsection::Closed:: *)
(*GitHubPath*)


$GitHubEncodePassword:=
	TrueQ@$GitHubConfig["EncodePassword"];


ClearAll[GitHubPath, FormatGitHubPath]


Options[GitHubPath]={
	"Username"->Automatic,
	"Password"->None,
	"Branch"->"master",
	"Tree"->"tree"
	};
Options[FormatGitHubPath]=
	Options[GitHubPath];
FormatGitHubPath[path___String,ops:OptionsPattern[]]:=
	URLBuild@<|
		"Scheme"->
			"https",
		"Domain"->
			"github.com",
		If[$GitHubEncodePassword||
			MatchQ[OptionValue@"Password",_String|Automatic],
			"Username"->
				Replace[OptionValue["Username"],
					{
						Automatic:>
							Replace[OptionValue@"Password",
								Automatic|_String:>$GitHubUserName
								],
						Except[_String]->None
						}
					],
			Nothing
			],
		If[$GitHubEncodePassword||
			MatchQ[OptionValue@"Password",_String|Automatic],
			"Password"->
				Replace[
					Replace[OptionValue["Username"],{
						Automatic:>$GitHubUserName,
						Except[_String]->None
						}],
					s_String:>
						Replace[OptionValue["Password"],
							Automatic:>GitHubPassword[s]
							]
					],
			Nothing
			],
		"Path"->
			{
				Replace[OptionValue@"Username",
					Automatic:>$GitHubUserName
					],
				If[Length@{path}>1,
					Sequence@@Flatten@
						Insert[{path}, 
							{OptionValue["Tree"], OptionValue["Branch"]}, 
							2
							],
					Sequence@@{path}
					]
				}
		|>;
GitHubPath[repo_String, 
	t:"tree"|"raw"|"trunk", 
	branch_String, 
	p___String, 
	ops:OptionsPattern[]
	]:=
	GitHubPath[repo,
		If[t==="trunk", branch, Sequence@@{}],
		p, 
		"Branch"->If[t==="trunk", Nothing, branch],
		"Tree"->t,
		ops
		];
GitHubPath[path___String,ops:OptionsPattern[]]/;(TrueQ@$GitHubPathFormat):=
	FormatGitHubPath[path,ops];
GitHubPath[
	s_String?(
		(
			URLParse[#, "Scheme"]===None&&
				URLParse[#, "Domain"]===None&&
				Length@URLParse[#, "Path"]>1
			)||
		URLParse[#, "Scheme"]==="github"&
		),
	o:OptionsPattern[]
	]:=
	GitHubPathParse[
		If[URLParse[s, "Scheme"]===None&&URLParse[s, "Domain"]===None,
			"github:"<>s,
			s
			],
		o
		];
GitHubPath[URL[s_String], ops:OptionsPattern[]]:=GitHubPath[s, ops];
GitHubPath[GitHubPath[p___String, o___?OptionQ], op:OptionsPattern[]]:=
	GitHubPath[p, Sequence@@DeleteDuplicatesBy[Flatten@{op, o}, First]]


GitHubPath/:
	Normal[GitHubPath[repos___,ops___?OptionQ]]:=
		{
			FirstCase[{ops},
				("Username"->u_):>u,
				$GitHubUserName
				],
			repos
			};
GitHubPath/:
	URL[GitHubPath[path___String,ops:OptionsPattern[]]]:=
		FormatGitHubPath[path,ops]


Format[g:GitHubPath[path___String,ops:OptionsPattern[]]]:=
	RawBoxes@
		BoxForm`ArrangeSummaryBox[
			"GitHubPath",
			g,
			None,
			{
				BoxForm`MakeSummaryItem[{"Path: ", URLBuild[{path}]}, StandardForm],
				BoxForm`MakeSummaryItem[
					{"URL: ", 
						Hyperlink[FormatGitHubPath@@g]
						}, StandardForm]
				},
			Map[
				BoxForm`MakeSummaryItem[
					{
						Row@{#[[1]], ": "}, #[[2]]
						},
					StandardForm
					]&,
				Flatten[Normal/@{ops}]
				],
			StandardForm
			]


(* ::Subsubsection::Closed:: *)
(*GitHubPathQ*)


GitHubPathQ[path:_String|_URL]:=
	With[{p=URLParse[path]},
		(
			(MatchQ[p["Scheme"],"http"|"https"|None]&&p["Domain"]==="github.com")||
			p["Scheme"]==="github"&&p["Domain"]===None
			)
			&&
		Length@p["Path"]>0
		];
GitHubPathQ[_GitHubPath]:=
	True;


(* ::Subsubsection::Closed:: *)
(*GitHubPathParse*)


Options[GitHubPathParse]=
	Options[GitHubPath];
GitHubPathParse[path:_String|_URL, o:OptionsPattern[]]:=
	If[GitHubPathQ[path],
		Replace[
			DeleteCases[""]@
				URLParse[path,"Path"],
			{
				{user_,parts___}:>
					GitHubPath[parts,"Username"->user, o]
				}
			],
		$Failed
		];


(* ::Subsubsection::Closed:: *)
(*GitHubRepoParse*)


GitHubRepoParse[path:_String|_URL]:=
	If[GitHubPathQ[path],
		Replace[
			DeleteCases[""]@
				URLParse[path,"Path"],{
			{"repos",user_,parts__}|
			{user_,parts__,"releases"|"deployments"}|
			{user_,parts__,"releases"|"deployments","tag",___}:>
				GitHubPath[parts,"Username"->user]
			}],
		$Failed
		];


(* ::Subsubsection::Closed:: *)
(*GitHubRepoQ*)


iGitHubRepoQ[path:_String|_URL]:=
	GitHubPathQ[path]&&
	With[{p=URLParse[path]},
		!MatchQ[p["Path"],
			{"repos",__}|
			{__,"releases"|"deployments"}|
			{__,"releases"|"deployments","tag",___}
			]
		];
GitHubRepoQ[p:GitHubPath[___String,___?OptionQ]]:=
	iGitHubRepoQ[URL@p]
GitHubRepoQ[path:_String|_URL]:=
	GitHubRepoQ[GitHubPath@path];
GitHubRepoQ[_]:=False


(* ::Subsubsection::Closed:: *)
(*GitHubReleaseQ*)


GitHubReleaseQ[GitHubPath[p__String,___?OptionQ]]:=
	MatchQ[{p},
		{__,"releases"}|
		{__,"releases","tag",_}
		];
GitHubReleaseQ[path:_String|_URL]:=
	If[GitHubPathQ@path,
		Replace[GitHubPathParse[path],{
			g_GitHubPath:>
				GitHubReleaseQ@g,
			_->False
			}],
		False
		];


(* ::Subsubsection::Closed:: *)
(*GitHubQuery*)


GitHubQuery[
	path:_?(MatchQ[Flatten@{#},{___String}]&):{},
	query:(_String->_)|{(_String->_)...}:{},
	headers:_Association:<||>]:=
	HTTPRequest[
		URLBuild@<|
			"Scheme"->"https",
			"Domain"->"api.github.com",
			"Path"->Flatten@{path},
			"Query"->{query}
			|>,
		headers
		];


(* ::Subsubsection::Closed:: *)
(*Auth*)


(*GitHubAuth[
	user:_String|Automatic:Automatic,
	scopes:_String|{__String}:{"public_repo"}]:=
	GitHubQuery[
		Replace[user,Automatic:>$GitHubUserName],
		<|
			"Headers"\[Rule]{
				"Authorization"\[Rule]"token OAUTH-TOKEN",
				"
				}
			|>
		];*)


(* ::Subsubsection::Closed:: *)
(*AuthHeader*)


GitHubAuthHeader[
	user:_String|Automatic:Automatic,
	password:_String|Automatic:Automatic
	]:=
	StringJoin@{
		"Basic ",
		Developer`EncodeBase64@
			StringJoin@{
				Replace[user,
					Automatic:>
						$GitHubUserName
					],
				":",
				Replace[password,
					Automatic:>
						GitHubPassword[user]
					]
				}
		};


(* ::Subsubsection::Closed:: *)
(*UserAPI*)


GitHubUserAPI[
	type:"users"|"org":"users",
	user:_String|Automatic:Automatic,
	path:{___String}|_String:{},
	query:(_String->_)|{(_String->_)...}:{},
	headers:_Association:<||>
	]:=
	GitHubQuery[{
		type,
		Replace[user,Automatic:>$GitHubUserName],
		Flatten@path
		},
		query,
		headers
		];


(* ::Subsubsection::Closed:: *)
(*ReposAPI*)


GitHubReposAPI[
	repo_GitHubPath?GitHubRepoQ,
	path:{___String}|_String:{},
	query:(_String->_)|{(_String->_)...}:{},
	headers:_Association:<||>
	]:=
	GitHubQuery[
		Flatten@{
			"repos",
			Sequence@@Normal@repo,
			path
			},
		query,
		headers
		];
GitHubReposAPI[
	s_String?GitHubRepoQ,
	path:{___String}|_String:{},
	query:(_String->_)|{(_String->_)...}:{},
	headers:_Association:<||>
	]:=
	GitHubReposAPI[
		GitHubPathParse[s],
		path,
		query,
		headers
		];


(* ::Subsubsection::Closed:: *)
(*Repositories*)


GitHubRepositories[
	type:"user"|"org":"user",
	user:_String|Automatic:Automatic,
	query:(_String->_)|{(_String->_)...}:{},
	headers:_Association:<||>
	]:=
	GitHubUserAPI[type,user,"repos",query,headers];


(* ::Subsubsection::Closed:: *)
(*Create*)


$gitHubCreateParamMap=
	{
		"AutoInit"->"auto_init",
		"HasWiki"->"has_issues",
		"HasProjects"->"has_projects",
		"HasIssues"->"has_wiki",
		"Private"->"team_id",
		"GitIgnore"->"gitignore_template",
		"LicenseTemplate"->"license_template",
		"AllowSquashMerge"->"allow_squash_merge",
		"AllowMergeCommit"->"allow_merge_commit",
		"AllowRebaseMerge"->"allow_rebase_merge",
		e_:>ToLowerCase[e]
		};


Options[GitHubCreate]=
	{
		"Username"->Automatic,
		"Password"->Automatic,
		"Description"->Automatic,
		"HomePage"->Automatic,
		"AutoInit"->True,
		"HasWiki"->Automatic,
		"HasProjects"->Automatic,
		"HasIssues"->Automatic,
		"Private"->Automatic,
		"GitIgnore"->Automatic,
		"LicenseTemplate"->Automatic,
		"AllowSquashMerge"->Automatic,
		"AllowMergeCommit"->Automatic,
		"AllowRebaseMerge"->Automatic
		};
GitHubCreate[
	repo_String,
	ops:OptionsPattern[]
	]:=
	With[{u=Replace[OptionValue["Username"],Automatic:>$GitHubUserName]},
		GitHubQuery[
			{
				"user",
				"repos"
				},
			<|
				"Method"->"POST",
				"Body"->
					ExportString[
						Map[(First@#/.$gitHubCreateParamMap)->Last@#&,
							DeleteCases[_->Except[_String|True|False]]@
								FilterRules[
									Flatten@
										{
											"Name"->repo,
											ops,
											Options[GitHubCreate]
											},
									Except["Username"|"Password"]
									]
							],
						"JSON"
						],
				"Headers"->{
					"Authorization"->
						GitHubAuthHeader[
							u,
							OptionValue["Password"]
							]
						}
				|>
			]
		];


(* ::Subsubsection::Closed:: *)
(*Delete*)


GitHubDelete//ClearAll


Options[GitHubDelete]=
	{
		"Username"->Automatic,
		"Password"->Automatic
		};
GitHubDelete[
	repo_GitHubPath?GitHubRepoQ,
	ops:OptionsPattern[]
	]:=
	With[{uu=URLParse[URL@repo]},
		GitHubReposAPI[
			repo,
			<|
				"Method"->"DELETE",
				"Headers"->
					{
						"Authorization"->
							GitHubAuthHeader[
								DeleteCases[uu["Path"], ""][[1]],
								OptionValue["Password"]
								]
							}
				|>
			]
		];
GitHubDelete[
	s_String?GitHubRepoQ,
	ops:OptionsPattern[]
	]:=
	Block[{$GitHubPathFormat=False},
		GitHubDelete[
			GitHubPath[s, FilterRules[{ops}, Options@GitHubPath]],
			ops
			]
		];
(*GitHubDelete[
	s_String?(
		URLParse[#,"Scheme"]===None&&
		Length@URLParse[#,"Path"]===1&
		),
	ops:OptionsPattern[]
	]:=
	GitHubDelete[
		GitHubPath[s,
			FilterRules[{ops},Options@GitHubPath]
			],
		ops]*)


(* ::Subsubsection::Closed:: *)
(*CreateReadme*)


GitHubCreateReadme[repo_?GitRepoQ,readmeText:_String:""]:=
	With[{o=
		OpenWrite@
			FileNameJoin@{
				repo,
				"README.md"
				}
		},
		WriteString[o,readmeText];
		Close@o
		];


(* ::Subsubsection::Closed:: *)
(*Releases*)


GitHubReleases[
	repo:(_GitHubPath|_String)?GitHubRepoQ,
	identifier:_String|_Integer|None:None]:=
	GitHubReposAPI[
		repo,
		Switch[identifier,
			None,
				"releases",
			_Integer|_?(StringMatchQ[ToLowerCase@#,"latest"]&),
				{"releases",ToLowerCase@ToString@identifier},
			_,
				{"releases","tags",ToLowerCase@ToString@identifier}
			]
		];
GitHubReleases[
	repo:(_GitHubPath|_String)?GitHubReleaseQ,
	identifier:_String|_Integer|None:None
	]:=
	Replace[Replace[repo,s_String:>GitHubPathParse[s]],{
		GitHubPath[s__,"releases","tag",tag_String,o__?OptionQ]:>
			GitHubReleases[GitHubPath[s,o],tag],
		GitHubPath[s__,"releases",o__?OptionQ]:>
			GitHubReleases[GitHubPath[s,o],identifier]
		}]


(* ::Subsubsection::Closed:: *)
(*Deployments*)


GitHubDeployments[repo:(_GitHubRepo|_String)?GitHubRepoQ,
	identifier:_String|_Integer|None
	]:=
	GitHubReposAPI[repo,
		If[identifier===None,
			"deployments",
			{"deployments",ToLowerCase@ToString@identifier}
			]
		];


(* ::Subsubsection::Closed:: *)
(*GitHubClone *)


GitHubClone//Clear


Options[GitHubClone]=
	Join[
		Options[GitHubPath],
		{
			OverwriteTarget->False
			}
		];
GitHubClone[
	repo:(_String|_GitHubPath)?(Not@*GitHubReleaseQ),
	dir:(_String?(DirectoryQ@*DirectoryName))|Automatic:Automatic,
	ops:OptionsPattern[]
	]:=
	Module[
		{
			path=
				If[MatchQ[repo, _GitHubPath],
					repo,
					GitHubPath[repo, FilterRules[{ops}, Options@GitHubPath]]
					]
			},
		With[
			{
				o=Options[path],
				n=Normal[path]
				},
			Switch[Lookup[o, "Tree", "tree"],
				"tree",
					If[Length@n<3,
						GitClone[URL[path], dir,
							TrueQ@OptionValue[OverwriteTarget]
							],
						SVNExport[
							URL[GitHubPath[repo, "Tree"->"trunk", "Branch"->Nothing]],
							dir,
							"TrustServer"->True,
							OverwriteTarget->OptionValue[OverwriteTarget]
							]
						],
				"trunk",
					SVNExport[
						URL[path],
						dir,
						"TrustServer"->True,
						OverwriteTarget->OptionValue[OverwriteTarget]
						],
				"raw",
					Replace[
						URLDownload[
							URL[path],
							FileNameJoin@{
								If[StringQ@dir, 
									If[!DirectoryQ@dir, CreateDirectory[dir]];
									dir,
									$TemporaryDirectory
									], 
								Last@n
								}
							],
						File[f_]:>f
						],
				_,
					$Failed
				]
			]/;path=!=$Failed
		];
GitHubClone[
	repo:(_String|_GitHubPath)?GitHubReleaseQ,
	dir:(_String?(DirectoryQ@*DirectoryName))|Automatic:Automatic
	]:=
	With[{release=
		GitHubImport["Releases",
			repo,
			"latest"
			]["Content"]
		},
		If[AssociationQ@release,
			If[Length@release["Assets"]>0,
				With[{url=
					release[["Assets",-1,"BrowserDownloadURL"]]
					},
					URLDownload[
						url,
						FileNameJoin@{
							Replace[dir, 
								{
									Automatic:>$TemporaryDirectory,
									_:>(If[!DirectoryQ@dir, CreateDirectory[dir]];dir)
									}
								],
							URLParse[url,"Path"][[-1]]
							}
						]
					],
				ExtractArchive[
					URLDownload[
						release["ZipballURL"],
						FileNameJoin@{
							$TemporaryDirectory,
							URLParse[release["ZipballURL"],"Path"][[-1]]
							}
						],
					Replace[dir, 
						{
							Automatic:>$TemporaryDirectory,
							_:>(If[!DirectoryQ@dir, CreateDirectory[dir]];dir)
							}
						]
					]
				],
			$Failed
			]
		]


(* ::Subsubsection::Closed:: *)
(*GitHubConfigure*)


GitHubConfigure[
	dirBase:_String?DirectoryQ|Automatic,
	repo:_String?GitHubRepoQ|_GitHubPath,
	ignorePats:{___String}|None:None,
	excludePats:{___String}|None:None
	]:=
	Module[{repoExistsQ, dir=Replace[dirBase, Automatic:>Directory[]]},
		If[!GitRepoQ@dir,
			GitInit[dir, ignorePats, excludePats];
			GitSetRemote[dir, repo];
			repoExistsQ=Between[URLRead[repo,"StatusCode"],{200,299}];
			If[repoExistsQ, GitRealignRemotes[dir]]
			];
		If[GitRepoQ@dir,
			If[!ValueQ[repoExistsQ],
				repoExistsQ=Between[URLRead[repo,"StatusCode"],{200,299}]
				];
			If[!repoExistsQ,
				GitHubImport["Create",
					URLParse[repo, "Path"][[-1]]
					];
				GitSetRemote[dir, repo]
				]
			]
		];


(* ::Subsubsection::Closed:: *)
(*GitHubPush*)


GitHubPush[
	dir:_String?GitRepoQ,
	repo:_String|_GitHubPath|Automatic:Automatic
	]:=
	Replace[repo,{
		Automatic:>
			Replace[GitGetPushURL[dir],
				{
					s_String:>
						Git["Push", dir, s],
					(r_->s_):>
						Quiet@
							Check[
								Git["Push", dir, r, "master"],
								Git["Push", s]
								]
					}
				],
		s_String?(URLParse[#, "Scheme"]===None&):>
			Quiet@
				Check[
					Git["Push", dir, s, "master"],
					Git["Push", dir, URL@GitHubPath[s]]
					],
		s_String:>
			Git["Push", dir, s]
		}]


(* ::Subsubsection::Closed:: *)
(*GitHub*)


$GitHubActions=
	<|
		"Push"->
			GitHubPush,
		"Configure"->
			GitHubConfigure,
		"Repositories"->
			GitHubRepositories,
		"Clone"->
			GitHubClone,
		"Create"->
			GitHubCreate,
		"Delete"->
			GitHubDelete,
		"CreateReadme"->
			GitHubCreateReadme,
		"Releases"->
			GitHubReleases,
		"Deployments"->
			GitHubDownloads,
		"Path"->
			Function[GitHubPath[##]],
		"URL"->
			Function[
				Replace[GitHubPath[##],
					g_GitHubPath:>URL[g]
					]
				],
		"RawPath"->
			Function[GitHubPath[##, "Tree"->"raw"]],
		"RawURL"->
			Function[
				Replace[GitHubPath[##, "Tree"->"raw"],
					g_GitHubPath:>URL[g]
					]
				],
		"SVNPath"->
			Function[GitHubPath[##, "Tree"->"trunk", "Branch"->Nothing]],
		"SVNURL"->
			Function[
				Replace[GitHubPath[##, "Tree"->"trunk", "Branch"->Nothing],
					g_GitHubPath:>URL[g]
					]
				],
		"PathQ"->
			GitHubPathQ,
		"RepoQ"->
			GitHubRepoQ
		|>;


$githubactions:=
	KeyMap[ToLowerCase]@$GitHubActions


packageAddAutocompletions[
	"GitHub",
	{
		Keys[$GitHubActions]
		}
	]


GitHub//Clear


GitHub[
	command_?(KeyMemberQ[$githubactions,ToLowerCase@#]&),
	args:Except[_?OptionQ]...,
	opp___?OptionQ
	]:=
	Block[{$GitHubRepoFormat=True},
		With[
			{
				cmd=$githubactions[ToLowerCase@command],
				ropp=Sequence@@FilterRules[{opp}, Except["GitHubImport"]]
				},
			With[
				{
					r=
						If[Options[cmd]=!={},
							cmd[args, Sequence@@FilterRules[{opp}, Options@cmd]],
							With[{c=cmd[args, ropp]},
								If[Head@c===cmd,
									cmd[args],
									c
									]
								]
							]
					},
				Replace[r,
					h_HTTPRequest:>
						If[Lookup[{opp}, "GitHubImport", $GitHubImport]=!=False, 
							GitHubImport, 
							Identity
							]@
							URLRead[h]
					]/;Head[r]=!=cmd
				]
			]
		];


GitHub[
	path:{___String}|_String:{},
	query:(_String->_)|{(_String->_)...}:{},
	headers:_Association:<||>,
	opp___?OptionQ
	]:=
	Block[{$GitHubRepoFormat=True},
		If[
			Lookup[{opp}, "GitHubImport", $GitHubImport]=!=False, 
			GitHubImport, 
			Identity
			]@
			URLRead[
				GitHubQuery[
					path,
					query,
					headers
					]
				]
		];


(* ::Subsubsection::Closed:: *)
(*GitHubImport*)


GitHubImport[a_Association]:=
	Association@
		KeyValueMap[
			StringReplace[
				StringJoin[
					Replace[
						HoldPattern[Capitalize[s_String]]:>
							(ToUpperCase@StringTake[s,1]<>StringDrop[s,1])
						]@*Capitalize/@StringSplit[#,"_"]
					],{
				"Id"~~EndOfString->"ID",
				"Url"->"URL",
				"Html"->"HTML"
				}]->
				Which[
					StringEndsQ[#,"_at"],
						DateObject@#2,
					StringEndsQ[#,"url"],
						URL[#2],
					True,
						GitHubImport@#2
					]&,
		a
		];
GitHubImport[h_HTTPResponse]:=
	<|
		"StatusCode"->
			h["StatusCode"],
		"Content"->
			If[MatchQ[h["StatusCode"],0|(_?(Between@{200,299}))],
				Quiet[
					Check[
						GitHubImport@Import[h, "RawJSON"],
						Null,
						Import::jsonnullinput
						],
					Import::jsonnullinput
					],
				$Failed
				]
		|>;
GitHubImport[s_String]:=
	s;
GitHubImport[l_List]:=
	GitHubImport/@l;
GitHubImport[e_]:=
	e


GitHubImport[
	command_?(KeyMemberQ[$githubactions,ToLowerCase@#]&),
	args__
	]:=
	With[{gh=GitHub[command,args]},
		GitHubImport[gh]/;Head[gh]=!=GitHub
		]
GitHubImport[
	path:{___String}|_String:{},
	query:(_String->_)|{(_String->_)...}:{},
	headers:_Association:<||>
	]:=
	With[{gh=GitHub[path,query,headers]},
		GitHubImport[gh]/;Head[gh]=!=GitHub
		]


(* ::Subsection:: *)
(*End*)


End[];
EndPackage[];
