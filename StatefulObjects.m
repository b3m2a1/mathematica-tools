(* ::Package:: *)



(* ::Chapter:: *)
(*StatefulObject*)



BeginPackage["StatefulObjects`"];


StatefulObject::usage="ExpressionStore-backed object";
StatefulObjects::usage="Lists the objects in the ExpressionStore";
StatefulObjectNew::usage="Makes a new StatefulObject";


BeginPackage["`Package`"];


$Objects::usage="";


createObject::usage="";
updateObjectField::usage="";
updateObjectFieldKey::usage="";
updateObjectFieldPart::usage="";
removeObjectField::usage="";
removeObject::usage="";


bind::usage="";


clearObjects::usage="";
addObject::usage="";
dropObject::usage="";


objectExists::usage="";
objectSymbol::usage="";
fromName::usage="";
objectData::usage="";


prop::usage="";
props::usage="";


EndPackage[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Object cache*)



(* ::Text:: *)
(*
	Using a weak hash map so that we can store data but still have it nicely cleaned up
*)



If[Head@$Objects=!=Language`ExpressionStore,
  $Objects=Language`NewExpressionStore["Objects"];
  clearObjects[];
  ]


(* ::Subsection:: *)
(*OOP*)



objPat = _Symbol?objectSymbol|_StatefulObject;


(* ::Subsubsection::Closed:: *)
(*MutationHandler*)



objectMutationHandler~SetAttributes~HoldAllComplete


bindMutation/:(bindMutation[pat_]:=op_):=
  With[
    {
      p=
        HoldPattern[objectMutationHandler[pat]]/.
          obj->obj:objPat
      },
    p:=Quiet@Check[op, Language`MutationFallthrough]
    ];


(* ::Subsubsubsection::Closed:: *)
(*
l@field= ...
l@field[...]= ...
l@field[[...]]= ...
*)



Do[
  With[{fn=fn},
    bindMutation[fn[obj[field_String], val_]]:=
      updateObjectField[obj, field, val, fn];
    bindMutation[fn[obj[field_String][key__], val_]]:=
      updateObjectFieldKey[obj, field, key, val, fn];
    bindMutation[fn[obj[field_String, key__], val_]]:=
      updateObjectFieldKey[obj, field, key, val, fn];
    bindMutation[fn[obj[[field_String, key__]], val_]]:=
      updateObjectFieldPart[obj, field, key, val, fn];
    bindMutation[fn[obj[field_String][[key__]], val_]]:=
      updateObjectFieldPart[obj, field, key, val, fn]
    ],
  {
    fn,
      {
        Set, SetDelayed,
        AddTo, SubtractFrom, TimesBy, DivideBy,
        AppendTo, PrependTo,
        AssociateTo, KeyDropFrom
        }
    }
  ]


Do[
  With[{fn=fn},
    bindMutation[fn[obj[field_String]]]:=
      updateObjectField[obj, field, $$noval, fn];
    bindMutation[fn[obj[field_String][key__]]]:=
      updateObjectFieldKey[obj, field, key, $$noval, fn];
    bindMutation[fn[obj[field_String][[key__]]]]:=
      updateObjectFieldPart[obj, field, key, $$noval, fn]
    ],
  {fn, {Increment, Decrement}}
  ]


(* ::Subsubsubsection::Closed:: *)
(*l =.*)



bindMutation[obj[field_String]=.]:=
  removeStatefulObjectField[obj];


(* ::Subsubsubsection::Closed:: *)
(*Fallback*)



objectMutationHandler[e___]:=
  Language`MutationFallthrough;


Language`SetMutationHandler[StatefulObject, objectMutationHandler];


(* ::Subsubsection::Closed:: *)
(*Accessors*)



(* ::Subsubsubsection::Closed:: *)
(*DeleteObject*)



StatefulObject/:DeleteObject[obj_StatefulObject]:=
  removeObject[obj]


(* ::Subsubsubsection::Closed:: *)
(*Keys*)



StatefulObject/:Keys[obj_StatefulObject]:=
  props[obj]


(* ::Subsubsubsection::Closed:: *)
(*
l@field
l[....]
l[[...]]
*)



obj_StatefulObject[field_String]:=
  getStatefulObjectField[obj, field];
obj_StatefulObject[field_, key__]:=
  getStatefulObjectFieldKey[obj, field, key];
StatefulObject/:obj_StatefulObject[[field_, key__]]:=
  getStatefulObjectFieldPart[obj, field, key]


(* ::Subsubsubsection::Closed:: *)
(*l@field[...]*)



obj_StatefulObject[field_String[arg___]]:=
  getStatefulObjectField[obj, field][arg];


(* ::Subsubsubsection::Closed:: *)
(*l@field[...]*)



HoldPattern[obj_StatefulObject[field_String[[arg___]]]]:=
  getStatefulObjectField[obj, field][[arg]];


(* ::Subsubsection::Closed:: *)
(*Constructor*)



StatefulObjectNew[name_String, props_Association]:=
  createObject[name, props];
StatefulObjectNew[name_String]:=
  createObject[name, <||>]
StatefulObjectNew[props_Association]:=
  createObject[CreateUUID[], props]
StatefulObjectNew[]:=
  createObject[CreateUUID[], <||>]


StatefulObject[id_Integer]?System`Private`HoldEntryQ:=
  fromID[id]


StatefulObject[name_String]?System`Private`HoldEntryQ:=
  fromName[name]


(* ::Subsection:: *)
(*API*)



(* ::Subsubsection::Closed:: *)
(*StatefulObjects*)



StatefulObjects[]:=
  ($Objects@"listTable"[])[[All, 1]]


(* ::Subsubsection::Closed:: *)
(*fromID*)



fromID[name_]:=
  SelectFirst[
    StatefulObjects[], 
    #["ObjectID"]==name&, 
    Missing["KeyAbsent", name]
    ]


(* ::Subsubsection::Closed:: *)
(*fromName*)



fromName[name_]:=
  SelectFirst[
    StatefulObjects[], 
    #["ObjectName"]==name&, 
    Missing["KeyAbsent", name]
    ]


(* ::Subsubsection::Closed:: *)
(*objectExists*)



objectSymbol[s_Symbol]:=
  MatchQ[OwnValues[s], {_:>_StatefulObject}];
objectSymbol~SetAttributes~HoldFirst;


objectExists[name_String]:=
  !MissingQ@fromID[name];
objectExists[eo_StatefulObject]:=
  System`Private`HoldNoEntryQ@eo;
objectExists[s_Symbol]:=
  MatchQ[OwnValues[s], {_:>_StatefulObject?objectExists}];
objectExists[e_]:=
  With[{eval=Evaluate@e},
    MatchQ[eval, _StatefulObject|_String]&&
      objectExists@eval
    ];
objectExists~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*bindMethod*)



bindMethod[obj_, symbol_]:=
  Module[{boundmethod},
    SetAttributes[boundmethod, HoldAllComplete];
    boundmethod[args___]:=
      symbol[obj, args];
    Hold[boundmethod]
    ]


(* ::Subsubsection::Closed:: *)
(*prop*)



prop[val_]:=
  Module[{field},
    field=val;
    Hold[field]
    ];


(* ::Subsubsection::Closed:: *)
(*getProp*)



getProp[eo_, field_]:=
  Replace[$Objects@"get"[eo, field], 
    Null:>Missing["FieldAbsent", field]
    ]


(* ::Subsubsection::Closed:: *)
(*bind*)



bind[obj_, key_, attr_Symbol]:=
  If[System`Private`HasDownCodeQ[attr]||Length@DownValues[attr]>0,
    $Objects@"put"[obj, key, bindMethod[obj, attr]],
    $Objects@"put"[obj, key, prop[attr]]
    ];
bind[obj_, key_, attr_]:=
  $Objects@"put"[obj, key, prop[attr]]


(* ::Subsubsection::Closed:: *)
(*props*)



props[obj_]:=
  $Objects@"getKeys"[obj]


(* ::Subsubsection::Closed:: *)
(*addObject*)



addObject[obj_StatefulObject, data_]:=
  (
    KeyValueMap[bind[obj, ##]&, data];
    obj
    )


(* ::Subsubsection::Closed:: *)
(*dropObject*)



dropObject[obj_StatefulObject]:=
  $Objects@"remove"[obj];


(* ::Subsubsection::Closed:: *)
(*clearObjects*)



clearObjects[]:=
  Scan[$Objects@"remove", ($Objects@"listTable"[])[[All, 1]]]


(* ::Subsubsection::Closed:: *)
(*Create*)



createObject[name_, props_]:=
  Module[
    {
      id=$ModuleNumber
      },
    addObject[
      System`Private`HoldSetNoEntry@StatefulObject[name],
      Join[
        <|
          "ObjectName"->name,
          "ObjectID"->id
          |>,
        props
        ]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*Update*)



(* ::Subsubsubsection::Closed:: *)
(*Template*)



updateStatefulObjectTemplate[eo:objPat, field_, args___, fn_]:=
  With[
    {
      sym=getProp[eo, field]
      },
    Replace[sym,
      {
        Hold[var_]:>
          With[{v=fn[var, args]},
            v
            ],
        m_Missing:>
          If[Unevaluated[fn]===Set,
            bind[eo, field, args];
            args,
            bind[eo, field, Null];
            updateStatefulObjectTemplate[eo, field, args, fn]
            ]
        }
      ]
    ];
updateStatefulObjectTemplate~SetAttributes~HoldAllComplete


(* ::Subsubsubsection::Closed:: *)
(*Field*)



updateObjectField[eo:objPat, field_, $$noval, fn_]:=
  updateStatefulObjectTemplate[eo, field, fn];
updateObjectField[eo:objPat, field_, val_, fn_]:=
  updateStatefulObjectTemplate[eo, field, val, fn];
updateObjectField~SetAttributes~HoldAllComplete


(* ::Subsubsubsection::Closed:: *)
(*Key*)



updateKey[var_, key__, $$noval, fn_]:=
  fn[var[key]];
updateKey[var_, key__, val_, fn_]:=
  fn[var[key], val];
updateKey[fn_]:=
  Function[Null, updateKey[##, fn], HoldAllComplete];
updateKey~SetAttributes~HoldAllComplete


updateObjectFieldKey[eo:objPat, field_, key__, val_, fn_]:=
  updateStatefulObjectTemplate[eo, field, key, val, updateKey@fn];
updateObjectField~SetAttributes~HoldAllComplete


(* ::Subsubsubsection::Closed:: *)
(*Part*)



updatePart[var_, key__, $$noval, fn_]:=
  fn[var[[key]]];
updatePart[var_, key__, val_, fn_]:=
  fn[var[[key]], val];
updatePart[fn_]:=
  Function[Null, updatePart[##, fn], HoldAllComplete];
updatePart~SetAttributes~HoldAllComplete


updateObjectFieldPart[eo:objPat, field_, key__, val_, fn_]:=
  updateStatefulObjectTemplate[eo, field, key, val, updatePart@fn];
updateObjectFieldPart~SetAttributes~HoldAllComplete


(* ::Subsubsection::Closed:: *)
(*Remove*)



removeStatefulObject[eo_StatefulObject]:=
  dropObject@eo(*With[
		{
			data=objectData[eo]
			},
		If[data["Callback"][[1]]["Remove", eo["Name"]]=!=False,
			Internal`SetValueNoTrack@@Append[data["Variable"], data["ID"]];
			dropObject[eo];,
			callbackFailure["Remove", eo["Name"]]
			]
		]*)


(* ::Subsubsection::Closed:: *)
(*Get*)



getStatefulObjectField[eo_, field_]:=
  Replace[getProp[eo, field],
    Hold[s_]:>s
    ]


getStatefulObjectFieldKey[eo_, field_, key__]:=
  Replace[getProp[eo, field],
    Hold[s_]:>s[key]
    ]


getStatefulObjectFieldPart[eo_, field_, key__]:=
  Replace[getProp[eo, field],
    Hold[s_]:>s[[key]]
    ]


(* ::Subsection:: *)
(*Formatting*)



(* ::Subsubsection::Closed:: *)
(*Format*)



(* ::Subsubsubsection::Closed:: *)
(*icon*)



icon=Graphics[GraphicsComplex[{{0.5540586437689723, 211.22613338276963}, 
  {-0.11885204965098099, 69.48566880720689}, {1.895235455634566, 66.51109879296129}, 
  {36.46816458878453, 45.82443827920583}, {72.56953673589284, 25.254364656063316}, 
  {105., 8.}, {117.49570858673958, -0.06536816528434243}, {124.51492874992734, 
  0.878732187481832}, {153.42947348710427, 17.25603227115834}, {187.56811054955077, 
  37.74806448723797}, {223., 58.}, {237.49613893835684, 65.93798263270537}, 
  {239.44594135623103, 68.77386661723037}, {240.11885204965097, 210.51433119279312}, 
  {238.10817153091133, 213.48815870359985}, {145.5623427147421, 267.24177696470235}, 
  {124.51789730359812, 279.13257824151054}, {117.49193495504996, 280.08944271909996}, 
  {2.503861061643165, 214.0620173672946}, {43.560047301171394, 179.23757445736743}, 
  {16.346153846153875, 194.}, {15.567087214709938, 76.25017298082007}, 
  {70.4694381579433, 44.82787267542078}, {74.5, 42.999999999999986}, {111.75, 22.}, 
  {112.4336012394578, 139.7510221593381}, {75., 160.}, {79.94375549235953, 
  314.50317351584255}, {80.62371165264662, 308.6707476960657}, {82.51492874992734, 
  304.12126781251817}, {88.12126781251817, 298.5149287499273}, {93.49682648415748, 
  295.9437554923595}, {99.32925230393431, 296.6237116526466}, {103.8787321874818, 
  298.5149287499273}, {109.48507125007268, 304.1212678125181}, {111.37628834735341, 
  308.6707476960657}, {112.05624450764047, 314.50317351584255}, {109.48507125007266, 
  319.87873218748183}, {103.87873218748184, 325.4850712500727}, {99.32925230393414, 
  327.3762883473535}, {93.49682648415748, 328.0562445076405}, {88.12126781251814, 
  325.4850712500727}, {82.51492874992734, 319.87873218748183}, {62.564008107307274, 
  229.75523262164617}, {25.5, 209.}, {24.623711652646566, 207.32925230393425}, 
  {47.56953673589285, 194.25436465606333}, {84.47027694984585, 172.83017776811124}, 
  {89.64644660940671, 170.35355339059325}, {118.49657530216143, 153.9415793762163}, 
  {123.51554114468777, 154.87630837736708}, {214.5, 207.}, {215.39771586001623, 
  208.69697839236858}, {173., 233.}, {139.583974852831, 251.2773500981125}, 
  {121.50172712087756, 262.04152273992685}, {116.48268169651413, 261.13045451257136}, 
  {99.55278640450004, 250.77639320225003}, {90.44721359549996, 246.22360679775}, 
  {128.554058643769, 443.22613338276966}, {127.88114795034902, 301.4856688072069}, 
  {129.84188611699153, 298.52565835097477}, {159., 281.}, {201.43735713450124, 
  256.7576805065603}, {241.4698967117442, 233.82912846845667}, {245.49613893835684, 
  231.9379826327054}, {252.51338019910503, 232.885103658122}, {276.5712535371437, 
  247.74275212228625}, {298.44721359549993, 260.22360679775005}, {329.5603540167316, 
  276.76185842572966}, {366.1581138830084, 298.52565835097477}, {368.11885204965097, 
  301.4856688072069}, {367.44594135623106, 443.2261333827695}, {365.4961389383568, 
  446.06201736729463}, {301.64644660940684, 482.3535533905934}, {296.46989671174424, 
  484.82912846845664}, {252.51759026884469, 511.1314566517398}, {245.49193495504994, 
  512.0894427191}, {130.50386106164316, 446.0620173672945}, {221.5, 74.99999999999997}, 
  {223.5, 76.}, {223.56499957561044, 193.75347488813327}, {180.56247065725793, 
  168.7579915822958}, {149.44721359549996, 152.22360679774997}, {128.5, 140.}, 
  {128.4342507654553, 22.24784324219455}, {177., 408.}, {144.37499999999997, 426.}, 
  {143.56652973793376, 308.2492058023085}, {220.43778884201336, 264.7584613285443}, 
  {239.90000000000006, 254.}, {240.43353579571203, 371.750908222062}, {212., 388.}, 
  {231.94375549235951, 34.50317351584252}, {232.62371165264662, 28.67074769606567}, 
  {234.51492874992735, 24.12126781251818}, {240.12126781251814, 18.514928749927325}, 
  {245.49682648415748, 15.943755492359532}, {251.32925230393423, 16.623711652646534}, 
  {255.8787321874818, 18.51492874992734}, {261.4850712500727, 24.121267812518163}, 
  {264.0562445076405, 29.49682648415748}, {263.3762883473535, 35.32925230393416}, 
  {261.4850712500727, 39.87873218748182}, {255.8787321874818, 45.48507125007266}, 
  {250.50317351584252, 48.05624450764047}, {244.67074769606572, 47.37628834735343}, 
  {240.1212678125181, 45.48507125007268}, {234.51492874992732, 39.87873218748186}, 
  {179.43096710757885, 455.25351006328174}, {153.5, 441.}, {152.6095655952785, 
  439.3123475237772}, {197.42274445151546, 414.73300350430594}, {207.48311746980065, 
  407.87116867471985}, {240.47027694984587, 388.8301777681113}, {246.49902628924144, 
  385.9688108569224}, {251.51554114468777, 386.8763083773671}, {342.5, 439.}, 
  {343.41160639295765, 440.7161335220982}, {329.44329242308265, 447.76871699665236}, 
  {289., 472.}, {255.57600084799745, 490.26499947000156}, {249.50097371075856, 
  494.0311891430776}, {244.48507125007265, 493.12126781251817}, {211.44721359549987, 
  474.2236067977501}, {256.55405864376894, 211.22613338276958}, {255.881147950349, 
  69.48566880720689}, {257.84188611699165, 66.52565835097472}, {287., 49.}, 
  {329.4373571345012, 24.757680506560252}, {369.46989671174424, 1.8291284684566702}, 
  {373.4961389383568, -0.06201736729459836}, {380.51338019910503, 0.8851036581220255}, 
  {404.5712535371437, 15.742752122286229}, {426.4472135955, 28.223606797749916}, 
  {457.56035401673165, 44.7618584257296}, {494.15811388300847, 66.52565835097475}, 
  {496.11885204965097, 69.48566880720689}, {495.44594135623106, 211.22613338276958}, 
  {493.49613893835686, 214.0620173672944}, {429.6464466094066, 250.35355339059313}, 
  {424.46989671174424, 252.8291284684567}, {380.5175902688447, 279.1314566517398}, 
  {373.49193495504994, 280.08944271909996}, {258.50386106164314, 214.06201736729457}, 
  {349.5, 306.99999999999994}, {351.5, 308.00000000000006}, {351.5648100405573, 
  425.7538096281325}, {315.5652428534063, 405.7530461106083}, {287.8787321874825, 
  389.48507125007285}, {256.5, 372.}, {256.43425076545526, 254.24784324219453}, {305., 
  176.}, {272.37500000000006, 194.}, {271.56652973793376, 76.2492058023085}, 
  {348.4377888420134, 32.758461328544314}, {367.9, 22.}, {368.43353579571203, 
  139.75090822206198}, {340., 156.}, {307.43096710757885, 223.2535100632817}, {281.5, 
  209.}, {280.60956559527847, 207.31234752377722}, {325.4227444515155, 
  182.733003504306}, {335.4831174698007, 175.87116867471997}, {368.47027694984587, 
  156.83017776811124}, {374.49902628924144, 153.9688108569224}, {379.51554114468775, 
  154.8763083773671}, {470.5, 207.}, {471.41160639295765, 208.71613352209818}, 
  {457.4432924230827, 215.76871699665244}, {417., 240.}, {383.5760008479975, 
  258.2649994700016}, {377.50097371075856, 262.0311891430776}, {372.4850712500727, 
  261.12126781251817}, {339.4472135955, 242.22360679774997}, {384.87873218748183, 
  317.5149287499273}, {383.9360143311849, 309.495888933317}, {386.5149287499273, 
  304.12126781251817}, {392.1212678125181, 298.5149287499273}, {397.49682648415745, 
  295.9437554923595}, {403.3292523039342, 296.6237116526465}, {407.8787321874819, 
  298.5149287499273}, {413.4850712500727, 304.1212678125181}, {415.3762883473535, 
  308.67074769606586}, {416.0562445076405, 314.50317351584255}, {413.4850712500727, 
  319.878732187482}, {407.87873218748194, 325.4850712500727}, {402.504111066683, 
  328.06398566881506}, {394.4850712500727, 327.12126781251817}, {392., 325.5}, {386.5, 
  320.}, {477.5, 75.00000000000017}, {479.5, 76.}, {479.5648100405573, 
  193.75380962813253}, {443.5652428534063, 173.75304611060832}, {415.8787321874809, 
  157.48507125007245}, {384.5, 140.}, {384.43425076545526, 22.24784324219455}}, 
  {Hue[0.6, 0.3, 0.95], EdgeForm[Hue[0.6, 0.3, 0.75]], 
   Annotation[FilledCurve[{{Line[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
        17, 18, 19}]}, {Line[{20, 21, 22, 23, 24, 25, 26, 27}]}, 
      {Line[{44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59}]}, 
      {Line[{80, 81, 82, 83, 84, 85, 86}]}}], "Geometry"], 
   Annotation[FilledCurve[{{Line[{60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 
        73, 74, 75, 76, 77, 78, 79}]}, {Line[{87, 88, 89, 90, 91, 92, 93}]}, 
      {Line[{110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 
        125}]}, {Line[{146, 147, 148, 149, 150, 151, 152}]}}], "Geometry"], 
   Annotation[FilledCurve[{{Line[{126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 
        137, 138, 139, 140, 141, 142, 143, 144, 145}]}, 
      {Line[{153, 154, 155, 156, 157, 158, 159}]}, 
      {Line[{160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 
        175}]}, {Line[{192, 193, 194, 195, 196, 197, 198}]}}], "Geometry"], 
   Annotation[Polygon[{{28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43}, 
     {94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109}, {176, 
     177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191}}], 
    "Geometry"]}], ImageSize -> {28.9609375, Automatic}, Background->None]


(* ::Subsubsubsection::Closed:: *)
(*Format*)



MakeBoxes[
  eo_StatefulObject?objectExists,
  fmt:StandardForm
  ]:=
  BoxForm`ArrangeSummaryBox[
    StatefulObject,
    StatefulObject,
    icon,
    {
      BoxForm`MakeSummaryItem[{"Name: ", eo@"ObjectName"}, fmt]
      },
    {
      BoxForm`MakeSummaryItem[{"Fields: ", Keys@eo}, fmt]
      },
    fmt
    ]


End[];


EndPackage[];



