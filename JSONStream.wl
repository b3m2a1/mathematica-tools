(* ::Package:: *)

(* ::Section:: *)
(*JSONStream*)


BeginPackage["JSONStream`"];


(*Package Declarations*)
JSONStream::usage="JSONStream[strm]
Read[JSONStream[s], type]
Close[JSONStream[s]]";
JSONStreamRead::usage="JSONStreamRead[JSONStream[s], type]";
JSONStreamClose::usage="JSONStreamClose[JSONStream[s]]";


(* ::Subsubsection::Closed:: *)
(*Private Declarations*)


AppendTo[$ContextPath,$Context<>"Package`"];


Begin["`Package`"];


(*Package Declarations*)
jStreamReader::usage="jStreamReader[stream]";
$jStreamBracketTokenIndicators::usage="$jStreamBracketTokenIndicators
$jStreamBracketTokenIndicators";
$jStreamChunkTokenIndicators::usage="$jStreamChunkTokenIndicators";
$jStreamStringTokenIndicators::usage="$jStreamStringTokenIndicators";
$jStreamInStringTokenIndicators::usage="$jStreamInStringTokenIndicators
$jStreamInStringTokenIndicators";
$jStreamPossibleTokenIndicators::usage="$jStreamPossibleTokenIndicators";
$jStreamBracketMap::usage="$jStreamBracketMap";
jStreamReadToSep::usage="jStreamReadToSep[reader]";
jStreamHandleSepData::usage="jStreamHandleSepData[reader, {ret, sep, sp}]";
jStreamReadTokenSegments::usage="jStreamReadTokenSegments[reader]";
jStreamBuildToken::usage="jStreamBuildToken[segs]";
jStreamReadToken::usage="jStreamReadToken[reader]";


End[];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


jStreamReader[stream : _String | _File | _InputStream] :=
 Module[{
   jStreamReader,
   jStreamData
   },
  jStreamReader~ClearAttributes~Temporary;
  jStreamData~ClearAttributes~Temporary;
  jStreamData =
   <|
    "InString" -> False,
    "BlockDepth" -> <|Association -> 0, List -> 0, 
      CompoundExpression -> 0|>,
    "BlockStack" -> {},
    "Cache" -> Internal`Bag[],
    "CacheStack" -> {},
    "CachePush" ->
     Function[
      Internal`StuffBag[jStreamData["Cache"], #]
      ],
    "CacheRecurse" ->
     Function[
      jStreamData["CacheStack"] = {jStreamData["CacheStack"], 
        jStreamData["Cache"]};
      jStreamData["Cache"] = Internal`Bag[];
      ],
    "CacheReset" ->
     Function[
      With[{c = jStreamData["CacheStack"]},
       jStreamData["CacheStack"] = c[[1]];
       jStreamData["Cache"] = c[[2]];
       ]
      ],
    "CachePart" ->
     Function[
      Internal`BagPart[jStreamData["Cache"], #]
      ],
    "CacheClear" ->
     Function[
      jStreamData["Cache"] = Internal`Bag[];
      ],
    "CacheDump" ->
     Function[
      With[{data = Internal`BagPart[jStreamData["Cache"], All]},
       jStreamData["Cache"] = Internal`Bag[];
       data
       ]
      ],
    "Stream" ->
     Replace[stream, {
       _File | _String?FileExistsQ :> OpenRead[stream],
       _String :> StringToStream[stream]
       }]
    |>;
  jStreamReader["HeldSymbol"] = Hold[jStreamData];
  jStreamReader["Symbol"] := Unevaluated[jStreamData];
  jStreamReader["Data"] := jStreamData;
  jStreamReader["Next"] := jStreamReadToken[jStreamReader];
  jStreamReader[s__] := jStreamData[s];
  jStreamReader /: Set[jStreamReader[p___], v_] :=
   
   jStreamData[p] = v;
  jStreamReader /: ReadString[jStreamReader, a___] :=
   
   ReadString[jStreamData["Stream"], a];
  jStreamReader /: Read[jStreamReader, a___] :=
   
   Read[jStreamData["Stream"], a];
  jStreamReader /: ReadList[jStreamReader, a___] :=
   
   ReadList[jStreamData["Stream"], a];
  jStreamReader /: StreamPosition[jStreamReader] :=
   
   StreamPosition@jStreamData["Stream"];
  jStreamReader /: SetStreamPosition[jStreamReader, p_] :=
   
   SetStreamPosition[jStreamData["Stream"], p];
  jStreamReader /: Close[jStreamReader] :=
   (
    Close@jStreamData["Stream"];
    jStreamData // Remove
    );
  jStreamReader
  ]
$jStreamBracketTokenIndicators =
  {"{", "}", "[", "]", "(", ")"};
$jStreamChunkTokenIndicators =
  {",", ":"};
$jStreamStringTokenIndicators =
  {"\""};
$jStreamInStringTokenIndicators =
  {"\\\"", "\""};
$jStreamPossibleTokenIndicators =
  Alternatives @@ Join[
    $jStreamStringTokenIndicators,
    $jStreamBracketTokenIndicators,
    $jStreamChunkTokenIndicators
    ];
$jStreamInStringTokenIndicators =
  Alternatives @@ $jStreamInStringTokenIndicators;
$jStreamBracketTokenIndicators =
  Alternatives @@ $jStreamBracketTokenIndicators;
$jStreamBracketMap = <|
   "{" -> {Opening, Association},
   "}" -> {Closing, Association},
   "[" -> {Opening, List},
   "]" -> {Closing, List},
   "(" -> {Opening, CompoundExpression},
   ")" -> {Closing, CompoundExpression}
   |>;
jStreamReadToSep[reader_] :=
  Module[{
    sp = StreamPosition@reader,
    strm = reader["Stream"],
    ret,
    sep
    },
   ret =
    With[{test = Read[strm, Character]},
     If[test === EndOfFile, Throw[EndOfFile]];
     SetStreamPosition[strm, sp];
     If[reader["InString"],
      If[StringMatchQ[test, $jStreamInStringTokenIndicators] ||
      
          StringMatchQ[
         test <> "\"", $jStreamInStringTokenIndicators],
       "",
       ReadString[strm, $jStreamInStringTokenIndicators]
       ],
      If[StringMatchQ[test, $jStreamPossibleTokenIndicators],
       "",
       ReadString[strm, $jStreamPossibleTokenIndicators]
       ]
      ]
     ];
   sep =
    If[reader["InString"],
     With[{ec = Read[reader, Character]},
      If[ec === "\\", ec <> Read[reader, Character], ec]
      ],
     Read[reader, Character]
     ];
   If[StringMatchQ[sep, $jStreamBracketTokenIndicators] && 
     StringLength@StringTrim[ret] > 0,
    SetStreamPosition[reader, StreamPosition[reader] - 1];
    sep = ""
    ];
   {ret, sep, sp}
   ];
jStreamHandleSepData[reader_, {ret_, sep_, sp_}] :=
  
  With[{sepData = $jStreamBracketMap[sep]},
   Which[
    sep === "",
    {ret, sep, Value, Expression},
    sep === ":",
    {ret, sep, Key, None},
    sep === ",",
    {ret, sep, Value, Expression},
    sep === "\"" && reader["InString"],
    reader["InString"] = False;
    {ret, sep, None, String},
    sep === "\"",
    reader["InString"] = True;
    {ret, sep, None, Continue},
    sep === "\\\"",
    {ret, sep, None, Continue},
    sepData[[1]] === Closing,
    If[reader["BlockStack"][[-1]] == sepData[[2]],
     reader["BlockDepth", sepData[[2]]] =
      Max@{reader["BlockDepth", sepData[[2]]] - 1, 0};
     reader["BlockStack"] = Drop[reader["BlockStack"], -1]
     ];
    {ret, sep, sepData, Expression},
    sepData[[1]] === Opening,
    reader["BlockDepth", sepData[[2]]] =
     Max@{reader["BlockDepth", sepData[[2]]] - 1, 0};
    reader["BlockStack"] =
     Append[reader["BlockStack"], sepData[[2]]];
    {ret, sep, sepData, None},
    True,
    {ret, sep, sepData, Expression}
    ]
   ];
jStreamReadTokenSegments[reader_] :=
 Block[{segs},
  segs = {None, 
    jStreamHandleSepData[reader, jStreamReadToSep[reader]]};
  While[segs[[-1, -1]] === Continue,
   segs = {segs, 
     jStreamHandleSepData[reader, jStreamReadToSep[reader]]}
   ];
  Cases[segs, {_String, _String, __}, \[Infinity]]
  ]
jStreamBuildToken[segs_] :=
 {
  Switch[segs[[-1, -1]],
   None, Null,
   String,
   {
    ToExpression@StringJoin@segs[[All, ;; 2]],
    String
    },
   _,
   {
    Replace[StringTrim@StringJoin@segs[[-1, 1]], {
      "null" -> Null,
      e_ :> ToExpression@e
      }],
    segs[[-1, -1]]
    }
   ],
  Replace[segs[[-1, -2]], {
    {o_, "{}"} :> {o, Association},
    {o_, "[]"} :> {o, List},
    {o_, "()"} :> CompoundExpression
    }]
  }
jStreamReadToken[reader_] :=
  
  Catch@jStreamBuildToken@jStreamReadTokenSegments[reader];


(*Different types of read processors *)

jStreamRead[reader_, Key] :=
  Replace[jStreamReadToken[reader], {
    {Null, Key} :>
     Replace[reader["CacheDump"][][[-1, 1]],
      {a_, String | Expression} :> a
      ],
    EndOfFile :> EndOfFile,
    e_ :>
     (
      reader["CachePush"][e];
      jStreamRead[reader, Key]
      )
    }];
jStreamRead[reader_, Association] :=
  
  Replace[jStreamReadToken[reader], {
    {_, {Closing, Association}} :>
     
     Association@Cases[reader["CacheDump"][], _Rule],
    {Null, Key} :>
     (
      reader["CachePush"][
       reader["CachePart"][-1][[1, 1]] ->
        (
         reader["CacheRecurse"][];
         ((reader["CacheReset"][]; #) &@jStreamRead[reader, Value])
         )
       ];
      jStreamRead[reader, Association]
      ),
    EndOfFile :> EndOfFile,
    e_ :>
     (
      reader["CachePush"][e];
      jStreamRead[reader, Association]
      )
    }];
jStreamRead[reader_, List] :=
  Replace[jStreamReadToken[reader], {
    {_, {Closing, List}} :>
     reader["CacheDump"][],
    {{v_, _}, None | Value} :>
     (
      reader["CachePush"][v];
      jStreamRead[reader, CompoundExpression]
      ),
    {Null, {Opening, t_}} :>
     (
      reader["CachePush"][
       jStreamRead[reader, t]
       ];
      jStreamRead[reader, CompoundExpression]
      ),
    EndOfFile :> EndOfFile
    }];
jStreamRead[reader_, CompoundExpression] :=
  
  Replace[jStreamReadToken[reader], {
    {_, {Closing, CompoundExpression}} :>
     $wrapper @@ 
      reader["CacheDump"],
    {Null, {Opening, t_}} :> jStreamRead[reader, t],
    {{v_, _}, None | Value} :>
     (
      reader["CachePush"][v];
      jStreamRead[reader, CompoundExpression]
      ),
    {Null, {Opening, t_}} :>
     (
      reader["CachePush"][
       jStreamRead[reader, t]
       ];
      jStreamRead[reader, CompoundExpression]
      ),
    EndOfFile :> EndOfFile
    }];
jStreamRead[reader_, Value] :=
  Replace[jStreamReadToken[reader], {
    {{a_, _}, None | Value} :>
     a,
    {Null, {Opening, t_}} :> jStreamRead[reader, t],
    EndOfFile :> EndOfFile,
    e_ :>
     (
      reader["CacheAdd"][e];
      jStreamRead[reader, Value]
      )
    }];
jStreamRead[reader_, KeyValuePattern] :=
  
  jStreamRead[reader, Key] ->
   jStreamRead[reader, Value];
jStreamRead[reader_, Automatic] :=
  
  Replace[jStreamReadToken[reader], {
    {Null, {Opening, t_}} :> jStreamRead[reader, t],
    {{a_, _}, None | Value} :>
     a
    }];
jStreamRead[reader_, "Token"] :=
  jStreamReadToken[reader];


JSONStream[strm : _String | _File | _InputStream] :=
  
  JSONStream[jStreamReader[strm]];
JSONStreamRead[JSONStream[s_Symbol], type_: "Token"] :=
  
  Replace[jStreamRead[s, type],
   EndOfFile | (_ -> EndOfFile) :> JSONStreamClose[JSONStream[s]]
   ];
JSONStreamClose[JSONStream[s_Symbol]] :=
  (
   Close@s;
   Remove[s];
   EndOfFile
   );
JSONStream /: Read[JSONStream[s_Symbol], type___] :=
  
  JSONStreamRead[JSONStream[s], type];
JSONStream /: Close[JSONStream[s_Symbol]] :=
  
  JSONStreamClose[JSONStream[s]];
Format[JSONStream[
   s_Symbol?(MatchQ[#["Data"]["Stream"], _InputStream] &)]] :=
 
 With[{is = s["Data"]["Stream"]},
  RawBoxes@
   BoxForm`ArrangeSummaryBox[
    "JSONStream",
    JSONStream[s],
    BoxForm`GenericIcon[InputStream],
    {
     BoxForm`SummaryItem[{"Name: ", 
       Replace[is[[1]], f_String :> FileNameTake@f]}],
     BoxForm`SummaryItem[{"Unique ID: ", is[[2]]}]
     },
    {
     BoxForm`SummaryItem[{"Open: ",
        Dynamic[Options[is] =!= {},
        UpdateInterval -> 1
        ]
       }]
     },
    StandardForm
    ]
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
