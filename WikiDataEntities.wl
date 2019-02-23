(* ::Package:: *)

(* ::Section:: *)
(*WikiData Entities*)


(* ::Text:: *)
(*This is a package for hooking the Entity framework into WikiData data. The core ideas and code were written by Carl Lange. A small syntax convenience layer was then layered on top of this.*)


BeginPackage["WikiDataEntities`"]


WikiData::usage="A message head for playing with WikiData";
WikiDataDataset::usage="WikiDataDataset[class] returns the dataset for an entity class
WikiDataDataset[class, pred] returns the dataset for an entity class and predicate
";
WikiDataClasses::usage="WikiDataClasses[pred] returns the classes implementing pred";
WikiDataEntityStore::usage="";
WikiDataPredicates::usage=
  "WikiDataPredicates[name] returns the predicates matching name";


(* ::Subsubsection::Closed:: *)
(*Package Functions*)


BeginPackage["`Package`"]


$wikiDataBase::usage="";
wikidataQuery::usage="";
entityQuery::usage="";
propertyQuery::usage="";
entityClassQuery::usage="";
extractEntityIDs::usage="";
wikidataEntityClassDataset::usage="";
wikidataRelatedProps::usage="";
wikidataRelatedItems::usage="";


EndPackage[]


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(* $wikiDataBase*)


(* ::Text:: *)
(*Per *)
(*  https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#SPARQL_endpoint*)


(* ::Text:: *)
(*JSON will also be more compact than CSV speeding things up some, presumably*)


$wikiDataBase=
  <|
    "Scheme"-> "https",
    "Domain" -> "query.wikidata.org",
    "Path"-> "bigdata/namespace/wdq/sparql"
    |>


(* ::Subsubsection::Closed:: *)
(*executeRequest*)


executeRequest[req_]:=
  Module[{str, res, bytes},
    bytes=URLRead[req, "BodyBytes"];
    If[ListQ@bytes,
      str=FromCharacterCode@bytes;
      res=ImportString[str, "RawJSON"];
      If[!AssociationQ@res, str, res],
     bytes
     ]
   ]


(* ::Subsubsection::Closed:: *)
(*prepQuery*)


prepQuery[query_, limit_]:=
  StringJoin[
    StringTrim@$entityPrefixHead<>"\n"<>query,
    "
    LIMIT "<>ToString@limit
    ]


(* ::Subsubsection::Closed:: *)
(*wikidataQuery*)


wikidataQuery//Clear
wikidataQuery[query_, limit_, returnTiming_:False] :=
  If[returnTiming, AbsoluteTiming, Identity]@executeRequest@
    HTTPRequest[
      $wikiDataBase,
      <|
        "Method"->"POST",
        "Body"->
          URLQueryEncode[
            <|
              "query"-> prepQuery[query, limit]
              |>
            ],
        "Headers"->
          <|
            "Accept"->
              "application/sparql-results+json"
            |>,
        "ContentType"-> "application/x-www-form-urlencoded"
        |>
      ]


(* ::Subsubsection::Closed:: *)
(*wikidataQueryVerbose*)


wikidataQueryVerbose[query_, limit_]:=
  Module[
    {
      queryResults
      },
    queryResults=wikidataQuery[query, limit, True];
    Print@
      Internal`LoadingPanel@
        StringForm[
          "Query execution took `` seconds",
          queryResults[[1]]
          ];
     queryResults[[2]]
     ];


(* ::Subsubsection::Closed:: *)
(*$entityPrefixHead*)


$entityPrefixHead=
  "PREFIX wikibase: <http://wikiba.se/ontology#>
PREFIX wd: <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX entity: <http://www.wikidata.org/entity/>";


(* ::Subsubsection::Closed:: *)
(*$entityQueryTemplate*)


$entityQueryTemplate=
  StringTemplate[
    "SELECT DISTINCT ?item WHERE { ?item wdt:`pred` wd:`entityClass`. }"
    ];


(* ::Subsubsection::Closed:: *)
(*entityQuery*)


entityQuery[entityClass_, pred_:"P31"]:=
  $entityQueryTemplate[<|"entityClass"->entityClass, "pred"->pred|>]


(* ::Subsubsection::Closed:: *)
(*$entityClassQueryTemplate*)


$entityClassQueryTemplate=
  StringTemplate[
    "SELECT
  ?item ?value
WHERE 
{
  ?item wdt:`pred` ?value    
  SERVICE wikibase:label { bd:serviceParam wikibase:language \"[AUTO_LANGUAGE],en\". }
}"
]


(* ::Subsubsection::Closed:: *)
(*entityClassQuery*)


entityClassQuery[pred_]:=
  $entityClassQueryTemplate[<|"pred"->pred|>]


(* ::Subsubsection::Closed:: *)
(*$propertyQueryTemplate*)


$propertyQueryInner="{
    {
    BIND(entity:`entity` AS ?entity) .
  
    hint:Query hint:optimizer 'None' .
  {  BIND(?entity AS ?valUrl) .
    BIND(\"N/A\" AS ?propUrl ) .
    BIND(\"Label\"@`lang` AS ?propLabel ) .
       ?entity rdfs:label ?val .
      
        FILTER (LANG(?val) = \"`lang`\") 
  }
    UNION
    {   BIND(?entity AS ?valUrl) .
      
        BIND(\"AltLabel\"@`lang` AS ?propLabel ) .
        optional{?entity skos:altLabel ?val}.
        FILTER (LANG(?val) = \"`lang`\") 
    }
    UNION
    {   BIND(?entity AS ?valUrl) .
      
        BIND(\"Description\"@`lang` AS ?propLabel ) .
        optional{?entity schema:description ?val}.
        FILTER (LANG(?val) = \"`lang`\") 
    }
     UNION
  {  ?entity ?propUrl ?valUrl .
    ?property ?ref ?propUrl .
    ?property rdf:type wikibase:Property .
    ?property rdfs:label ?propLabel.
       FILTER (lang(?propLabel) = '`lang`' )
        filter  isliteral(?valUrl) 
        BIND(?valUrl AS ?val)
  }
  UNION
  {  ?entity ?propUrl ?valUrl .
    ?property ?ref ?propUrl .
    ?property rdf:type wikibase:Property .
    ?property rdfs:label ?propLabel.
       FILTER (lang(?propLabel) = '`lang`' ) 
        filter  isIRI(?valUrl) 
        ?valUrl rdfs:label ?valLabel 
    FILTER (LANG(?valLabel) = \"`lang`\") 
         BIND(CONCAT(?valLabel) AS ?val)
  }
        BIND( SUBSTR(str(?propUrl),38, 250) AS ?propNumber)
    }
   }";


$propQueryEnding="
} ORDER BY xsd:integer(?propNumber)";


$propQueryBeginning="SELECT ?entity ?propLabel ?val
WHERE
{";


$propertyQueryTemplate=
  StringTemplate[
    $propQueryBeginning<>$propertyQueryInner<>$propQueryEnding
    ]


(* ::Subsubsection::Closed:: *)
(*propertyQuery*)


(* ::Text:: *)
(*Is this superior to: https://www.wikidata.org/w/api.php?action=wbgetentities  ?*)


propertyQuery[entity_String, lang_:"en"]:=
  $propertyQueryTemplate@<|"entity"->entity, "lang"->lang|>
propertyQuery[entity:{__String}, lang_:"en"]:=
  $propQueryBeginning<>
    With[{temp=StringTemplate[$propertyQueryInner]},
      StringRiffle[
        temp@<|"entity"->#, "lang"->lang|>&/@entity,
        "\n    UNION\n"
        ]
      ]<>$propQueryEnding


(* ::Subsubsection::Closed:: *)
(*getEnts*)


$entBase=
  <|
    "Scheme"->"https",
    "Domain"->"www.wikidata.org",
    "Path"->{"","w","api.php"}
    |>


getEnts[ids_]:=
  executeRequest@
    Append[$entBase, 
      "Query"->{"action"->"wbgetentities", "ids"->ids}
      ]


(* ::Subsubsection::Closed:: *)
(*extractEntityIDs*)


extractEntityIDs[json_]:=
  StringTrim[
    json[["results", "bindings", All, "item", "value"]],
    "http://www.wikidata.org/entity/"
    ]


(* ::Subsubsection::Closed:: *)
(*aggregateEntityClassDataset*)


$entityPrefix="http://www.wikidata.org/entity/";


aggregateEntityClassDataset[res_]:=
  Module[{resDS, resGroups},
    resDS = Global`ugh = Dataset[Join@@res[[All, "results", "bindings"]]];
    resGroups=
      GroupBy[
        resDS, 
        StringTrim[#["entity", "value"], $entityPrefix]&,
        extractPropertyDataset
        ];
    resGroups
    ]


(* ::Subsubsection::Closed:: *)
(*cleanData*)


$xmlDataTypePrefix="http://www.w3.org/2001/XMLSchema#";


cleanData//Clear;
icleanData[$xmlDataTypePrefix<>"dateTime", val_]:=
  DateObject[val];
icleanData[$xmlDataTypePrefix<>"decimal", val_]:=
  Internal`StringToDouble[val];
icleanData[_, val_]:=
  val;
Function[
  cleanData[k:$xmlDataTypePrefix<>#, val_]:=
    icleanData[k, val]
  ]/@{
  "dateTime",
  "decimal"
  };
cleanData[k_String, val_]:=
  icleanData[$xmlDataTypePrefix<>k, val];
cleanData[_, val_]:=
  val;


(* ::Subsubsection::Closed:: *)
(*extractPropertyDataset*)


extractPropertyDataset[resDS_]:=
  Module[{ resPairs, resMerge },
    resPairs = 
      #propLabel["value"]->cleanData[#val["datatype"], #val["value"]]&/@
        resDS[[All, {"propLabel", "val"}, {"value", "datatype"}]];
    resMerge=
      Merge[resPairs, 
        If[Length[#]==1, #[[1]], #]&
        ];
    KeySortBy[resMerge, #=!="Label"&]
    ]


(* ::Subsubsection::Closed:: *)
(*wikidataEntityClassDataset*)


$wikiDataChunkSize=15;


wikidataEntityClassDataset//Clear
wikidataEntityClassDataset[
  ids:{__String},
  limit_,
  lang_,
  verb_
  ]:=
   Module[
     {
       data,
       chunks,
       chunkSize=$wikiDataChunkSize,
       chunkData
       },
     chunks = Partition[ids, UpTo[chunkSize]];
     data=
       If[verb, wikidataQueryVerbose, wikidataQuery][
         propertyQuery[#, lang],
         limit
         ]&/@chunks;
     If[AllTrue[data, AssociationQ],
       aggregateEntityClassDataset[data],
       data
       ]
     ];
wikidataEntityClassDataset[
  baseData_Association,
  limit_,
  lang_
  ]:=
   Module[
     {
       ids
       },
     ids = extractEntityIDs@baseData;
     wikidataEntityClassDataset[ids, limit, lang]
     ];
wikidataEntityClassDataset[
  entityClass_String, 
  pred_String, 
  limit_Integer,
  lang_String
  ]:=
  Module[{baseData, ids, data},
    baseData= wikidataQuery[entityQuery[entityClass, pred], limit];
    wikidataEntityClassDataset[baseData, limit, lang]
    ]


(* ::Subsubsection::Closed:: *)
(*wikiPropsScraping*)


(*baseURL="https://www.wikidata.org/wiki/Category:Wikidata:List_of_properties";
links:=Import[baseURL, "Hyperlinks"];
propertyLists:=
  Pick[
    #,
    StringMatchQ[
      #,
      "https://www.wikidata.org/wiki/Wikidata:List_of_properties/"~~end:Except["/"]../;end=!="Summary_table"
      ]
    ]&@links;
scrapeProps[link_]:=
  Module[
    {
      xml
      },
    xml=Import[link, {"HTML", "XMLObject"}];
    Cases[
      xml,
      XMLElement[
        "tr",
        _,
        {
          ___,
          XMLElement["td", _, 
            {
              ___,t_String, ___
              }],
          XMLElement["td", _, 
            {___, XMLElement["a", _, {v_}], ___}
            ],
          ___
          }
        ]\[RuleDelayed]t\[Rule]v,
      \[Infinity]
      ]
    ]
pullListIDs[lists_]:=
  AssociationThread[
    URLParse[lists, "Path"][[All, -1]],
    scrapeProps/@lists
    ]*)


(* ::Subsubsection::Closed:: *)
(*$wikiDataProperties*)


(* ::Text:: *)
(*Iteratively scraped off of:*)
(*  https://www.wikidata.org/wiki/Category:Wikidata:List_of_properties*)


$propsURL=
  "https://raw.githubusercontent.com/b3m2a1/mathematica-tools/master/WikiDataProps.wl";


$wikiDataProperties//Clear


If[Length@OwnValues[$wikiDataProperties]==0,
$wikiDataProperties:=
  $wikiDataProperties=
    KeyMap[
      StringRiffle@
        StringSplit[StringTrim[#, "Wikidata_property"], "_"]&,
      Association/@Import[$propsURL]
      ]
];


(* ::Subsubsection::Closed:: *)
(*wikidataRelatedStuff*)


wikidataRelatedStuff[baseData_, baseType_, subType_]:=
  Join@@
   KeyValueMap[
     With[{tag=#},
       KeyMap[tag<>":"<>#&, #2]
       ]&,
     KeySelect[
       #,
       StringContainsQ[subType]
       ]&/@KeySelect[baseData, StringContainsQ[baseType]]
     ];
wikidataRelatedStuff[baseData_, query_]:=
 KeySelect[
   Join@@
     KeyValueMap[
       With[{tag=#},
         KeyMap[tag<>":"<>#&, #2]
         ]&,
        baseData
       ],
   StringContainsQ[query]
   ]


(* ::Subsubsection::Closed:: *)
(*wikidataRelatedProps*)


wikidataRelatedProps[baseType_, subType_]:=
  wikidataRelatedStuff[$wikiDataProperties, baseType, subType];
wikidataRelatedProps[query_]:=
  wikidataRelatedStuff[$wikiDataProperties, query];


(* ::Subsubsection::Closed:: *)
(*wikidataRelatedItems*)


wikidataRelatedItems[baseType_, subType_]:=
  wikidataRelatedStuff[$wikiDataItems, baseType, subType];
wikidataRelatedItems[query_]:=
  wikidataRelatedStuff[$wikiDataItems, query];


(* ::Subsubsection::Closed:: *)
(*getWikiType*)


WikiData::toomany=
  "More than one type could be valid for base type ``. Possibilities are ``.";
WikiData::badcode=
  "Couldn't get type specs for type `` and selector ``";


getWikiType[type_, head_, selector_]:=
  Module[{choices},
    If[StringMatchQ[type, head~~NumberString],
      type,
      choices=selector[type];
      Which[
        !AssociationQ@choices,
          Message[WikiData::badcode, 
            type,
            selector
            ],
        Length@choices===1,
          choices[[1]],
        True,
          Message[WikiData::toomany,
            type,
            StringSplit[Keys@choices, ":"][[All, 1]]
            ]
        ]
      ]  
    ]


(* ::Subsubsection::Closed:: *)
(*WikiDataDataset*)


WikiDataDataset//Clear
Options[WikiDataDataset]=
  {
    "MaxItems"->1000,
    "Language"->Automatic,
    "Verbose"->False,
    "ChunkSize"->15
    };
WikiDataDataset[class_String, predicate:_String:"P31", ops:OptionsPattern[]]:=
  Module[
    {
      limit=OptionValue["MaxItems"],
      lang=OptionValue["Language"],
      pred,
      cls,
      verb=TrueQ@OptionValue["Verbose"],
      chunks=OptionValue["ChunkSize"]
      },
    lang=Replace[lang, Automatic:>$Language];
    If[StringLength[lang]>2, lang=LanguageData[lang, "Codes"][[1]]];
    pred=getWikiType[predicate, "P", wikidataRelatedProps];
    cls=getWikiType[predicate, "Q", wikidataRelatedItems];
    If[StringQ[pred]&&StringQ[cls],
      Block[
        {$wikiDataChunkSize=chunks},
        wikidataEntityClassDataset[cls, pred, limit, lang, verb]
        ],
      Failure["BadQuery",
        <|
         "MessageTemplate"->"Can't process query for class `` and predicate ``",
         "MessageParameters"->{class, predicate}
         |>
        ]
      ]
    ];
WikiDataDataset[ids:{__String}, ops:OptionsPattern[]]:=
  Module[
    {
      limit=OptionValue["MaxItems"],
      lang=OptionValue["Language"],
      chunks=OptionValue["ChunkSize"],
      verb=TrueQ@OptionValue["Verbose"]
      },
   lang=Replace[lang, Automatic:>$Language];
    If[StringLength[lang]>2, lang=LanguageData[lang, "Codes"][[1]]];
    Block[
      {$wikiDataChunkSize=chunks},
      wikidataEntityClassDataset[ids, limit, lang, verb]
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*WikiDataClasses*)


Options[WikiDataClasses]=
  {
    "MaxItems"->1000,
    "Language"->Automatic,
    "Verbose"->False
    };
WikiDataClasses[predicate:_String:"P31", ops:OptionsPattern[]]:=
  Module[
    {
      limit=OptionValue["MaxItems"],
      lang=OptionValue["Language"],
      pred,
      cls,
      verb=TrueQ@OptionValue["Verbose"],
      queryResults
      },
    lang=Replace[lang, Automatic:>$Language];
    If[StringLength[lang]>2, lang=LanguageData[lang, "Codes"][[1]]];
    pred=getWikiType[predicate, "P", wikidataRelatedProps];
    If[StringQ@pred,
      extractEntityIDs@
        If[verb,  
          wikidataQueryVerbose,
          wikidataQuery
          ][entityQuery[cls, pred], limit],
      Failure["BadQuery",
        <|
         "MessageTemplate"->"Can't process query for predicate class ``",
         "MessageParameters"->{predicate}
         |>
        ]
      ]
    ];
WikiDataClasses[class_String, predicate_String, ops:OptionsPattern[]]:=
  Module[
    {
      limit=OptionValue["MaxItems"],
      lang=OptionValue["Language"],
      pred,
      cls
      },
    lang=Replace[lang, Automatic:>$Language];
    If[StringLength[lang]>2, lang=LanguageData[lang, "Codes"][[1]]];
    pred=getWikiType[predicate, "P", wikidataRelatedProps];
    cls=getWikiType[predicate, "Q", wikidataRelatedItems];
    If[StringQ[pred]&&StringQ[cls],
      extractEntityIDs@
        If[verb,  
          wikidataQueryVerbose,
          wikidataQuery
          ][entityQuery[cls, pred], limit],
      Failure["BadQuery",
        <|
         "MessageTemplate"->"Can't process query for class `` and predicate ``",
         "MessageParameters"->{class, predicate}
         |>
        ]
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*WikiDataPredicates*)


groupPreds[res_]:=
  GroupBy[
    Thread[{
      StringSplit[Keys[res], ":"],
      Values[res]
      }],
    #[[1, 1]]&->(#[[1, 2]]->#[[2]]&),
    Association
    ]


WikiDataPredicates[pred_String]:=
  wikidataRelatedProps[pred]
WikiDataPredicates[baseType_String, pred_String]:=
  groupPreds@
    wikidataRelatedProps[baseType, pred]


(* ::Subsubsection:: *)
(*End*)


End[]


(* ::Subsection:: *)
(*End Package*)


EndPackage[]
