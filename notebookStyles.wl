(* ::Package:: *)

processSheet//Clear
processSheet[{nb_NotebookObject, {implements_NotebookObject, ___}}]:=implements;
processSheet[_]:=Nothing;
getSheet//Clear;
getSheet[nb_NotebookObject]:=
  processSheet@Lookup[NotebookInformation[nb], "StyleDefinitions", {}];
getSheet[e_]:=e;
getStyleSheetChain[nb_NotebookObject]:=
  Rest@
    Module[{s=<||>},
      FixedPointList[
        getSheet, 
        Replace[Lookup[NotebookInformation[nb], "StyleDefinitions", {}],
          {
            {n_NotebookObject}:>n,
            {n_NotebookObject, {i_, ___}}:>i
            }
          ]
        ]
      ]


mergeStyles[styles_]:=
  Merge[
    {
      styles
      },
    If[AnyTrue[#, ListQ[#]&&OptionQ[#]&],
      Merge[Flatten@#, Last],
      Last@#
      ]&
    ];
mergeStyleSet[styles_]:=
  Merge[
    Normal/@styles,
    mergeStyles
    ];


getStyleChain[nbs:{__NotebookObject}]:=
  Module[
    {
      cells=Cells/@nbs,
      names,
      ops
      },
    ops=ConstantArray[<||>, Length@nbs];
    MapIndexed[
      Replace[
        #,
        {
          Cell[StyleData[name:_String|All, env:_String|None:None], o___?OptionQ]:>
            Set[
              ops[[#2[[1]], Key@If[env===None, name, {name, env}] ]],
              Merge[
                {
                  Normal@Replace[
                      ops[[#2[[1]], Key@If[env===None, name, {name, env}]]], 
                      Except[_Association]:><||>
                      ],
                  o
                  },
                Last
                ]
              ],
          c:Cell[
            StyleData[
              name:_String|All, env:_String|None:None,
              StyleDefinitions->StyleData[parent_]
              ], 
            o___?OptionQ
            ]:>
            With[
              {
                inherited=
                  mergeStyles@
                    Flatten@List@Lookup[ops[[;;#2[[1]]-1]], Key@parent, <||>]
                },
              Set[
                ops[[#2[[1]],Key@If[env===None, name, {name, env}] ]],
                Merge[
                  {
                    Replace[
                      ops[[#2[[1]], Key@If[env===None, name, {name, env}]]], 
                      Except[_Association]-><||>
                      ],
                    o
                    },
                  Last
                  ]
                ]
              ]
          },
        1
        ]&,
      NotebookRead/@cells
      ];
    mergeStyleSet@ops
    ]
