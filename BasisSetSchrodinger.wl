(* ::Package:: *)

(* ::Section:: *)
(*BasisSetSchrodinger*)


BeginPackage["BasisSetSchrodinger`"];


WavefunctionEigensystem::usage="";
Wavefunctions::usage="";
WavefunctionPlot::usage="";


(* ::Subsection:: *)
(*Package*)


BeginPackage["`Package`"];


pib::usage="";
ho::usage="";
getBasisFunction::usage="";


getPotentialFunction::usage="";


hel::usage="";
ham::usage="";


getSolns::usage="";
expandSoln::usage="";


shiftScaledWfns::usage="";


$defaults::usage="";


iWavefunctionEigensystem::usage="";
iWavefunctions::usage="";
iWavefunctionPlot::usage="";


EndPackage[]


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*pib*)


pib[{l_, min_}, x_][n_]:=
  With[{max=min+l},
    Piecewise[
      {
        {0, x<=min },
        {Sqrt[2/l]*Sin[(n*Pi/l)*x], min<x<max},
        {0, x>=max}
        }
      ]
    ];
pib[{l_, min_}]:=
  Function[Null, pib[{l, min}, #], HoldFirst];
pib~SetAttributes~HoldRest;


(* ::Subsubsection:: *)
(*ho*)


ho[{omega_, m_, hb_, x0_}, x_][nn_]:=
  With[{a=m*omega/hb, n=nn-1},
    (1/Sqrt[(2^n)n!])*Power[a/Pi, 1/4]*Exp[-a/2*(x-x0)^2]*HermiteH[n, Sqrt[a]*(x-x0)]
    ];
ho[{omega_, m_, hb_, x0_}]:=
  Function[Null, ho[{omega, m, hb, x0}, #], HoldFirst];
ho~SetAttributes~HoldRest;


(* ::Subsubsection:: *)
(*getBasisFunction*)


getBasisFunction//Clear;
getBasisFunction["ParticleInABox", ops___]:=
  With[{oop=Select[{ops}, OptionQ]},
    pib[
      {
       Lookup[oop, "Length", 1.],
       Lookup[oop, "Minimum", 0.]
       }
      ]
    ];
getBasisFunction["HarmonicOscillator", ops___]:=
  With[{oop=Select[{ops}, OptionQ]},
    ho[
      {
       Lookup[oop, "Frequency", 1.],
       Lookup[oop, "Mass", 1],
       Lookup[oop, "PlanckConstant", 1],
       Lookup[oop, "Center", 0.]
       }
      ]
    ];
getBasisFunction[l_List]:=
  getBasisFunction@@l;
getBasisFunction[___]:=
  $Failed;


(* ::Subsubsection:: *)
(*hel*)


$integrator=NIntegrate; 
(* I put this here so people can hack it later *)


ihel[{n_, m_}, bf_, pot_, {hb_, mu_}, {min_, max_}, x_Symbol]:=
  With[{psin=bf[x][n], psim=bf[x][m], p=pot[x]},
    Quiet[
      $integrator[
          psin*(
            -hb/(2mu)*D[psim, {x, 2}]+p*psim
            ), 
        {x, min, max}
        ],
      {
        NIntegrate::ncvb,
        NIntegrate::slwcon
        }
      ]
    ]


hel[{n_, m_}, bf_, pot_, {hb_, mu_}, {min_, max_}, x_Symbol, useCached_]:=
  Module[{res},
    res=ihel[{n, m}, bf, pot, {hb, mu},{min, max}, x];
    If[NumericQ@res,
      If[useCached,
        hel[{n, m}, bf, pot, {hb, mu},{min, max}, x, True]=res,
        res
        ],
      Throw@
        Failure["BadHam",
          <|
            "MessageTemplate"->"Non-numeric Hamiltonian element ``",
            "MessageParameter"->{res}
            |>
          ]
      ]
    ]


(* ::Subsubsection:: *)
(*ham*)


ham//Clear
ham[nT_, bf_, pot_,{hb_, mu_}, {min_, max_},  x_, useCached_]:=
  Table[
    hel[{n ,m}, bf, pot,{hb, mu}, {min, max}, x, useCached], 
    {n, 1, nT}, 
    {m, 1, nT}
    ];


(* ::Subsubsection::Closed:: *)
(*getSolns*)


getSolns[rep_]:=
  Module[{es=Eigensystem[rep], resorting, phasing},
    resorting=Ordering[es[[1]]];
    phasing=Sign@es[[2, 1]];
    es=#[[resorting]]&/@es;
    es
    ]


(* ::Subsubsection::Closed:: *)
(*expandSoln*)


expandSoln[{vec_, bf_}, x_]:=
  Dot[vec, bf[x]/@Range[Length@vec]];
expandSoln~SetAttributes~HoldRest


(* ::Subsubsection::Closed:: *)
(*shiftScaledWfns*)


shiftScaledWfns[es_, scaling_]:=
  MapThread[
    #+scaling*#2&,
    es
    ]


(* ::Subsubsection:: *)
(*$defaults*)


$defaults=
  <|
    "BasisFunction"->{"ParticleInABox", "Length"->1.},
    "PlanckConstant"->1,
    "Mass"->1,
    "BasisSize"->5,
    "PotentialFunction"->
      (
        Piecewise[{
          {10^6, #<=0},
          {0, 0<#<1},
          {10^6, #>=1}
          }]&
        ),
    "Range"->{0., 1.}
    |>;


(* ::Subsubsection:: *)
(*WavefunctionEigensystem*)


Options[iWavefunctionEigensystem]=
  {
    "BasisFunction"->Automatic,
    "PotentialFunction"->Automatic,
    "Range"->Automatic,
    "BasisSize"->Automatic,
    "Mass"->Automatic,
    "PlanckConstant"->Automatic,
    "UseCachedElements"->False
    };
iWavefunctionEigensystem[ops:OptionsPattern[]]:=
  Module[
    {
      nT=  
        Replace[OptionValue["BasisSize"], 
          Except[_Integer]:>$defaults["BasisSize"]],
      hb=
        Replace[OptionValue["PlanckConstant"], 
          Except[_?NumericQ]:>$defaults["PlanckConstant"]],
      m=
        Replace[OptionValue["Mass"], 
          Except[_?NumericQ]:>$defaults["Mass"]],
      r=
        Replace[OptionValue["Range"], 
          Except[{_?NumericQ, _?NumericQ}]:>$defaults["Range"]],
      bf=
        Replace[OptionValue["BasisFunction"], 
          Automatic:>$defaults["BasisFunction"]],
      pot=
        Replace[OptionValue["PotentialFunction"], 
          Automatic:>$defaults["PotentialFunction"]],
      h,
      es
      },
    bf=getBasisFunction@bf;
    h=ham[nT, bf, pot, {hb, m}, r,  \[FormalX], TrueQ@OptionValue["UseCachedElements"]];
    es=getSolns@h;
    es[[2]]=Threshold[es[[2]], 10^-6];
    es
    ]


WavefunctionEigensystem//ClearAll
Options[WavefunctionEigensystem]=
  Options[iWavefunctionEigensystem];
WavefunctionEigensystem[e___]:=
  Module[{res=Catch@iWavefunctionEigensystem[e]},
    res/;Head[res]=!=iWavefunctionEigensystem
    ]


(* ::Subsubsection::Closed:: *)
(*Wavefunctions*)


Options[iWavefunctions]=
  Options[iWavefunctionEigensystem];
iWavefunctions[es:{_List, _List}, bf:Except[_?OptionQ]]:=
  {es[[1]], expandSoln[{#, bf}, \[FormalX]]&/@es[[2]]};
iWavefunctions[es:{_List, _List}, ops:OptionsPattern[]]:=
  Module[
    {
      bf=
        Replace[OptionValue["BasisFunction"], 
          Automatic:>$defaults["BasisFunction"]]
      },
    bf=getBasisFunction@bf;
    iWavefunctions[es, bf]
    ];
iWavefunctions[ops:OptionsPattern[]]:=
  iWavefunctions[iWavefunctionEigensystem[ops], ops]


Wavefunctions//ClearAll
Options[Wavefunctions]=
  Options[iWavefunctions];
Wavefunctions[e___]:=
  Module[{res=Catch@iWavefunctions[e]},
    res/;Head[res]=!=iWavefunctions
    ]


(* ::Subsubsection::Closed:: *)
(*WavefunctionPlot*)


iWavefunctionPlot//Clear
Options[iWavefunctionPlot]=
  Join[
    Options[Wavefunctions],
    Options[Plot],
    {
      "WavefunctionScaling"->1
      }
    ];
iWavefunctionPlot[
  wfs:{_List, _List?(Not@MatrixQ[#, Internal`RealValuedNumberQ]&)},
  pot_,
  {min_?NumericQ, max_?NumericQ},
  ops:OptionsPattern[]
  ]:=
  Plot[
    Evaluate@
      Prepend[shiftScaledWfns[wfs, OptionValue["WavefunctionScaling"]], pot[\[FormalX]]], 
    {\[FormalX], min, max},
    Evaluate@FilterRules[
      {
        ops,
        PlotStyle->Prepend[ColorData[97]/@Range[15], Directive[Dashed, Gray]],
        PlotRange->All
        },
      Options[Plot]
      ]
    ];
iWavefunctionPlot[
  wfs:{_List, _List?(Not@MatrixQ[#, Internal`RealValuedNumberQ]&)},
  ops:OptionsPattern[]
  ]:=
  Module[
    {
      pot=
        Replace[OptionValue["PotentialFunction"], 
          Automatic:>$defaults["PotentialFunction"]
          ],
      range=
        Replace[OptionValue["Range"], 
          Except[{_?NumericQ, _?NumericQ}]:>
            (
              (1.1*(#-Mean[#])+Mean[#])&@$defaults["Range"]
              )
          ]
      },
    iWavefunctionPlot[wfs, pot, range, ops]
    ];
iWavefunctionPlot[
  es:{_List, _List?(MatrixQ[#, Internal`RealValuedNumberQ]&)}, 
  ops:OptionsPattern[]
  ]:=
  iWavefunctionPlot[
    iWavefunctions[es, FilterRules[{ops}, Options[iWavefunctions]]],
    ops
    ];
iWavefunctionPlot[ops:OptionsPattern[]]:=
  iWavefunctionPlot[
    iWavefunctions[FilterRules[{ops}, Options[iWavefunctions]]],
    ops
    ]


WavefunctionPlot//ClearAll
Options[WavefunctionPlot]=
  Options[iWavefunctionPlot];
WavefunctionPlot[e___]:=
  Module[{res=Catch@iWavefunctionPlot[e]},
    res/;Head[res]=!=iWavefunctionPlot
    ]


(* ::Subsubsection:: *)
(*End*)


End[]


(* ::Subsection::Closed:: *)
(*EndPackage*)


EndPackage[]
