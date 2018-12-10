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


(* ::Subsubsection::Closed:: *)
(*ho*)


ho[{omega_, m_, hb_}, x_][n_]:=
  With[{a=m*omega/hb},
    (1/Sqrt[(2^n)n!])*Power[a/Pi, 1/4]*Exp[-a/2*x^2]*HermiteH[n, Sqrt[a]*x]
    ];
ho[{omega_, m_, hb_}]:=
  Function[Null, ho[{omega, m, hb}, #], HoldFirst];
ho~SetAttributes~HoldRest;


(* ::Subsubsection::Closed:: *)
(*getBasisFunction*)


getBasisFunction//Clear;
getBasisFunction["ParticleInABox", ops___]:=
  pib[
    {
     Lookup[{ops}, "Length", 1.],
     Lookup[{ops}, "Minimum", 0.]
     }
    ];
getBasisFunction["HarmonicOscillator", ops___]:=
  ho[
    {
     Lookup[{ops}, "Frequency", 1.],
     Lookup[{ops}, "Mass", 1],
     Lookup[{ops}, "PlanckConstant", 1]
     }
    ];
getBasisFunction[l_List]:=
  getBasisFunction@@l;
getBasisFunction[___]:=
  $Failed;


(* ::Subsubsection:: *)
(*hel*)


$integrator=NIntegrate; 
(* I put this here so people can hack it later *)


hel[{n_, m_}, bf_, pot_, {hb_, mu_}, {min_, max_}, x_Symbol]:=
  hel[{n, m}, bf, pot, {hb, mu},{min, max}, x]=
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
      ];


(* ::Subsubsection::Closed:: *)
(*ham*)


ham[nT_, bf_, pot_,{hb_, mu_}, {min_, max_},  x_]:=
  Table[hel[{n ,m}, bf, pot,{hb, mu}, {min, max}, x], {n, 1, nT}, {m, 1, nT}];


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
          {10^4, #<=0},
          {0, 0<#<1},
          {10^4, #>=1}
          }]&
        ),
    "Range"->{0., 1.}
    |>;


(* ::Subsubsection:: *)
(*WavefunctionEigensystem*)


Options[WavefunctionEigensystem]=
  {
    "BasisFunction"->Automatic,
    "PotentialFunction"->Automatic,
    "Range"->Automatic,
    "BasisSize"->Automatic,
    "Mass"->Automatic,
    "PlanckConstant"->Automatic
    };
WavefunctionEigensystem[ops:OptionsPattern[]]:=
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
    h=ham[nT, bf, pot, {hb, m}, r,  \[FormalX]];
    es=getSolns@h;
    es[[2]]=Threshold[es[[2]], 10^-6];
    es
    ]


(* ::Subsubsection::Closed:: *)
(*Wavefunctions*)


Options[Wavefunctions]=
  Options[WavefunctionEigensystem];
Wavefunctions[es:{_List, _List}, bf:Except[_?OptionQ]]:=
  {es[[1]], expandSoln[{#, bf}, \[FormalX]]&/@es[[2]]};
Wavefunctions[es:{_List, _List}, ops:OptionsPattern[]]:=
  Module[
    {
      bf=
        Replace[OptionValue["BasisFunction"], 
          Automatic:>$defaults["BasisFunction"]]
      },
    bf=getBasisFunction@bf;
    Wavefunctions[es, bf]
    ];
Wavefunctions[ops:OptionsPattern[]]:=
  Wavefunctions[WavefunctionEigensystem[ops], ops]


(* ::Subsubsection:: *)
(*WavefunctionPlot*)


WavefunctionPlot//Clear
Options[WavefunctionPlot]=
  Join[
    Options[Wavefunctions],
    Options[Plot],
    {
      "WavefunctionScaling"->1
      }
    ];
WavefunctionPlot[
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
WavefunctionPlot[
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
    WavefunctionPlot[wfs, pot, range, ops]
    ];
WavefunctionPlot[
  es:{_List, _List?(MatrixQ[#, Internal`RealValuedNumberQ]&)}, 
  ops:OptionsPattern[]
  ]:=
  WavefunctionPlot[
    Wavefunctions[es, FilterRules[{ops}, Options[Wavefunctions]]],
    ops
    ];
WavefunctionPlot[ops:OptionsPattern[]]:=
  WavefunctionPlot[
    Wavefunctions[FilterRules[{ops}, Options[Wavefunctions]]],
    ops
    ]


(* ::Subsubsection:: *)
(*End*)


End[]


(* ::Subsection::Closed:: *)
(*EndPackage*)


EndPackage[]
