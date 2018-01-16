(* ::Package:: *)

(* ::Section:: *)
(*Sheepshead*)


BeginPackage["Sheepshead`", {"PacletManager`"}];


(*Package Declarations*)
PlaySheepshead::usage="PlaySheepshead[] starts a game of Sheepshead
PlaySheepshead[True] restarts the game
";


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*Package Implementation*)


(* ::Subsubsection:: *)
(*Primary Implementation*)


(*

$docConnection
-- Sets up the service connection

*)
$docConnection :=
  (
   If[Length@PacletFind["ServiceConnection_DeckOfCards"]==0,
    PacletInstall[
     "ServiceConnection_DeckOfCards",
     "Site" ->
      "http://www.wolframcloud.com/objects/b3m2a1.paclets/PacletServer"
     ]
    ];
   $docConnection = ServiceConnect["DeckOfCards"]
   );
(*
sheepsheadDeck[]
-- Loads a new deck
*)
sheepsheadDeck[] :=
  $docConnection["New",
   "cards" ->
    StringJoin /@
     Tuples[
      {
       {"7", "8", "9", "0", "K", "A", "J", "Q"},
       {"D", "H", "S", "C"}
       }
      ]
   ];
(*
sheepsheadGame["New"]
-- Initializes a new game
*)
If[! AssociationQ@$sheepsheadGames,
  $sheepsheadGames = <||>
  ];
sheepsheadGame["New"] :=
  With[{d = sheepsheadDeck[]},
   $sheepsheadGames[d["deck_id"]] =
    <|
     "ID" -> d["deck_id"],
     "Hand" -> 0,
     "Dealer" -> 1,
     "Chips" -> AssociationMap[10 &, Range[5]]
     |>;
   sheepsheadGame[d["deck_id"]]
   ];
(*
sheepsheadGame[id_]["Deal"]
-- Deals the cards
*)
sheepsheadGame[id_]["Deal"] :=
  (
   $docConnection["Shuffle", "Deck" -> id];
   $sheepsheadGames[id, "Players"] =
    AssociationMap[
     <|
       "Hand" ->
        SortBy[
         SortBy[
          $docConnection[
           "PileDrawFromDeck",
           "Pile" -> "player_" <> ToString[#],
           "count" -> 6,
           "Deck" -> id
           ],
          #["Suit"] &
          ],
         sheepsheadCardPower[
           Entity["DeckOfCardsPlayingCard", "QC"], #] &
         ],
       "Tricks" -> {},
       "Picker" -> False,
       "Partner" -> False
       |> &,
     Range[5]
     ];
   $sheepsheadGames[id, "Blind"] =
    $docConnection["PileDrawFromDeck",
     "Pile" -> "blind",
     "count" -> 2,
     "Deck" -> id
     ];
   $sheepsheadGames[id, "CalledCard"] =
     None;
   $sheepsheadGames[id, "Leader"] =
    Set[
     $sheepsheadGames[id, "Dealer"],
     Mod[$sheepsheadGames[id, "Dealer"] + 1, 5, 1]
     ];
   $sheepsheadGames[id, "Current"] =
    $sheepsheadGames[id, "Dealer"];
   $sheepsheadGames[id, "Stack"] =
    <||>;
   );
(*
sheepsheadCalledCard[hand_, buried_, suit:"Clubs"|"Spades"|"Hearts"]
-- Determines the called card from the specified suit
*)
sheepsheadCalledCard[hand_, buried_, 
  suit : "Clubs" | "Spades" | "Hearts"] :=
 With[
  {
   testAce =
    Entity["DeckOfCardsPlayingCard", "A" <> StringTake[suit, 1]],
   test10 =
    Entity["DeckOfCardsPlayingCard", "0" <> StringTake[suit, 1]],
   liveAces =
    Complement[
     Entity["DeckOfCardsPlayingCard", "A" <> StringTake[#, 1]] & /@ 
      EntityValue[hand, "Suit"],
     Append[Join[hand, buried], Entity["DeckOfCardsPlayingCard", "AD"]]
     ],
   live10s =
    Complement[
     Entity["DeckOfCardsPlayingCard", "0" <> StringTake[#, 1]] & /@ 
      EntityValue[hand, "Suit"],
     Join[hand, buried]
     ]
   },
  Which[
   Length@liveAces > 0,
   If[MemberQ[liveAces, testAce], testAce, $Failed],
   Length@live10s > 0,
   If[MemberQ[live10s, test10], test10, $Failed],
   True,
   sheepsheadCalledCard::lolwut = 
    "Wow the chances of this happening are miniscule, but you can't call an ace or a 10... Guess you're going it alone!";
   Message[sheepsheadCalledCard::lolwut];
   None
   ]
  ]
(* 
sheepsheadGame[id_]["PickOrPass",picking_]
-- specifies whether the player picked or passed
*)
sheepsheadGame[id_]["PickOrPass", picking_] :=
  Module[
   {
    cur = $sheepsheadGames[id, "Current"],
    picker = TrueQ@picking,
    blind = $sheepsheadGames[id, "Blind"]
    },
   $sheepsheadGames[id, "Players", cur, "Picker"] = picker;
   If[picker,
    $sheepsheadGames[id, "Blind"] = {};
    $sheepsheadGames[id, "Players", cur, "Hand"] =
     Join[
      $sheepsheadGames[id, "Players", cur, "Hand"],
      blind
      ],
    $sheepsheadGames[id, "Current"] =
     Mod[$sheepsheadGames[id, "Current"] + 1, 5, 1]
    ]
   ];
(* 
sheepsheadGame[id_]["DiscardAndCall", d:{_, _}, \
calledSuit:"Clubs"|"Spades"|"Hearts"]
-- picker discards 2 cards and call a suit
*)
sheepsheadGame[id_][
   "DiscardAndCall", 
   d : {_, _},
   calledSuit : "Clubs" | "Spades" | "Hearts"
   ] :=
  Module[
   {
    cur = $sheepsheadGames[id, "Current"],
    hand
    },
   hand = $sheepsheadGames[id, "Players", cur, "Hand"];
   If[AllTrue[d, MemberQ[hand, #] &],
    With[
     {
      c =
       sheepsheadCalledCard[Complement[hand, d], d, calledSuit]
      },
     If[c =!= $Failed,
      $sheepsheadGames[id, "Players", cur, "Hand"] =
       Complement[hand, d];
      $sheepsheadGames[id, "CalledCard"] = c;
      $sheepsheadGames[id, "Current"] =
       $sheepsheadGames[id, "Leader"]
      ];
     c
     ],
    $Failed
    ]
   ];
(*
sheepsheadTrumpQ[card_]
-- tests whether a card is trump or fail
*)
sheepsheadTrumpQ[card_] :=
  
  MemberQ[ {"Queen", "Jack"}, card["Value"]] ||
   card["Suit"] == "Diamonds";
(*
sheepsheadValidateCard[leadCard_, hand_, card_]
-- validates an attempted play
*)
sheepsheadValidateCard[leadCard_, hand_, card_, calledCard_] :=
  
  If[!sheepsheadTrumpQ[leadCard]&&
    leadCard["Suit"] == calledCard["Suit"] &&
    MemberQ[hand, calledCard],
   card == calledCard,
   If[sheepsheadTrumpQ[leadCard],
    sheepsheadTrumpQ[card] || Not[AnyTrue[hand, sheepsheadTrumpQ]],
    (!sheepsheadTrumpQ[card] && leadCard["Suit"] == card["Suit"]) ||
     Not[AnyTrue[hand, leadCard["Suit"] == #["Suit"] &]]
    ]
   ];
(* 
sheepsheadGame[id_]["Play", card_]
-- attempt to play card
 *)
sheepsheadGame[id_]["Play", card_] :=
  Module[
   {
    cur = $sheepsheadGames[id, "Current"],
    hand,
    lead =
     If[Length@$sheepsheadGames[id, "Stack"] == 0, 
      None,
      $sheepsheadGames[id, "Stack"][$sheepsheadGames[id, "Leader"]]
      ]
    },
   hand =
    $sheepsheadGames[id, "Players", cur, "Hand"];
   If[
    (
      lead === None ||
       
       sheepsheadValidateCard[lead, hand, card,
        $sheepsheadGames[id, "CalledCard"]
         ]
      ) && MemberQ[hand, card],
    $sheepsheadGames[id, "Stack", cur] =
     card;
    $sheepsheadGames[id, "Players", cur, "Hand"] =
     DeleteCases[hand, card];
    If[Length@$sheepsheadGames[id, "Stack"] == 5,
     sheepsheadGame[id]["EndTrick"],
     $sheepsheadGames[id, "Current"] =
       Mod[cur + 1, 5, 1];
     ],
    $Failed
    ]
   ];
(*
sheepsheadCardPower[leadCard_, card_]
-- calculates card power
*)
sheepsheadCardPower[leadCard_, card_] :=
  
  If[sheepsheadTrumpQ[card], 
    10000 +
     Switch[card["Suit"],
      "Clubs", 500,
      "Spades", 400,
      "Hearts", 300,
      "Diamonds", 200
      ],
    If[leadCard["Suit"] == card["Suit"], 100, 0]
    ] +
   Switch[card["Value"],
    "Queen", 1000,
    "Jack", 500,
    "Ace", 11,
    "10", 10,
    "King", 4,
    "9", 3,
    "8", 2,
    "7", 1
    ];
(* 
sheepsheadGame[id_]["EndTrick"]
-- Figure out who took the trick and end the hand if done 
*)
sheepsheadGame[id_]["EndTrick"] :=
 Module[
  {
   stack = $sheepsheadGames[id, "Stack"],
   lead = $sheepsheadGames[id, "Stack"][$sheepsheadGames[id, 
      "Leader"]],
   winner
   },
  winner = Last@Keys@SortBy[stack, sheepsheadCardPower[lead, #] &];
  $sheepsheadGames[id, "Players", winner, "Tricks"] =
   Join[
    $sheepsheadGames[id, "Players", winner, "Tricks"],
    Values[$sheepsheadGames[id, "Stack"]]
    ];
  $sheepsheadGames[id, "Stack"] =
   <||>;
  If[Length@$sheepsheadGames[id, "Players", winner, "Hand"] > 0,
   $sheepsheadGames[id, "Leader"] = winner;
   $sheepsheadGames[id, "Current"] = winner,
   sheepsheadGame[id]["EndHand"]
   ]
  ]
(*
sheepsheadCardPoints[cards:{__Entity}]
-- calculate the points associated with the cards
*)
sheepsheadCardPoints[cards : {__Entity}] :=
 Replace[
  EntityValue[cards, "Value"],
  {
   "Ace" -> 11,
   "10" -> 10,
   "King" -> 4,
   "Queen" -> 3,
   "Jack" -> 2,
   _ -> 0
   },
  {1}
  ]
(* 
sheepsheadGame[id_]["ChangePoints", player_,inc_]
-- increase or decrease a players chip count
*)
sheepsheadGame[id_]["ChangePoints", player_, 
  inc_] :=
 $sheepsheadGames["Chips", player] += inc
(* 
sheepsheadGame[id_]["EndHand"]
-- end a hand and assign points
 *)
sheepsheadGame[id_]["EndHand"] :=
 Module[
  {
   tricks =
    AssociationMap[
     $sheepsheadGames[id, "Players", #, "Tricks"] &,
     Range[5]
     ],
   picker =
    SelectFirst[Range[5],
     $sheepsheadGames[id, "Players", #, "Picker"] &
     ],
   partner =
    SelectFirst[Range[5],
     $sheepsheadGames[id, "Players", #, "Partner"] &,
     Nothing
     ],
   others,
   pickerPoints
   },
  tricks =
   sheepsheadCardPoints /@ tricks;
  others =
   Complement[Range[5], {picker, partner}];
  pickerPoints =
   Total[Lookup[tricks, {picker, partner}]];
  Which[
   pickerPoints == 0,
   (* Picker got no tricked, so picker loses 6, partner loses 3 *)
   
   sheepsheadGame[id]["ChangePoints", picker, -6];
   If[IntegerQ@partner,
    sheepsheadGame[id]["ChangePoints", partner, -3]
    ];
   sheepsheadGame[id]["ChangePoints", #, +3] & /@ others;,
   pickerPoints < 31,
   (* Picker got a trick but no Schneider, so picker loses 4, 
   partner loses 2 *)
   
   sheepsheadGame[id]["ChangePoints", picker, -4];
   If[IntegerQ@partner,
    sheepsheadGame[id]["ChangePoints", partner, -4]
    ];
   sheepsheadGame[id]["ChangePoints", #, +2] & /@ others;,
   pickerPoints < 61,
   (* Picker lost, so picker loses 2, partner loses 1 *)
   
   sheepsheadGame[id]["ChangePoints", picker, -2];
   If[IntegerQ@partner,
    sheepsheadGame[id]["ChangePoints", partner, -1]
    ];
   sheepsheadGame[id]["ChangePoints", #, +1] & /@ others;,
   pickerPoints < 120 - 30,
   (* Others got Schneider, so picker gets 2, partner gets 1 *)
   
   sheepsheadGame[id]["ChangePoints", picker, +2];
   If[IntegerQ@partner,
    sheepsheadGame[id]["ChangePoints", partner, +1]
    ];
   sheepsheadGame[id]["ChangePoints", #, -1] & /@ others;,
   pickerPoints < 120,
   (* Others got a trick but no Schneider, so picker gets 4, 
   partner gets 2 *)
   sheepsheadGame[id]["ChangePoints", picker, +4];
   If[IntegerQ@partner,
    sheepsheadGame[id]["ChangePoints", partner, +2]
    ];
   sheepsheadGame[id]["ChangePoints", #, -2] & /@ others;,
   pickerPoints == 120,
   (* Others got no tricked, so picker gets 6, partner gets 3 *)
   
   sheepsheadGame[id]["ChangePoints", picker, +6];
   If[IntegerQ@partner,
    sheepsheadGame[id]["ChangePoints", partner, +3]
    ];
   sheepsheadGame[id]["ChangePoints", #, -3] & /@ others;
   ];
  ]
(* 
$sheepsheadCardBack
-- the cardback image for players other than the current 
*)
If[! MatchQ[OwnValues[$sheepsheadCardBack], {_ :> _Image}],
  $sheepsheadCardBack :=
   $sheepsheadCardBack =
    ImageResize[
     ImageTake[
      Import["http://www.jimknapp.com/Cards/Bicycle_files/image002.jpg"], All, {225, -1}
      ],
     {226, 314}
     ]
  ];
(*
sheepsheadCardGraphic[card_,origin_, angle_, size_]
-- generate a Graphics object for a card 
*)
sheepsheadCardGraphic[card_, origin_, angle_, size_] :=
  
  If[Abs@angle > 0,
    Rotate[#, angle, {0, 0}] &,
    Identity
    ]@
   Inset[
    Replace[card["Image"], Except[_?ImageQ] :> $sheepsheadCardBack],
     origin,
    {Center, Center},
    size
    ];
(*
sheepsheadGameBoard[sheepsheadGame[id_], refreshFunction_:Null]
-- make a game board to play on
*)
sheepsheadGameBoard[sheepsheadGame[id_],
  refreshFunction_: Null
  ] :=
 Module[
  {
   vertices = CirclePoints[{13, \[Pi]/2}, 5],
   hand,
   lead = $sheepsheadGames[id, "Leader"],
   current = $sheepsheadGames[id, "Current"],
   stack = $sheepsheadGames[id, "Stack"],
   blind = $sheepsheadGames[id, "Blind"],
   calledCard = $sheepsheadGames[id, "CalledCard"],
   tricks =
    AssociationMap[
     Floor[Length@$sheepsheadGames[id, "Players", #, "Tricks"]/5] &,
     Range[5]
     ],
   picker =
    SelectFirst[
     Range[5],
     $sheepsheadGames[id, "Players", #, "Picker"] &,
     1
     ]
   },
  hand = $sheepsheadGames[id, "Players", current, "Hand"];
  stack = KeySortBy[stack, Abs[current - #] &];
  Graphics[
   {
    Darker[Green, .8],
    Disk[{0, 0}, 15],
    Darker@Green,
    Polygon[vertices],
    If[Length@hand == 0,
     (* Hand over so redeal *)
     Inset[
      Button["Redeal",
       sheepsheadGame[id]["Deal"];
       refreshFunction[]
       ],
      {0, 0}
      ],
     {
      (* Backside graphics for non current hands*)
      
      With[{handl = Min@{Length@hand, 6}/2 + .5},
       Table[
        Map[
         sheepsheadCardGraphic[
           "Back",
           {0, 1} +
            {-1, -1}*vertices[[1]] +
            (# - handl)*{2, 0},
           n*2 \[Pi]/5,
           3
           ] &,
         Range[
          Min@{Length@hand, 6} -
           Boole@KeyMemberQ[stack, Mod[current + n, 5, 1]]
          ]
         ],
        {n, 4}
        ]
       ],
      (* Current player's hand *)
      Which[
       Length@blind > 0,
       With[{handl = Length@hand/2 + .5},
        MapIndexed[
         With[{
            lc =
             {0, 2.5} +
              {-1, -1}*vertices[[1]] +
              (#2[[1]] - handl)*{2, 0}
            },
           sheepsheadCardGraphic[#, lc, 0, 5]
           ] &, 
         hand
         ]
        ],
       Length@hand > 6,
       (* Need to discard and call *)
       DynamicModule[
        {
         discardSel = {}, lastDisc,
          h = hand, v = vertices,
         badChoices
         },
        Dynamic[
         {
          White,
          If[Length@discardSel < 2,
           badChoices = {};
           Text[
            "Select two cards to discard",
            {0, 0}
            ],
           Text[
            "Choose a fail suit to call",
            {0, 0}
            ]
           ],
          With[{handl = Length@h/2 + .5},
           MapIndexed[
            With[
              {
               lc =
                {0, If[MemberQ[discardSel, # ], 3.5, 2.5]} +
                 {-1, -1}*v[[1]] +
                 (#2[[1]] - handl)*{2, 0}
               },
              If[MemberQ[badChoices, #],
               {
                Red,
                Rectangle[lc - 2.6*{1, 314/226}, lc + 2.6*{1, 314/226},
                 RoundingRadius -> .5
                 ],
                sheepsheadCardGraphic[#, lc, 0, 5]
                },
               Mouseover[
                sheepsheadCardGraphic[#, lc, 0, 5],
                EventHandler[
                 {
                  LightBlue,
                  
                  Rectangle[lc - 2.6*{1, 314/226}, 
                   lc + 2.6*{1, 314/226},
                   RoundingRadius -> .5
                   ],
                  sheepsheadCardGraphic[#, lc, 0, 5]
                  },
                 {
                  "MouseClicked" :>
                   (
                    If[MemberQ[discardSel, #],
                    discardSel =
                    DeleteCases[discardSel, #],
                    AppendTo[discardSel, #]
                    ];
                    If[Length@discardSel == 3,
                    With[{called = discardSel[[-1]]},
                    If[! sheepsheadTrumpQ@called,
                    Replace[
                    sheepsheadGame[id][
                    "DiscardAndCall",
                    discardSel[[;; 2]],
                    called["Suit"]
                    ],
                    {
                    $Failed :>
                    (
                    discardSel =
                    discardSel[[;; 2]];
                    AppendTo[badChoices, called]
                    ),
                    Except[$Failed] :>
                    refreshFunction[]
                    }
                    ]
                    ]
                    ];
                    ];
                    )
                  }
                 ]
                ]
               ]
              ] &,
            h
            ]
           ]
          },
         TrackedSymbols :> {discardSel, badChoices}
         ]
        ],
       True,
       With[{handl = Length@hand/2 + .5},
        MapIndexed[
         With[{
            lc =
             {0, 2.5} +
              {-1, -1}*vertices[[1]] +
              (#2[[1]] - handl)*{2, 0}
            },
           Mouseover[
            sheepsheadCardGraphic[#, lc, 0, 5],
            EventHandler[
             {
              LightBlue,
              Rectangle[lc - 2.6*{1, 314/226}, lc + 2.6*{1, 314/226},
               RoundingRadius -> .5
               ],
              sheepsheadCardGraphic[#, lc, 0, 5]
              },
             {
              "MouseClicked" :>
               (
                sheepsheadGame[id]["Play", #];
                refreshFunction[]
                )
              }
             ]
            ]
           ] &,
         hand
         ]
        ]
       ],
      Which[
       Length@blind > 0,
       {
        sheepsheadCardGraphic[
         "Back",
         {-1, 1},
         0,
         5
         ],
        sheepsheadCardGraphic[
         "Back",
         {1, 1},
         0,
         5
         ],
        Inset[
         Button[
          "Pick",
          sheepsheadGame[id]["PickOrPass", True];
          refreshFunction[]
          ],
         {0, 1}
         ],
        Inset[
         Button[
          "Pass",
          sheepsheadGame[id]["PickOrPass", False];
          refreshFunction[]
          ],
         {0, -1}
         ]
        },
       Length@hand > 6,
       {},
       True,
       (* Cards on the table *)
       MapIndexed[
        sheepsheadCardGraphic[
          #,
          {-3, 4} + {-1, -1}*vertices[[1]],
          -#2[[1]]*2 \[Pi]/5,
          5
          ] &,
        Values@stack
        ]
       ],
      (* Player identifiers *)
      Map[
       Rotate[
         With[{pl = Mod[current + # - 1, 5, 1]},
          {
           Switch[pl,
            current, Yellow,
            lead, Red,
            _, White
            ],
           Text[
            pl, 
            {7, .5} +
             {-1, -1}*vertices[[1]]
            ],
           If[pl === picker&&Length@blind==0,
            {
             Green,
             Text[
              "P", 
              {7, 2} + {-1, -1}*vertices[[1]]
              ],
             Pink,
             If[calledCard=!=None,
               Text[
                CanonicalName@calledCard, 
                {8, 2} + {-1, -1}*vertices[[1]]
                ],
              {}
              ]
             },
            {}
            ],
           White,
           Text[
            tricks[pl], 
            {8, .5} +
             {-1, -1}*vertices[[1]]
            ]
           }
          ],
         (# - 1)*2 \[Pi]/5,
         {0, 0}
         ] &,
       Range[5]
       ]
      }
     ]
    }
   ]
  ]
(*
sheepsheadGameInterface[game_]
-- creates a dynamic game interface for game
*)
sheepsheadGameInterface[game_] :=
 DynamicModule[
   {refreshFlag},
   Dynamic[
    refreshFlag;
    sheepsheadGameBoard[
     game,
     Function[
      refreshFlag = RandomReal[]
      ]
     ],
    TrackedSymbols :> {refreshFlag}
    ]
   ] // Deploy


(* ::Subsection:: *)
(*PlaySheepshead*)


PlaySheepshead[redeal : True | False : False] :=
 
 Module[{redealFlag = redeal},
  If[! MatchQ[$currentSheepsheadGame, 
     sheepsheadGame[_?(KeyMemberQ[$sheepsheadGames, #] &)]],
   $currentSheepsheadGame = sheepsheadGame["New"];
   redealFlag = True
   ];
  If[redealFlag, $currentSheepsheadGame["Deal"]];
  sheepsheadGameInterface@$currentSheepsheadGame
  ]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
