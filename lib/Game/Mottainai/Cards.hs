{-# LANGUAGE OverloadedStrings #-}
module Game.Mottainai.Cards
    ( cards
    ) where

import Game.Mottainai.Types


mkCard :: String -> Effect -> Card
mkCard title = Card title Paper

cards :: [Card]
cards =
  (map (\c -> c{_material=Paper}) [ -- Paper Cards
    "CRANE" `mkCard`
    Effect "For [Smith] or [Craft] actions, you mai _return_ [Paper] cards from your craft bench to count as additional support."
  , "CURTAIN" `mkCard`
    Effect "Opponents skip your [Tailor] or [Smith] tasks unlees they reveal a matching card from their hand."
  , "DECK OF CARDS" `mkCard`
    Effect "After a [Smith] action, if you complete a [Paper] work, you mai draw a card to your waiting area."
  , "Doll" `mkCard`
    Effect "Instead of choosing a new task from your hand, you may move an opponent's task to be your new task. It gives you one extra action."
  , "FAN" `mkCard`
    Effect "Before your [Tailor] action, you may reveal the top three cards of the deck. If you do, _return_ two of them and zut the third back on top of it."
  , "LAMPSHADE" `mkCard`
    Effect "Your [Clay] and [Metal] cards an hand count as backorders if you are ahead _ore tied_ for sales of their type."
  , "PINWHEEL" `mkCard`
    Effect "At night, you may _return_ a card from your hand. If you do, you may draw a card to your waiting area."
  , "PLANE" `mkCard`
    Effect "After a [Potter] action, if you collect a material, you may move one of your works from one wing to the other."
  , "POEM" `mkCard`
    Effect "After you complete a [Paper] work, you may _return_ it. If you do, complete the top card of the deck for free."
  , "SCROLL" `mkCard`
    Effect "+3 Points"
  , "SKETCH" `mkCard`
    Effect "Instead of choosing a new task from your hand, you may move one of your helpers to become your new task."
  , "STRAW" `mkCard`
    Effect "For [Smith] actions, you require one less support to complete a [Cloth] or [Clay] work."
  ])
  ++
  (map (\c -> c{_material=Stone}) [ -- Stone Cards
    "AMULET" `mkCard`
    Effect "After you complete a work, you may sell a material from your craft bench."
  , "BENCH" `mkCard`
    Effect "+2 points for each of your [Stone] works."
  , "DAIDORO" `mkCard`
    Effect "In the morning, you may restock the floor from the top of the deck untill there are three cards on the floor."
  , "FOUNTAIN" `mkCard`
    Effect "Before a [Clerk] task, you may reveal [Monk] cards from your hand. Each one counts as a [Clerk] helper during the task."
  , "FROG" `mkCard`
    Effect "After you complete this, if no opponent has fewer works than you, take an extra turn after this one."
  , "GO SET" `mkCard`
    Effect "All your [Stone] works count as being in both wings at the same time."
  , "PILLAR" `mkCard`
    Effect "All sales of your most sold resource type are considered covered. (Choose one type if tied.)"
  , "STATUE" `mkCard`
    Effect "After you complete this, transfer two materials from the floor to your craft bench."
  , "STOOL" `mkCard`
    Effect "After you complete a [Stone], [Clay] or [Metal] work, you may draw a card to your waiting area."
  , "TABLET" `mkCard`
    Effect "After you complete this either _return_ all cards on the floor, or restock the floor from the deck until it has both a [Stone] and a [Metal]."
  , "TOWER" `mkCard`
    Effect "Opponents skip your [Clerk], [Monk] or [Potter] tasks unless they reveal a matching card from their hand."
  ])
  ++
  (map (\c -> c{_material=Cloth}) [ -- Cloth Cards
    "CLOAK" `mkCard`
    Effect "After you complete a work, you may _return_ it. If you do, comuplete a [Metal] work from your hand for free."
  , "FLAG" `mkCard`
    Effect "Before using your task, you may reveal a matching card in hand to gain one extra action of that task."
  , "HANDKERCHIEF" `mkCard`
    Effect "Instead of checking the hand limit in the morning, you may discard one card from your hand to the floor."
  , "KITE" `mkCard`
    Effect "In the morning, you may transfer a card from your hand to any craft bench. If you do, treat Kite as an exact copy of one of that players's works until the end of your turn."
  , "MASK" `mkCard`
    Effect "Opponents cannot convert actions from your task into [Prayer] or [Craft] actions."
  , "PUPPET" `mkCard`
    Effect "Before your [Tailor] action, you may _return_ this to trade your hand with an oponent. Place thair hand in your waiting area."
  , "QUILT" `mkCard`
    Effect "Your [Paper], [Stone] and [Cloth] sales are covered. Your [Paper], [Stone] and [Cloth] cards in hand count as backorders if you ar ahead _or tied_ for sales of their type."
  , "ROBE" `mkCard`
    Effect "For a [Clerk] action, you may sell all materials of one type from your craft bench."
  , "SOCKS" `mkCard`
    Effect "For a [Potter] action, you may collect the top card of the dack instead of a card from the floor."
  , "TAPESTRY" `mkCard`
    Effect "+1 point for each work in this wing."
  , "UMBRELLA" `mkCard`
    Effect "In the morning, add a card from the top of the dech to the floor. If you do, you may convert a matichng helper into a sale."
  ])
  ++
  (map (\c -> c{_material=Clay}) [ -- Clay Cards
  ])
  ++
  (map (\c -> c{_material=Metal}) [ -- Metal Cards
  ])
