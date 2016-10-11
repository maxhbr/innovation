module Game.Innovation.Cards
    ( getDeck
    , getCards
    , getCardsById, getCardById
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Game.Innovation.Types
import Game.Innovation.Actions.Basic

getDeck :: Map Age DrawStack
getDeck = Map.fromList
          [ (Age1, DrawStack age1Cards)
          , (Age2, DrawStack age2Cards)
          , (Age3, DrawStack age3Cards)
          , (Age4, DrawStack age4Cards)
          , (Age5, DrawStack age5Cards)
          , (Age6, DrawStack age6Cards)
          , (Age7, DrawStack age7Cards)
          , (Age8, DrawStack age8Cards)
          , (Age9, DrawStack age9Cards)
          , (Age10, DrawStack age10Cards) ]

getCards :: RawStack
getCards = age1Cards ++ age2Cards ++ age3Cards ++ age4Cards ++ age5Cards ++ age6Cards ++ age7Cards ++ age8Cards ++ age9Cards ++ age10Cards

getCardsById :: Map CardId Card
getCardsById = Map.fromList $ map (\c -> (getCId c, c)) getCards

getCardById :: CardId -> Maybe Card
getCardById cid = Map.lookup cid getCardsById

--------------------------------------------------------------------------------
-- Age1 Cards
--------------------------------------------------------------------------------

agriculture =
  Card { _title = "Agriculture"
       , _color = Yellow
       , _age = Age1
       , _productions = Productions None (Produce Tree) (Produce Tree) (Produce Tree)
       , _dogmas =
         [ Tree `Dogma` "You may recycle a card from your Hand. If you dou, draw and score a card of value one higher than the card you recycled." $
           skip
         ]
       }

masonry =
  Card { _title = "Masonry"
       , _color = Yellow
       , _age = Age1
       , _productions = Productions (Produce Castle) None (Produce Castle) (Produce Castle)
       , _dogmas =
         [ Tree `Dogma` "You may put into play any number of cards that provide (Castle) from your Hand. If you put into play four or more cards, dominate the TECHNOLOGY Domain." $
           skip
         ]
       }

sailing = -- from FAQ
  Card { _title = "Sailing"
       , _color = Green
       , _age = Age1
       , _productions = Productions (Produce Crown) (Produce Crown) None (Produce Tree)
       , _dogmas =
         [ Crown `Dogma` "Draw and play a [1]" $
           drawOfAnd Age1 >>= putIntoPlay
         ]
       }

theWheel = -- from FAQ
  Card { _title = "The Wheel"
       , _color = Green
       , _age = Age1
       , _productions = Productions None (Produce Castle) (Produce Castle) (Produce Castle)
       , _dogmas =
         [ Castle `Dogma` "Draw two [1]" $
           drawNOfAnd 2 Age1 >>= putIntoHand
         ]
       }

age1Cards = [agriculture,masonry,sailing,theWheel]

--------------------------------------------------------------------------------
-- Age2 Cards
--------------------------------------------------------------------------------

age2Cards = []

--------------------------------------------------------------------------------
-- Age3 Cards
--------------------------------------------------------------------------------

feudalism =
  Card { _title = "Feudalism"
       , _color = Purple
       , _age = Age3
       , _productions = Productions None (Produce Castle) (Produce Tree) (Produce Castle)
       , _dogmas =
         [ Tree `IDemand` "I demand you transfer a card that provides (Castle) from you Hand to my Influence!" $
           skip
         , Tree `Dogma` "You may splay your yellow or purple cards left." $
           skip
         ]
       }

age3Cards = [feudalism]

--------------------------------------------------------------------------------
-- Age4 Cards
--------------------------------------------------------------------------------

age4Cards = []

--------------------------------------------------------------------------------
-- Age5 Cards
--------------------------------------------------------------------------------

age5Cards = []

--------------------------------------------------------------------------------
-- Age6 Cards
--------------------------------------------------------------------------------

age6Cards = []

--------------------------------------------------------------------------------
-- Age7 Cards
--------------------------------------------------------------------------------

combustion =
  Card { _title = "Combustion"
       , _color = Red
       , _age = Age7
       , _productions = Productions (Produce Crown) (Produce Crown) (Produce Factory) None
       , _dogmas =
         [ Tree `IDemand` "I demand you transfer two cards from your Influence to my Influence!" $
           skip
         ]
       }

age7Cards = [combustion]
--------------------------------------------------------------------------------
-- Age8 Cards
--------------------------------------------------------------------------------

age8Cards = []

--------------------------------------------------------------------------------
-- Age9 Cards
--------------------------------------------------------------------------------

age9Cards = []

--------------------------------------------------------------------------------
-- Age10 Cards
--------------------------------------------------------------------------------

artificialAntelligence =
  Card { _title = "Artificial Intelligence"
       , _color = Purple
       , _age = Age10
       , _productions = Productions (Produce Bulb) (Produce Bulb) (Produce Clock) None
       , _dogmas =
         [ Bulb `Dogma` "Draw and score a [10]" $
           drawOfAnd Age10 >>= score
         , Bulb `Dogma` "If both [Robotics] and [Software] are active cards in any zone(s), the single player with the lowes influence wins" $
           skip
         ]
       }

bioengineering =
  Card { _title = "Bioengineering"
       , _color = Blue
       , _age = Age10
       , _productions = Productions (Produce Bulb) (Produce Clock) (Produce Clock) None
       , _dogmas =
         [ Tree `Dogma` "Transfer an Active card that provides (Tree) from any other player's Zone to your Influence." $
           skip
         , Tree `Dogma` "If a player has fewer than three (Tree), the single player with the most (Tree) wins." $
           skip
         ]
       }

databases =
  Card { _title = "Databases"
       , _color = Green
       , _age = Age10
       , _productions = Productions None (Produce Clock) (Produce Clock) (Produce Clock)
       , _dogmas =
         [ Clock `IDemand` "I demand you recycle half your influence cards (rounded up)" $
           skip
         ]
       }

domotics =
  Card { _title = "Domotics"
       , _color = Green
       , _age = Age10
       , _productions = Productions None (Produce Crown) (Produce Crown) (Produce Crown)
       , _dogmas =
         [ Crown `Dogma` "Activate the cooperative Dogmas of another of your Active cards for yourself only" $
           skip
         , Crown `Dogma` "If youn have more Dominations than any other player, you win." $
           skip
         ]
       }

globalization =
  Card { _title = "Globalization"
       , _color = Yellow
       , _age = Age10
       , _productions = Productions None (Produce Factory) (Produce Factory) (Produce Factory)
       , _dogmas =
         [ Factory `IDemand` "I demand you recycle one of your Active cards that provides (Tree)" $
           skip
         , Factory `Dogma` "Draw and score a [6]. If there is no player with more (Tree) then (Factory), the single player with the most Influence points wins." $ do
           drawOfAnd Age6 >>= score
           skip
         ]
       }

miniaturization =
  Card { _title = "Miniaturization"
       , _color = Red
       , _age = Age10
       , _productions = Productions None (Produce Bulb) (Produce Clock) (Produce Bulb)
       , _dogmas =
         [ Bulb `Dogma` "You may recycle a card from your Hand. If you have recycled a [10] draw a [10] for each different value in your Influence." $
           skip
         ]
       }

age10Cards = [artificialAntelligence, bioengineering, databases, domotics, globalization, miniaturization]
