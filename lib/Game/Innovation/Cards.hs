module Game.Innovation.Cards
    ( DeckName
    , getDeck
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Game.Innovation.Types

type DeckName = String

getDeck :: DeckName -> Map Age Stack
getDeck "base" = Map.fromList
                 [(Age1,[agriculture,masonry])
                 ,(Age2,[])
                 ,(Age3,[feudalism])
                 ,(Age4,[])
                 ,(Age5,[])
                 ,(Age6,[])
                 ,(Age7,[combustion])
                 ,(Age8,[])
                 ,(Age9,[])
                 ,(Age10,age10Cards)]

--------------------------------------------------------------------------------
-- Age1 Cards
--------------------------------------------------------------------------------

agriculture =
  Card { _title       = "Agriculture"
       , _color       = Yellow
       , _age         = Age1
       , _productions = Productions None (Produce Tree) (Produce Tree) (Produce Tree)
       , _dogmas      =
         [Tree `Dogma` RawDescription "You may recycle a card from your Hand. If you dou, draw and score a card of value one higher than the card you recycled."]}

masonry =
  Card { _title       = "Masonry"
       , _color       = Yellow
       , _age         = Age1
       , _productions = Productions (Produce Castle) None (Produce Castle) (Produce Castle)
       , _dogmas      =
         [Tree `Dogma` RawDescription "You may put into play any number of cards that provide (Castle) from your Hand. If you put into play four or more cards, dominate the TECHNOLOGY Domain."]}

--------------------------------------------------------------------------------
-- Age2 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age3 Cards
--------------------------------------------------------------------------------

feudalism =
  Card { _title       = "Feudalism"
       , _color       = Purple
       , _age         = Age3
       , _productions = Productions None (Produce Castle) (Produce Tree) (Produce Castle)
       , _dogmas      =
         [Tree `IDemand` RawDescription "I demand you transfer a card that provides (Castle) from you Hand to my Influence!"
         ,Tree `Dogma`   RawDescription "You may splay your yellow or purple cards left."]}

--------------------------------------------------------------------------------
-- Age4 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age5 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age6 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age7 Cards
--------------------------------------------------------------------------------

combustion =
  Card { _title       = "Combustion"
       , _color       = Red
       , _age         = Age7
       , _productions = Productions (Produce Crown) (Produce Crown) (Produce Factory) None
       , _dogmas      =
         [Tree `IDemand` RawDescription "I demand you transfer two cards from your Influence to my Influence!"]}

--------------------------------------------------------------------------------
-- Age8 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age9 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age10 Cards
--------------------------------------------------------------------------------

artificialAntelligence =
  Card { _title = "Artificial Intelligence"
       , _color = Purple
       , _age = Age10
       , _productions = Productions (Produce Bulb) (Produce Bulb) (Produce Clock) None
       , _dogmas =
         [Bulb `Dogma` RawDescription "Draw and score a [10]"
         ,Bulb `Dogma` RawDescription "If both [Robotics] and [Software] are active cards in any zone(s), the single player with the lowes influence wins"]}

bioengineering =
  Card { _title       = "Bioengineering"
       , _color       = Blue
       , _age         = Age10
       , _productions = Productions (Produce Bulb) (Produce Clock) (Produce Clock) None
       , _dogmas      =
         [Tree `Dogma` RawDescription "Transfer an Active card that provides (Tree) from any other player's Zone to your Influence."
         ,Tree `Dogma` RawDescription "If a player has fewer than three (Tree), the single player with the most (Tree) wins."]}

databases =
  Card { _title = "Databases"
       , _color = Green
       , _age = Age10
       , _productions = Productions None (Produce Clock) (Produce Clock) (Produce Clock)
       , _dogmas =
         [Clock `IDemand` RawDescription "you recycle half your influence cards (rounded up)"]
       }

domotics =
  Card { _title = "Domotics"
       , _color = Green
       , _age = Age10
       , _productions = Productions None (Produce Crown) (Produce Crown) (Produce Crown)
       , _dogmas =
         [Crown `Dogma` RawDescription "Activate the cooperative Dogmas of another of your Active cards for yourself only"
         ,Crown `Dogma` RawDescription "If youn have more Dominations than any other player, you win."]}

globalization =
  Card { _title = "Globalization"
       , _color = Yellow
       , _age = Age10
       , _productions = Productions None (Produce Factory) (Produce Factory) (Produce Factory)
       , _dogmas =
         [Factory `IDemand` RawDescription "you recycle one of your Active cards thet provides (Tree)"
         ,Factory `Dogma` RawDescription "Draw and score a [6]. If there is no player with more (Tree) then (Factory), the single player with the most Influence points wins."]}

miniaturization =
  Card { _title = "Miniaturization"
       , _color = Red
       , _age = Age10
       , _productions = Productions None (Produce Bulb) (Produce Clock) (Produce Bulb)
       , _dogmas =
         [Bulb `Dogma` RawDescription "You may recycle a card from your Hand. If you have recycled a [10] draw a [10] for each different value in your Influence."]}

age10Cards = [artificialAntelligence, bioengineering, databases, domotics, globalization, miniaturization]
