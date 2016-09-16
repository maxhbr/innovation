module Game.Innovation.Cards
    ( cards
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Game.Innovation.Types

cards :: Map Age Stack
cards = Map.fromList
        [(Age1,[agriculture])
        ,(Age2,[])
        ,(Age3,[])
        ,(Age4,[])
        ,(Age5,[])
        ,(Age6,[])
        ,(Age7,[])
        ,(Age8,[])
        ,(Age9,[])
        ,(Age10,[])]

--------------------------------------------------------------------------------
-- Age1 Cards
--------------------------------------------------------------------------------

agriculture =
  Card { _color       = Yellow
       , _age         = Age1
       , _productions = Productions None (Produce Tree) (Produce Tree) (Produce Tree)
       , _dogmas      =
         [Tree `Dogma` (RawDescription "You may recycle a card from your Hand. If you dou, draw and score a card of value one higher than the card you recycled.")]
       }

masonry =
  Card { _color       = Yellow
       , _age         = Age1
       , _productions = Productions (Produce Castle) None (Produce Castle) (Produce Castle)
       , _dogmas      =
         [Tree `Dogma` (RawDescription "You may put into play any number of cards that provide (Castle) from your Hand. If you put into play four or more cards, dominate the TECHNOLOGY Domain.")]
       }

--------------------------------------------------------------------------------
-- Age2 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age3 Cards
--------------------------------------------------------------------------------

feudalism =
  Card { _color       = Purple
       , _age         = Age3
       , _productions = Productions None (Produce Castle) (Produce Tree) (Produce Castle)
       , _dogmas      =
         [Tree `IDemand` (RawDescription "I demand you transfer a card that provides (Castle) from you Hand to my Influence!")
             , `Dogma`   (RawDescription "You may splay your yellow or purple cards left.")]
       }

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
  Card { _color       = Red
       , _age         = Age7
       , _productions = Productions (Produce Crown) (Produce Crown) (Produce Factory) None
       , _dogmas      =
         [Tree `IDemand` (RawDescription "I demand you transfer two cards from your Influence to my Influence!")]
       }


--------------------------------------------------------------------------------
-- Age8 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age9 Cards
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Age10 Cards
--------------------------------------------------------------------------------

bioengineering =
  Card { _color       = Blue
       , _age         = Age10
       , _productions = Productions (Produce Bulb) (Produce Clock) (Produce Clock) None
       , _dogmas      =
         [Tree `Dogma` (RawDescription "Transfer an Active card that provides (Tree) from any other player's Zone to your Influence.")
             , `Dogma` (RawDescription "If a player has fewer than three (Tree), the single player with the most (Tree) wins.")]
       }
