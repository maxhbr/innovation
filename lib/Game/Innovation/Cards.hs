{-# LANGUAGE OverloadedStrings #-}
module Game.Innovation.Cards
    ( getDeck
    , getCards
    , getCardsById, getCardById
    ) where

import Data.Map (Map)
import Control.Monad
import qualified Data.Map as Map

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Rules
import           Control.Monad.Trans
import qualified Control.Monad.Trans.Writer as W
import qualified Control.Monad.Trans.Reader as R

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

card :: Card
card = Card "" minBound minBound (Productions None None None None) EDogmaChain

toAge :: Age -> [Card] -> [Card]
toAge a = map (\c -> c{ _age = a})

--------------------------------------------------------------------------------
-- Age1 Cards
--------------------------------------------------------------------------------

age1Cards = toAge Age1
  [ card { _title = "Agriculture"
         , _color = Yellow
         , _productions = Productions None (Produce Tree) (Produce Tree) (Produce Tree)
         , _dogmas = dogmasFromList
           [ Tree `Dogma` "You may recycle a card from your Hand. If you dou, draw and score a card of value one higher than the card you recycled." $
             skip
           ]
         }
  , card { _title = "Archery"
         , _color = Red
         , _productions = Productions (Produce Castle) (Produce Bulb) None (Produce Castle)
         , _dogmas = dogmasFromList
           [ Tree `IDemand` "I demand you draw a [1]! Then, transfer the highest card from your Hand to my Hand!" $
             (\issuingUser -> do
                 drawOfAnd Age1 >>= putIntoHand
                 skip
             )
           ]
         }
  , card { _title = "City-States"
         , _color = Purple
         , _productions = Productions None (Produce Crown) (Produce Crown) (Produce Castle)
         , _dogmas = dogmasFromList
           [ Tree `IDemand` "I demand that if you have four (Castle) or more, you transfer an Active card that provides (Castle) to my Zone! If you do, draw a [1]!" $
             (\issuingUser -> do
                 numCastles <- mkA (getProductionsForSymbolOf Castle)
                 when (numCastles >= 4)
                   (do
                       skip
                       drawOfAnd Age1 >>= putIntoHand
                   )
             )
           ]
         }
  , card { _title = "Clothing"
         , _color = Green
         , _productions = Productions None (Produce Crown) (Produce Tree) (Produce Tree)
         , _dogmas = dogmasFromList
           [ Tree `Dogma` "Put into play a card of a different color than any card in your Zone." $ do
             playedColors <- mkA getPlayedColorsOf
             hand <- mkA getHandOf
             let handCardsNotOfPlayedColor = [card | card <- getRawStack hand
                                                   , _color card `notElem` playedColors]
             chosenCard <- mkA (\uid -> (uid `chooseOneOf` "your hand, which does not match any card in your zone") handCardsNotOfPlayedColor)
             skip
           , Tree `Dogma` "Draw and score a [1] for each color in your Zone and in no other player's." $ do
             playedColors <- mkA getPlayedColorsOf
             drawNAnd (length playedColors) >>= score
           ]
         }
  , card { _title = "Code of Laws" -- from FAQ
         , _color = Purple
         , _productions = Productions None (Produce Crown) (Produce Crown) (Produce Tree)
         , _dogmas = dogmasFromList
           [ Crown `Dogma` "You may tuck a card from your hand of the same color as any Card on your board. If you do you may splay that color of your cards left." $
             skip
           ]
         }
  , card { _title = "Domestication" -- from FAQ
         , _color = Yellow
         , _productions = Productions (Produce Castle) (Produce Crown) None (Produce Castle)
         , _dogmas = dogmasFromList
           [ Castle `Dogma` "Put Into Play the lowet card in your hand. Draw a [1]." $
             skip
           ]
         }
  , card { _title = "Masonry"
         , _color = Yellow
         , _productions = Productions (Produce Castle) None (Produce Castle) (Produce Castle)
         , _dogmas = dogmasFromList
           [ Tree `Dogma` "You may put into play any number of cards that provide (Castle) from your Hand. If you put into play four or more cards, dominate the TECHNOLOGY Domain." $
             skip
           ]
         }
  , card { _title = "Metalworking" -- from FAQ
         , _color = Red
         , _productions = Productions (Produce Castle) (Produce Castle) None (Produce Castle)
         , _dogmas = dogmasFromList
           [ Castle `Dogma` "Draw and reveal a [1]. If it has a [Castle], score it and repeat this dogma effect. Otherwise, keep it" $ let
                dogmaFun = do
                  card <- drawOfAnd Age1
                  if (card `providesSymbol` Castle)
                    then do
                    score card
                    dogmaFun
                    else putIntoHand card
               in dogmaFun
           ]
         }
  , card { _title = "Mysticism" -- from FAQ
         , _color = Purple
         , _productions = Productions None (Produce Castle) (Produce Castle) (Produce Castle)
         , _dogmas = dogmasFromList
           [ Castle `Dogma` "Draw a 1. If it is the same color as any card on your board, put into play it and draw a [1]." $
             skip
           ]
         }
  , card { _title = "Oars" -- from FAQ
         , _color = Red
         , _productions = Productions (Produce Castle) (Produce Crown) None (Produce Castle)
         , _dogmas =
           (Castle `GenIDemand` "I demand you transfer a card with a [Crown] from your hand to my score pile! If you do, draw a [1]." $
            const $
            lift (W.tell []))
           `DogmaChain`
           (( Castle `GenDogma` "If no cards were transferred due to this demand, draw a [1]." $
              lift (W.tell []))
            `DogmaChain` EDogmaChain)
         }
  , card { _title = "Pottery" -- from FAQ
         , _color = Blue
         , _productions = Productions None (Produce Tree) (Produce Tree) (Produce Tree)
         , _dogmas = dogmasFromList
           [ Tree `Dogma` "You may return up to three cards from your hand. If you returned any cards, draw and score a card of value equal to the number of cards you returned." $
             skip
           , Tree `Dogma` "Draw a [1]" $
             skip
           ]
         }
  , card { _title = "Sailing" -- from FAQ
         , _color = Green
         , _productions = Productions (Produce Crown) (Produce Crown) None (Produce Tree)
         , _dogmas = dogmasFromList
           [ Crown `Dogma` "Draw and put into play a [1]." $
             drawOfAnd Age1 >>= putIntoPlay
           ]
         }
  , card { _title = "The Wheel" -- from FAQ
         , _color = Green
         , _productions = Productions None (Produce Castle) (Produce Castle) (Produce Castle)
         , _dogmas = dogmasFromList
           [ Castle `Dogma` "Draw two [1]." $
             drawNOfAnd 2 Age1 >>= putIntoHand
           ]
         }
  , card { _title = "Tools" -- from FAQ
         , _color = Blue
         , _productions = Productions None (Produce Bulb) (Produce Bulb) (Produce Castle)
         , _dogmas = dogmasFromList
           [ Bulb `Dogma` "You may return three cards from your hand. If you do, draw and put into play a 3." $
             skip
           , Bulb `Dogma` "You may return a [3] from your hand, if you do, draw three [1]." $
             skip
           ]
         }
  , card { _title = "Writing" -- from FAQ
         , _color = Blue
         , _productions = Productions None (Produce Bulb) (Produce Bulb) (Produce Crown)
         , _dogmas = dogmasFromList
           [ Bulb `Dogma` "Draw a 2." $
             drawOfAnd Age2 >>= putIntoPlay
           ]
         }
  ]

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
       , _dogmas = dogmasFromList
         [ Tree `IDemand` "I demand you transfer a card that provides (Castle) from you Hand to my Influence!" $
           const $
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
       , _dogmas = dogmasFromList
         [ Tree `IDemand` "I demand you transfer two cards from your Influence to my Influence!" $
           const $
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
       , _dogmas = dogmasFromList
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
       , _dogmas = dogmasFromList
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
       , _dogmas = dogmasFromList
         [ Clock `IDemand` "I demand you recycle half your influence cards (rounded up)" $
           const $
           skip
         ]
       }

domotics =
  Card { _title = "Domotics"
       , _color = Green
       , _age = Age10
       , _productions = Productions None (Produce Crown) (Produce Crown) (Produce Crown)
       , _dogmas = dogmasFromList
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
       , _dogmas = dogmasFromList
         [ Factory `IDemand` "I demand you recycle one of your Active cards that provides (Tree)" $
           const $
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
       , _dogmas = dogmasFromList
         [ Bulb `Dogma` "You may recycle a card from your Hand. If you have recycled a [10] draw a [10] for each different value in your Influence." $
           skip
         ]
       }

age10Cards = [artificialAntelligence, bioengineering, databases, domotics, globalization, miniaturization]
