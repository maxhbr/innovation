{-# LANGUAGE ExistentialQuantification #-}
module Game.MetaGame.Play
       ( PlayResult
       , play
       ) where

import Game.MetaGame.Game

type SuccessPlayResult = ( InnerMoveResult board board -- ^ the result of the input Game
                         , Game -- ^ the evaluated "prefix" of the input Game
                         )
type FailPlayResult = ( SuccessPlayResult
                      , History -- ^ the unevaluated part of the input Game
                      )
type PlayResult = Either FailPlayResult SuccessPlayResult

play :: Game -> PlayResult
play g = undefined
