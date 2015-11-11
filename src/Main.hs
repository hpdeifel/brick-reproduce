{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Brick
import           Brick.Widgets.List
import           Control.Lens ((&), (.~))
import           Control.Monad (void)
import qualified Data.Vector as V
import           Graphics.Vty

type AppState = List String

event :: AppState -> Event -> EventM (Next AppState)
event l e = case e of
  EvKey (KChar 'q') [] -> halt l
  EvKey _ _ -> continue $ l & listElementsL .~ V.empty
                            & listSelectedL .~ Nothing
  _ -> continue l

main :: IO ()
main = do

  let app = App { appDraw = \l -> [renderList l (const str)]
                , appChooseCursor = showFirstCursor
                , appHandleEvent = event
                , appAttrMap = const (attrMap defAttr [])
                , appLiftVtyEvent = id
                , appStartEvent = return
                }

      initList = list "list" (V.fromList [show x | x <- [1..500]]) 1
                   & listSelectedL .~ Just 400


  void $ defaultMain app initList
