{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Zipper

import Brick (App, AttrMap, BrickEvent (VtyEvent), Widget, attrMap, on, simpleMain, str, vBox,
              withBorderStyle, (<+>))
import Brick.Main
import Brick.Types (EventM, Next)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (withAttr)
import Brick.Widgets.List (GenericList (listSelected), handleListEvent, list, renderList)
import Graphics.Vty (Attr, black, cyan, white, yellow)
import qualified Graphics.Vty as V


data Category = Category { name :: String, expanded :: Bool, subcategories :: [Category] }
  deriving Show


emptyCategory :: String -> Category
emptyCategory name = Category { name, expanded = False, subcategories = [] }


parentCategory :: String -> [Category] -> Category
parentCategory name subcategories = Category { name, expanded = False, subcategories }



data CategoryTree = CategoryTree [Category] Int
  deriving Show


testList :: CategoryTree
testList = CategoryTree [ parentCategory "bills" [ emptyCategory "phone"
                                                 , emptyCategory "rent"
                                                 ]

                        , emptyCategory "toplevel"
                        , parentCategory "expenses" [ parentCategory "fun" [ emptyCategory "eating out"
                                                                           , emptyCategory "games"
                                                                           ]
                                                    , emptyCategory "necessities"
                                                    ]
                        ] 0


attrs :: AttrMap
attrs = attrMap (white `on` black ) [("highlighted", black `on` cyan)]


spaces :: Int -> String
spaces n  = [' ' | _ <- [1..n]]


flattenNames :: Int -> [Category] -> [String]
flattenNames _ [] = []
flattenNames lvl (c:cs) =
  let rest = flattenNames lvl cs
      pad = spaces (2 * lvl) in
    case subcategories c of
      []    -> (pad ++ "- " ++ name c) : rest
      subcs -> (pad ++ "> " ++ name c) : subnames ++ rest
        where subnames = if expanded c then flattenNames (lvl + 1) subcs else []


flattenList :: [Category] -> [String]
flattenList = flattenNames 0


renderListElement ::  Bool -> String -> Widget ()
renderListElement selected s = if selected
                               then withAttr "highlighted" $ str s
                               else str s


renderApp :: CategoryTree -> [Widget ()]
renderApp (CategoryTree cl selected) = [vBox lines]
  where visibleCategories = flattenList cl
        lines = [renderListElement (i == selected) s | (i, s) <- zip [0..] visibleCategories]

handleEvent :: CategoryTree -> BrickEvent () e -> EventM () (Next CategoryTree)
handleEvent c (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt c
handleEvent c (VtyEvent (V.EvKey V.KEsc []))        = halt c
handleEvent c _                                     = continue c


app :: App CategoryTree e ()
app =
    App { appDraw = renderApp
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const attrs
        , appChooseCursor = neverShowCursor
        }


main :: IO ()
main = defaultMain app testList >>= (\finalState -> return ())
