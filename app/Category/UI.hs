{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Category.UI where

import Category.State
import Data.Zipper

import qualified Data.Text    as T
import qualified Graphics.Vty as V

import Brick              (Widget, str, vBox, (<+>))
import Brick.Forms        (formState, handleFormEvent, renderForm)
import Brick.Main         (continue, halt)
import Brick.Types        (BrickEvent (VtyEvent), EventM, Next)
import Brick.Widgets.Core (withAttr)
import Lens.Micro


spaces :: Int -> String
spaces n  = [' ' | _ <- [1.. 2 * n]]


renderString :: Bool -> String -> Widget ()
renderString selected s =
  if selected
  then withAttr "highlighted" $ str s
  else str s


-- Renders the focused subtree. Only the root node of the tree is rendered since we want the tree to
-- collapse as we walk back up towards the root
renderTree :: Int -> Bool -> Tree Category -> Widget ()
renderTree lvl selected t = renderString selected line
  where symbol = case t ^. children of
                   [] -> "\x25e6 " -- bullet point
                   _  -> "\x25b7 " -- right pointing triangle
        line   = spaces lvl ++ symbol ++ T.unpack (t ^. label . name)


-- Render a context in two parts - before and after the focused subtree
renderContext :: Int -> Maybe (Context Category) -> (Widget (), Widget ())
renderContext lvl c = case c of
  Nothing  -> (vBox [], vBox [])
  Just ctx -> ( vBox $ before : focusedLine : rls
              , vBox $ rrs ++ [after]
              )
    where rls             = map (renderTree lvl False) (reverse (ctx ^. lSiblings))
          rrs             = map (renderTree lvl False) (ctx ^. rSiblings)
          (before, after) = renderContext (lvl - 1) (ctx ^. pContext)
          focusedLine     = str $ spaces (lvl - 1)
                                 ++ "\x25bd " -- down pointing triangle
                                 ++ T.unpack (ctx ^. pLabel . name)


-- Renders the Category selector/editor
renderCategories :: CategoryState e -> [Widget ()]
renderCategories st = case st ^. categories . context of
  Nothing  -> [renderTree 0 True (st ^. categories . focus)]
  Just ctx -> [vBox [before, current, after]]
    where loc             = st ^. categories
          lvl             = getLevel (Just ctx)
          (before, after) = renderContext lvl (Just ctx)
          current         = if st ^. editing
                            then str (spaces lvl ++ "- ") <+> renderForm (st ^. editor)
                            else renderTree lvl True (loc ^. focus)


handleEditorEvent :: CategoryState e
  -> BrickEvent () e -> EventM () (Next (CategoryState e))
handleEditorEvent st evt =
  case evt of
      (VtyEvent (V.EvKey V.KEsc []))   -> continue $ st & editing .~ False
                                                        & categories %~ deleteTree
      (VtyEvent (V.EvKey V.KEnter [])) -> continue st'
        where st' = st & editing .~ False
                       & categories . focus . label . name
                         .~ formState (st ^. editor) ^. categoryFieldL
                       & editor .~ mkCategoryForm ""
      _ -> do
        editor' <- handleFormEvent evt $ st ^. editor
        continue $ (editor .~ editor') st


handleCategoryEvent :: CategoryState e
  -> BrickEvent () e
  -> EventM () (Next (CategoryState e))
handleCategoryEvent st evt =
  if st ^. editing
  then handleEditorEvent st evt
  else
    case evt of
      (VtyEvent (V.EvKey (V.KChar 'q') [])) -> halt st
      (VtyEvent (V.EvKey (V.KChar 'e') [])) -> continue $ st & editing .~ True
      (VtyEvent (V.EvKey V.KEsc []))        -> halt st
      (VtyEvent (V.EvKey V.KUp []))         -> continue $ st & categories %~ goLeft
      (VtyEvent (V.EvKey V.KDown  []))      -> continue $ st & categories %~ goRight
      (VtyEvent (V.EvKey V.KRight []))      -> continue $ st & categories %~ goDown
      (VtyEvent (V.EvKey V.KLeft []))       -> continue $ st & categories %~ goUp
      (VtyEvent (V.EvKey V.KDel []))        -> continue $ st & categories %~ deleteTree
      (VtyEvent (V.EvKey V.KEnter []))
        -> continue $ st & categories %~ addChild (Category 1 "")
                         & editing .~ True
      _ -> continue st
