{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module UI.Categories
where

import Brick
    ( App
    , AttrMap
    , BrickEvent (VtyEvent)
    , Widget
    , attrMap
    , on
    , simpleMain
    , str
    , vBox
    , withBorderStyle
    , (<+>)
    )
import Brick.Forms                ( Form, editTextField, newForm, renderForm )
import Brick.Main
import Brick.Types                ( EventM, Next )
import Brick.Widgets.Border       ( borderWithLabel, vBorder )
import Brick.Widgets.Border.Style ( unicode )
import Brick.Widgets.Center       ( center )
import Brick.Widgets.Core         ( withAttr )
import Brick.Widgets.List         ( GenericList (listSelected), handleListEvent, list, renderList )
import Control.Lens               ( Lens', lens )
import Control.Lens.Operators
import Control.Lens.TH
import Graphics.Vty               ( Attr, black, cyan, white, yellow )

import qualified Data.Text    as T
import qualified Graphics.Vty as V

import Data.Zipper


data Category
  = Category { _id   :: Int
             , _name :: T.Text
             }
  deriving (Eq, Show)
makeLenses ''Category


newtype CategoryField
  = CategoryField { _txt :: T.Text }
  deriving (Show)
makeLenses ''CategoryField


data CategoryState e
  = CategoryState { _categories :: Location Category
                  , _editing    :: Bool
                  , _editor     :: Form CategoryField e ()
                  }
makeLenses ''CategoryState


initCategoryForm :: Form CategoryField e ()
initCategoryForm = newForm [editTextField txt () (Just 1)] $ CategoryField { _txt = "" }


mkCategoryState = CategoryState
  { _categories = Location { _focus = Node { _label = Category 0 "Categories"
                                           , _children = []
                                           }
                           , _context = Nothing
                           }
  , _editing = False
  , _editor = initCategoryForm
  }


spaces :: Int -> String
spaces n  = [' ' | _ <- [1..n]]


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
                   [] -> "\x25e6 " -- bullet 
                   _  -> "\x25b7 " -- right pointing triangle
        line   = spaces (2 * lvl) ++ symbol ++ T.unpack (t ^. label . name)


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
          focusedLine     = str $ spaces (2 * (lvl - 1))
                                 ++ "\x25bd " -- down pointing triangle
                                 ++ T.unpack (ctx ^. pLabel . name)


getLevel :: Maybe (Context Category) -> Int
getLevel Nothing    = 0
getLevel (Just ctx) = 1 + getLevel (ctx ^. pContext)


renderCategories :: CategoryState e -> [Widget ()]
renderCategories st = case st ^. categories . context of
  Nothing  -> [renderTree 0 True (st ^. categories . focus)]
  Just ctx -> [vBox [before, current, after]]
    where loc             = st ^. categories
          lvl             = getLevel (loc ^. context)
          (before, after) = renderContext lvl (loc ^. context)
          current         = if st ^. editing
                            then str (spaces (2 * lvl) ++ "- ") <+> renderForm (st ^. editor)
                            else renderTree lvl True (loc ^. focus)

