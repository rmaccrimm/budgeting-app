{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module Category.State where

import Data.Zipper

import qualified Data.Text as T

import Brick.Forms   (Form, editTextField, newForm)
import Lens.Micro
import Lens.Micro.TH


data Category = Category
  { _identifier :: Int
  , _name       :: T.Text
  }
  deriving (Show)


newtype CategoryField = CategoryField T.Text
  deriving (Show)


data CategoryState e = CategoryState
  { _categories :: Location Category
  , _editing    :: Bool
  , _editor     :: Form CategoryField e ()
  }


makeLenses ''Category
makeLenses ''CategoryState


-- CategoryField lens needed for the Brick form API
categoryFieldL :: Lens' CategoryField T.Text
categoryFieldL = lens getter setter
  where getter = \(CategoryField t) -> t
        setter = \_ t -> CategoryField t


mkCategoryForm :: T.Text -> Form CategoryField e ()
mkCategoryForm s = newForm [editTextField categoryFieldL () (Just 1)] (CategoryField s)


mkCategoryState :: Location Category -> CategoryState e
mkCategoryState loc = CategoryState
  { _categories = loc
  , _editing = False
  , _editor = mkCategoryForm ""
  }
