{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Category.IO where

import Category.State
import Data.Zipper

import Data.Text              (pack)
import Database.SQLite.Simple
import Lens.Micro
import Lens.Micro.TH


data CategoryRow = CategoryRow
  { _category :: Category
  , _path     :: String
  }
  deriving (Show)
makeLenses ''CategoryRow


instance FromRow CategoryRow where
  fromRow = do
    rowId   <- field
    rowName <- field
    rowPath <- field
    return $ CategoryRow (Category rowId rowName) rowPath


-- Load a single category row by id
loadCategoryRow :: Connection -> Int -> IO [CategoryRow]
loadCategoryRow conn rowId = query conn select (Only rowId)
  where select = "SELECT id, name, path FROM category WHERE id = ?;"


loadCategoryTree :: Connection -> Int -> IO (Tree Category)
loadCategoryTree conn rootId = do
  [root]     <- loadCategoryRow conn rootId
  childRows  <- query conn select (Only rootId)
  childTrees <- mapM (loadCategoryTree conn) [c ^. category . identifier | c <- childRows]
  return $ Node (root ^. category) childTrees
  where select = "SELECT id, name, path FROM category WHERE path like '%/' || ?;"


loadRootLocation :: Connection -> IO (Location Category)
loadRootLocation conn = do
  [rootRow]  <- query_ conn "SELECT id, name, path FROM category WHERE path = '';"
  rootTree   <- loadCategoryTree conn (rootRow ^. category . identifier)
  return (Location rootTree Nothing)


-- Returns inserted category with the id field set
insertCategoryRow :: Connection -> String -> String -> IO Category
insertCategoryRow conn n p = do
  execute conn "INSERT INTO category (name, path) VALUES (?, ?);" (n, p)
  newId <- lastInsertRowId conn
  return $ Category (fromIntegral newId) (pack n)


-- Insert a new category as a child of the currently focused category and return a location with the
-- newly created category focused
newChildCategory :: Connection -> Location Category -> String -> IO (Location Category)
newChildCategory conn loc childName = do
    [parentRow] <- loadCategoryRow conn pId
    newCategory <- insertCategoryRow conn childName (parentRow ^. path ++ "/" ++ show pId)
    return (addChild newCategory loc)
      where  pId = loc ^. focus . label . identifier


-- Save the name of the currently focused category to db
updateCategoryName :: Connection -> Location Category -> IO ()
updateCategoryName conn loc = execute conn update (l ^. name, l ^. identifier)
  where l = loc ^. focus . label
        update = "UPDATE category SET name = ? WHERE id = ?;"


-- Delete the focused category and all it's child categories
deleteCategory :: Connection -> Location Category -> IO ()
deleteCategory = undefined
