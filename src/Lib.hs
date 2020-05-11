module Lib where

import           Data.Char                      ( toLower )
import           Data.List
import           Data.Maybe                     ( isJust )

data NodeInfo = NodeInfo { cost :: Cost
                         , nodeInfoName :: NodeName
                         } deriving (Show)

data Tree = Tree_TypeA NodeInfo String [Tree]
          | Tree_TypeB TypeB
          deriving (Show)

newtype NodeName = NodeName String deriving (Eq, Ord, Show)

newtype Cost = Cost Float deriving (Show)

data TypeB = TypeB Cost NodeName [TypeB] deriving (Show)

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa t1 t2 =
  filterBepa $ commonNodeNames (getNodeNames t1) (getNodeNames t2)

filterBepa :: [NodeName] -> [NodeName]
filterBepa = filter (not . isBepa)

getNodeNames :: Tree -> [NodeName]
getNodeNames (Tree_TypeA (NodeInfo _ nodeName) _ []) = [nodeName]
getNodeNames (Tree_TypeA (NodeInfo _ nodeName) _ trees) =
  nodeName : getNodeNamesFromTreeList trees
getNodeNames (Tree_TypeB typeB) = getNodeNamesTypeB typeB

getNodeNamesFromTreeList :: [Tree] -> [NodeName]
getNodeNamesFromTreeList = concatMap getNodeNames

getNodeNamesTypeB :: TypeB -> [NodeName]
getNodeNamesTypeB (TypeB _ nodeName []) = [nodeName]
getNodeNamesTypeB (TypeB _ nodeName typeBList) =
  nodeName : getNodeNamesFromTypeBList typeBList

getNodeNamesFromTypeBList :: [TypeB] -> [NodeName]
getNodeNamesFromTypeBList = concatMap getNodeNamesTypeB

isBepa :: NodeName -> Bool
isBepa (NodeName nodeName) =
  isJust $ findSubString "bepa" (toLower <$> nodeName)

findSubString :: String -> String -> Maybe Int
findSubString sub string = findIndex (isPrefixOf sub) (tails string)

-- If the list xs contains duplicate values the resulting list will have 
-- duplicate values as well. This might not be the expected behaviour of 
-- `commonNodeNames`. Therefore, I introduced `nub` to ensure unique 
-- values.
commonNodeNames :: [NodeName] -> [NodeName] -> [NodeName]
commonNodeNames xs ys = nub $ commonNodeNames' xs ys
commonNodeNames' _  [] = []
commonNodeNames' [] _  = []
commonNodeNames' (x : xs) ys =
  if x `elem` ys then x : commonNodeNames xs ys else commonNodeNames xs ys
