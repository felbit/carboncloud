module Lib where

import           Data.List
import           Data.Maybe

data NodeInfo = NodeInfo
  { cost :: Cost
  , nodeInfoName :: NodeName
  }

data Tree
  = Tree_TypeA NodeInfo
               String
               [Tree]
  | Tree_TypeB TypeB

newtype NodeName =
  NodeName String
  deriving (Eq)

newtype Cost =
  Cost Float

data TypeB =
  TypeB Cost
        NodeName
        [TypeB]

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa t1 t2 =
  filterBepa
    $  getCommonNodeNamesExceptBepa' t1
    ++ getCommonNodeNamesExceptBepa' t2
  where filterBepa = filter (not . isBepa)

getCommonNodeNamesExceptBepa' :: Tree -> [NodeName]
getCommonNodeNamesExceptBepa' (Tree_TypeA (NodeInfo _ nodeName) _ []) =
  [nodeName]
getCommonNodeNamesExceptBepa' (Tree_TypeA (NodeInfo _ nodeName) _ [tree]) =
  nodeName : getCommonNodeNamesExceptBepa' tree
getCommonNodeNamesExceptBepa' (Tree_TypeB typeB) =
  getCommonNodeNamesExceptBepaFromTypeB typeB

getCommonNodeNamesExceptBepaFromTypeB :: TypeB -> [NodeName]
getCommonNodeNamesExceptBepaFromTypeB (TypeB _ nodeName []) = [nodeName]
getCommonNodeNamesExceptBepaFromTypeB (TypeB _ nodeName [typeB]) =
  nodeName : getCommonNodeNamesExceptBepaFromTypeB typeB

isBepa :: NodeName -> Bool
isBepa (NodeName nodeName) = isJust $ findSubString "Bepa" nodeName

findSubString :: (Eq a) => [a] -> [a] -> Maybe Int
findSubString sub string = findIndex (isPrefixOf sub) (tails string)
