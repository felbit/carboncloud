module Lib where

import           Data.List
import           Data.Maybe

data NodeInfo = NodeInfo { cost :: Cost
                         , nodeInfoName :: NodeName
                         }

data Tree = TreeTypeA NodeInfo String [Tree]
          | TreeTypeB TypeB

newtype NodeName = NodeName String deriving (Eq)

newtype Cost = Cost Float

data TypeB = TypeB Cost NodeName [TypeB]

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa t1 t2 = filterBepa
  $ commonNodeNames (getNodeNames t1) (getNodeNames t2)
  where filterBepa = filter (not . isBepa)

getNodeNames :: Tree -> [NodeName]
getNodeNames (TreeTypeA (NodeInfo _ nodeName) _ []) = [nodeName]
getNodeNames (TreeTypeA (NodeInfo _ nodeName) _ [tree]) =
  nodeName : getNodeNames tree
getNodeNames (TreeTypeB typeB) = getNodeNamesTypeB typeB

getNodeNamesTypeB :: TypeB -> [NodeName]
getNodeNamesTypeB (TypeB _ nodeName []) = [nodeName]
getNodeNamesTypeB (TypeB _ nodeName [typeB]) =
  nodeName : getNodeNamesTypeB typeB

isBepa :: NodeName -> Bool
isBepa (NodeName nodeName) = isJust $ findSubString "Bepa" nodeName

findSubString :: (Eq a) => [a] -> [a] -> Maybe Int
findSubString sub string = findIndex (isPrefixOf sub) (tails string)

commonNodeNames :: Eq a => [a] -> [a] -> [a]
commonNodeNames _  [] = []
commonNodeNames [] _  = []
commonNodeNames (x : xs) ys =
  if x `elem` ys then x : commonNodeNames xs ys else commonNodeNames xs ys
