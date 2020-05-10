module Lib where

import           Data.List
import           Data.Maybe

data NodeInfo = NodeInfo { cost :: Cost
                         , nodeInfoName :: NodeName
                         }

data Tree = TreeTypeA NodeInfo String [Tree]
          | TreeTypeB TypeB

newtype NodeName = NodeName String deriving (Eq, Ord, Show)

newtype Cost = Cost Float

data TypeB = TypeB Cost NodeName [TypeB]

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa t1 t2 = filterBepa $ nub $ commonNodeNames
  (getNodeNames t1)
  (getNodeNames t2)
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
