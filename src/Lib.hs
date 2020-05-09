module Lib where


data NodeInfo = NodeInfo
  { cost :: Cost
  , nodeInfoName :: NodeName
  } deriving (Show)

data Tree
  = Tree_TypeA NodeInfo
               String
               [Tree]
  | Tree_TypeB TypeB
  deriving (Show)

newtype NodeName =
  NodeName String
  deriving (Show, Eq)

newtype Cost =
  Cost Float
  deriving (Show)

data TypeB =
  TypeB Cost
        NodeName
        [TypeB]
  deriving (Show)

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa t1 t2 =
  getCommonNodeNamesExceptBepa' t1 ++ getCommonNodeNamesExceptBepa' t2

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
