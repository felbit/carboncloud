module Lib where


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

newtype Cost =
  Cost Float

data TypeB =
  TypeB Cost
        NodeName
        [TypeB]

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa _ _ = undefined
