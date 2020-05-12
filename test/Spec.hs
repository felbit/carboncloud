{-# LANGUAGE OverloadedStrings #-}

import           Data.Char                      ( toLower )
import           Data.List                      ( sort )
import           Data.Maybe                     ( isJust )
import           Lib
import           Test.QuickCheck

-- * Generators

instance Arbitrary NodeName where
  arbitrary = do
    name <-
      oneof
      $   return
      <$> [ "foo"
          , "bar"
          , "quuz"
          , "Some\n"
          , "Bepa"
          , "MoreBepa"
          , "BepaWithSuffix"
          , "InfixBepaword"
          , "bePa"
          , "b_EPA"
          ]
    return $ NodeName name

instance Arbitrary Cost where
  arbitrary = Cost <$> arbitrary

instance Arbitrary NodeInfo where
  arbitrary = NodeInfo <$> arbitrary <*> arbitrary


-- Consideration: For recursive generators I have used a quickly decreasing
--   size parameter (one fourth). This is beneficial for test running times but
--   may potentially be harmful for test correctness.

-- | Type B Generator
instance Arbitrary TypeB where
  arbitrary = sized typeB'

typeB' :: Int -> Gen TypeB
typeB' 0 = TypeB <$> (Cost <$> arbitrary) <*> arbitrary <*> return []
typeB' n | n > 0 =
  TypeB <$> (Cost <$> arbitrary) <*> arbitrary <*> typeBList (n `div` 4)

typeBList :: Int -> Gen [TypeB]
typeBList 0         = return []
typeBList n | n > 0 = vectorOf n (typeB' n)

-- | Tree Generator
instance Arbitrary Tree where
  arbitrary = sized tree'

tree' :: Int -> Gen Tree
tree' 0 = oneof
  [ Tree_TypeA <$> arbitrary <*> arbitrary <*> return []
  , Tree_TypeB <$> typeB' 0
  ]
tree' n | n > 0 = oneof
  [ Tree_TypeA <$> arbitrary <*> arbitrary <*> treeList (n `div` 4)
  , Tree_TypeB <$> typeB' n
  ]

treeList :: Int -> Gen [Tree]
treeList 0         = return []
treeList n | n > 0 = vectorOf n (tree' n)

-- * Properties

prop_getCommonNodeNamesExceptBepa :: Tree -> Tree -> Bool
prop_getCommonNodeNamesExceptBepa tree1 tree2 =
  sort (getCommonNodeNamesExceptBepa tree1 tree2)
    == sort (getCommonNodeNamesExceptBepa tree2 tree1)

prop_isBepaFindsBepaCorrectly :: NodeName -> Bool
prop_isBepaFindsBepaCorrectly val =
  if nodeName
     `elem` ["Bepa", "MoreBepa", "BepaWithSuffix", "InfixBepaword", "bePa"]
  then
    isBepa val
  else
    not (isBepa val)
  where (NodeName nodeName) = val

prop_commonNodeNamesIsCommutative :: [NodeName] -> [NodeName] -> Bool
prop_commonNodeNamesIsCommutative _  [] = True
prop_commonNodeNamesIsCommutative [] _  = True
prop_commonNodeNamesIsCommutative xs ys =
  commonNodeNames xs ys == commonNodeNames ys xs

-- 'bepa' (case insensitive) should never be part of the results
prop_bepaNotPartOfResultSet :: Tree -> Tree -> Bool
prop_bepaNotPartOfResultSet t1 t2 =
  (not . containsBepa) $ getCommonNodeNamesExceptBepa t1 t2
 where
  containsBepa [] = False
  containsBepa (nodeName : rest) =
    isBepaCaseInsensitive nodeName || containsBepa rest
  isBepaCaseInsensitive (NodeName nodeName) =
    isJust $ findSubString "bepa" (toLower <$> nodeName)

-- * Main

main :: IO ()
main = do
  quickCheck prop_isBepaFindsBepaCorrectly
  quickCheck prop_commonNodeNamesIsCommutative
  quickCheck prop_bepaNotPartOfResultSet
  quickCheck prop_getCommonNodeNamesExceptBepa
