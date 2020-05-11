import           Data.Char                      ( toLower )
import           Data.List                      ( sort )
import           Data.Maybe                     ( isJust )
import           Lib
import           Test.QuickCheck

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
          ]
    return $ NodeName name

instance Arbitrary NodeInfo where
  arbitrary = do
    cost     <- Cost <$> arbitrary
    nodeName <- arbitrary
    return $ NodeInfo cost nodeName

instance Arbitrary TypeB where
  arbitrary = sized typeB'

typeB' :: Int -> Gen TypeB
typeB' 0 = TypeB <$> (Cost <$> arbitrary) <*> arbitrary <*> return []
typeB' n | n > 0 =
  TypeB <$> (Cost <$> arbitrary) <*> arbitrary <*> typeBList (n `div` 2)

typeBList :: Int -> Gen [TypeB]
typeBList 0         = return []
typeBList n | n > 0 = vectorOf n (typeB' n)

instance Arbitrary Tree where
  arbitrary = sized tree'

tree' :: Int -> Gen Tree
tree' 0 = oneof
  [ Tree_TypeA <$> arbitrary <*> arbitrary <*> return []
  , Tree_TypeB <$> typeB' 0
  ]
tree' n | n > 0 = oneof
  [ Tree_TypeA <$> arbitrary <*> arbitrary <*> treeList (n `div` 2)
  , Tree_TypeB <$> typeB' n
  ]

treeList :: Int -> Gen [Tree]
treeList 0         = return []
treeList n | n > 0 = vectorOf n (tree' n)

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
  sort (commonNodeNames xs ys) == sort (commonNodeNames ys xs)

-- 'bepa' (case insensitive) should never be part of the results
prop_bepaNotPartOfResultSet :: [NodeName] -> Bool
prop_bepaNotPartOfResultSet nodeNames = (not . containsBepa) filteredNodeTypes
 where
  filteredNodeTypes = filterBepa nodeNames
  containsBepa [] = False
  containsBepa (nodeName : rest) =
    isBepaCaseInsensitive nodeName || containsBepa rest
  isBepaCaseInsensitive (NodeName nodeName) =
    isJust $ findSubString "bepa" (toLower <$> nodeName)


main :: IO ()
main = do
  quickCheck prop_isBepaFindsBepaCorrectly
  quickCheck prop_commonNodeNamesIsCommutative
  quickCheck prop_bepaNotPartOfResultSet
  quickCheck prop_getCommonNodeNamesExceptBepa
