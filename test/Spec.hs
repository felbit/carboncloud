import           Lib

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test then putStrLn passStatement else putStrLn failStatement

typeB :: TypeB
typeB = TypeB (Cost 1.23) (NodeName "Node B") []

withBepa :: TypeB
withBepa = TypeB (Cost 1.23) (NodeName "Some Bepa") [withBepaWrongCase]

withBepaWrongCase :: TypeB
withBepaWrongCase = TypeB (Cost 1.23) (NodeName "New bePa-case") []

treeA = Tree_TypeA (NodeInfo (Cost 23.42) (NodeName "Node A"))
                   "Some Description"
                   [Tree_TypeB withBepa]

treeB = Tree_TypeB typeB

main :: IO ()
main = do
  putStrLn "Testing tree ..."
  assert
    (  (getCommonNodeNamesExceptBepa treeA treeB)
    == [NodeName "Node A", NodeName "New bePa-case", NodeName "Node B"]
    )
    "... success!"
    "... failure!"
