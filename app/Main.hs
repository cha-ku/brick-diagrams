{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text

data Cut = Horizontal | Vertical  deriving (Eq, Show, Read)

--Binary Tree with Cut
data Tree a = Leaf a | Node Cut (Tree a) (Tree a) deriving (Show, Read)
type STree = Tree String 
binTree :: STree
binTree = Node Vertical (Node Horizontal (Leaf "f1") (Leaf "f2")) (Node Horizontal (Leaf "f3") (Leaf "f4"))

treeToString :: STree -> String 
treeToString (Leaf n) = "leaf - " ++ show n
treeToString (Node cut lt rt) = "cut - " ++ show cut ++ " | " ++ treeToString lt ++ treeToString rt

side = 5
treeToDiag :: STree -> Diagram B
treeToDiag (Leaf a) = text a <> square side
treeToDiag (Node cut lt rt) = do
    if cut == Horizontal
        then treeToDiag lt === treeToDiag rt
    else if cut == Vertical
        then treeToDiag lt ||| treeToDiag rt
    else text "Error" <> square side

-- KD Tree

data KDTree morphism object = KLeaf morphism [object] [object] | KNode Cut (KDTree morphism object) (KDTree morphism object) deriving (Show, Read)
type KDString = KDTree String String
f1 = KLeaf "f1" ["x1"] ["x3", "x4"]
f2 = KLeaf "f2" ["x2"] ["x5"]
f3 = KLeaf "f3" ["x3"] ["x6"]
f4 = KLeaf "f4" ["x4", "x5"] ["x7"]
mytree :: KDString
mytree = KNode Vertical (KNode Horizontal f1 f2) (KNode Horizontal f3 f4)

kdtreeToDiag :: KDString -> Diagram B
kdtreeToDiag (KLeaf t xs ys) = text t <> rect (side * fromIntegral(length xs)) (side * fromIntegral(length ys))
kdtreeToDiag (KNode cut lt rt) = do
    if cut == Horizontal
        then kdtreeToDiag lt === kdtreeToDiag rt
    else if cut == Vertical
        then alignT (kdtreeToDiag lt) ||| alignT (kdtreeToDiag rt)
    else text "Error" <> square side


main = mainWith $ kdtreeToDiag mytree