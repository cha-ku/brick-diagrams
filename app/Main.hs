{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text

data Cut = Horizontal Double | Vertical  deriving (Eq, Show, Read)

--Binary Tree with Cut
--data Tree a = Leaf a | Node Cut (Tree a) (Tree a) deriving (Show, Read)
--type STree = Tree String 
--binTree :: STree
--binTree = Node Vertical (Node (Horizontal 0.5) (Leaf "f1") (Leaf "f2")) (Node Horizontal (Leaf "f3") (Leaf "f4"))
--
--treeToString :: STree -> String 
--treeToString (Leaf n) = "leaf - " ++ show n
--treeToString (Node cut lt rt) = "cut - " ++ show cut ++ " | " ++ treeToString lt ++ treeToString rt
--

--treeToDiag :: STree -> Diagram B
--treeToDiag (Leaf a) = text a <> square side
--treeToDiag (Node cut lt rt) =
--    case cut of Horizontal sf -> treeToDiag lt === treeToDiag rt
--                Vertical      -> treeToDiag lt ||| treeToDiag rt

-- KD Tree
-- get a ratio value alongside the Cut value 
side = 5
data KDTree morphism object = KLeaf morphism [object] [object] | KNode Cut (KDTree morphism object) (KDTree morphism object) deriving (Show, Read)
type KDString = KDTree String String
f1 = KLeaf "f1" ["x1"] ["x3", "x4"]
f2 = KLeaf "f2" ["x2"] ["x5"]
f3 = KLeaf "f3" ["x3"] ["x6"]
f4 = KLeaf "f4" ["x4", "x5"] ["x7"]
mytree :: KDString
mytree = KNode Vertical (KNode (Horizontal 0.66) f1 f2) (KNode (Horizontal 0.33) f3 f4)

-- scale the leaf during composition rather than from the get go 
kdtreeToDiag :: KDString -> Diagram B
kdtreeToDiag (KLeaf t xs ys) = text t <> square side
kdtreeToDiag (KNode cut lt rt) =
    -- get user to supply the overlap ratio
    case cut of Horizontal sf -> kdtreeToDiag lt # scaleY (sf * side) === kdtreeToDiag rt # scaleY ((1-sf) * side)
                Vertical      ->alignT (kdtreeToDiag lt # scaleX (0.5 * side))  ||| alignT (kdtreeToDiag rt # scaleX (0.5 * side))


main = mainWith $ alignT $ kdtreeToDiag mytree