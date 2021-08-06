{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text

data Cut = Horizontal Double | Vertical  deriving (Eq, Show, Read)
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
                Vertical      -> alignT (kdtreeToDiag lt # scaleX (0.5 * side))  ||| alignT (kdtreeToDiag rt # scaleX (0.5 * side))


main = mainWith $ alignT $ kdtreeToDiag mytree