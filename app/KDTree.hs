data Cut = Horizontal | Vertical deriving (Read, Show)

-- Horizontal Cut corresponds to vertical composition
-- Vertical Cut corresponds to horizontal composition

data KDTree morphism object = Leaf morphism [object] [object] | Node Cut (KDTree morphism object) (KDTree morphism object) deriving (Show, Read)
type KDString = KDTree String String
f1 :: KDString
f1 = Leaf "f1" ["x1"] ["x3", "x4"]
f2 :: KDString 
f2 = Leaf "f2" ["x2"] ["x5"]
f3 :: KDString 
f3 = Leaf "f3" ["x3"] ["x6"]
f4 :: KDString 
f4 = Leaf "f4" ["x4", "x5"] ["x7"]
mytree :: KDTree String String
mytree = Node Vertical ((Node Horizontal (f1 f2)) (Node Horizontal (f3 f4)))