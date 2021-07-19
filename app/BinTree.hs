{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
--import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
--import Diagrams.TwoD.Shapes
--import Diagrams.TwoD.Text

w = 10
h = 10

data Cut = Horizontal | Vertical  deriving (Eq, Show, Read)

data Tree a = Leaf a | Node Cut (Tree a) (Tree a) deriving (Show, Read)
type STree = Tree String 
binTree :: STree
binTree = Node Vertical (Node Horizontal (Leaf "f1") (Leaf "f2")) (Node Horizontal (Leaf "f3") (Leaf "f4"))