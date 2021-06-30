{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text

example :: Diagram B
--example = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none
--example = square 1 # fc aqua
--lhs = rect 2 5 # fc green === square 2 # fc red
--rhs = square 2 # fc yellow === rect 2 5 # fc aqua
w = 2
h = 5
lhs1 = text "f1" <> rect w h 
lhs2 = text "f2" <> square w
lhs  = lhs1 === lhs2 
rhs1 = text "f3" <> square w
rhs2 = text "f4" <> rect w h
rhs  = rhs1 === rhs2
example = center $ alignT lhs ||| alignT rhs
main = mainWith example