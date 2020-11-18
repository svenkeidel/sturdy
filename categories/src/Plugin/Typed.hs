{-# LANGUAGE GADTs #-}
module Plugin.Typed where

import GhcPlugins

data CategoryExpr x y where
  Primitive :: Var -> CategoryExpr x y
  Id :: CategoryExpr x x
  Compose :: CategoryExpr y z -> CategoryExpr x y -> CategoryExpr x z
  Product :: CategoryExpr x y -> CategoryExpr x z -> CategoryExpr x (y,z)
  Pi1 :: CategoryExpr (x,y) x
  Pi2 :: CategoryExpr (x,y) y
  CoProduct :: CategoryExpr x z -> CategoryExpr y z -> CategoryExpr (Either x y) z
  In1 :: CategoryExpr x (Either x y)
  In2 :: CategoryExpr y (Either x y)
  Distribute :: CategoryExpr (Either x y, z) (Either (x,z) (y,z))
  Unit :: CategoryExpr x ()
  Apply :: CategoryExpr (x -> y, x) y
  Curry :: CategoryExpr (x,y) z -> CategoryExpr y (x -> z)
  Uncurry :: CategoryExpr x (y -> z) -> CategoryExpr (y,x) z

(◦) :: CategoryExpr y z -> CategoryExpr x y -> CategoryExpr x z
(◦) = Compose

(△) :: CategoryExpr x y -> CategoryExpr x z -> CategoryExpr x (y,z)
(△) = Product

(▽) :: CategoryExpr x z -> CategoryExpr y z -> CategoryExpr (Either x y) z
(▽) = CoProduct

product :: CategoryExpr gamma (x -> (y -> (x,y)))
product = Curry (Curry ((Pi1 ◦ Pi2) △ Pi1))

left :: CategoryExpr gamma (x -> Either x y)
left = Curry (In1 ◦ Pi1)

right :: CategoryExpr gamma (y -> Either x y)
right = Curry (In2 ◦ Pi1)

lam :: CategoryExpr (x,gamma) y -> CategoryExpr gamma (x -> y)
lam f = Curry f

app :: CategoryExpr gamma (x -> y) -> CategoryExpr gamma x -> CategoryExpr gamma y
app f x = Apply ◦ (f △ x)

matchProduct :: CategoryExpr gamma (x, y) -> CategoryExpr ((x, y),gamma) z -> CategoryExpr gamma z
matchProduct f g = g ◦ (f △ Id)

