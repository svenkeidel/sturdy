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
  Distribute :: CategoryExpr (x, Either y z) (Either (x,y) (x,z))
  Unit :: CategoryExpr x ()
  Apply :: CategoryExpr (x -> y, x) y
  Curry :: CategoryExpr (x,y) z -> CategoryExpr x (y -> z)
  Uncurry :: CategoryExpr x (y -> z) -> CategoryExpr (x,y) z

(◦) :: CategoryExpr y z -> CategoryExpr x y -> CategoryExpr x z
(◦) = Compose

(△) :: CategoryExpr x y -> CategoryExpr x z -> CategoryExpr x (y,z)
(△) = Product

(▽) :: CategoryExpr x z -> CategoryExpr y z -> CategoryExpr (Either x y) z
(▽) = CoProduct

product :: CategoryExpr gamma (x -> (y -> (x,y)))
product = Curry (Curry ((Pi2 ◦ Pi1) △ Pi2))

left :: CategoryExpr gamma (x -> Either x y)
left = Curry (In1 ◦ Pi2)

right :: CategoryExpr gamma (y -> Either x y)
right = Curry (In2 ◦ Pi2)

lam :: CategoryExpr (gamma,x) y -> CategoryExpr gamma (x -> y)
lam f = Curry f

app :: CategoryExpr gamma (x -> y) -> CategoryExpr gamma x -> CategoryExpr gamma y
app f x = Apply ◦ (f △ x)

matchProduct :: CategoryExpr gamma (x, y) -> CategoryExpr (gamma, (x, y)) z -> CategoryExpr gamma z
matchProduct f g = g ◦ (Id △ f)

matchCoproduct :: CategoryExpr gamma (Either x y) -> CategoryExpr (gamma, x) z -> CategoryExpr (gamma, y) z -> CategoryExpr gamma z
matchCoproduct f g h = (g ▽ h) ◦ Distribute ◦ (Id △ f)


