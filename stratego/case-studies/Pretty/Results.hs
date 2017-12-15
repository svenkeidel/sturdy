module Pretty.Results where

import WildcardSemantics

import Data.Foldable

import Text.PrettyPrint hiding (sep)


ppResults :: (Functor f, Foldable f) => (Term -> Doc) -> f Term -> Doc
ppResults ppTerm res = braces
              $ cat
              $ punctuate (comma <> space)
              $ toList
              $ ppTerm <$> res
