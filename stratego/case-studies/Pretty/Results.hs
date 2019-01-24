module Pretty.Results where

import Prelude hiding ((<>))

import WildcardSemantics

import Data.Foldable

import Text.PrettyPrint

ppResults :: (Functor f, Foldable f) => (Term -> Doc) -> f Term -> Doc
ppResults ppTerm res = braces
              $ cat
              $ punctuate (comma <> space)
              $ toList
              $ ppTerm <$> res
