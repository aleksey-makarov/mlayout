{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module DhallExtra where

import Data.Functor.Foldable
import Data.List.NonEmpty as LNE
import Data.Sequence as DS
import Data.Tree
import Dhall
import Dhall.Core as DC
import Dhall.TH
import Dhall.Map as DM
import Dhall.Parser
import Dhall.TypeCheck

import DataFunctorFoldableExtra

treeType = $(staticDhallExpression "./Tree/Type")
treeToDhallExpression = $(staticDhallExpression "let Tree = ./Tree/Type \
                                                \let List/map = ./List/map \
                                                \in λ(dtype : Type) \
                                                \ → λ(d : dtype) \
                                                \ → λ(children : List (Tree dtype)) \
                                                \ → λ(a : Type) \
                                                \ → λ(f : { data : dtype, subtrees : List a } → a) \
                                                \ → f { data = d, subtrees = List/map (Tree dtype) a (λ(tree : Tree dtype) → tree a f) children }"
                        )

treeToDhall :: Expr Src X -> Expr Src X -> [Expr Src X] -> Expr Src X
treeToDhall dtype d children = App (App (App treeToDhallExpression dtype) d) childrenExpression
  where
    childrenExpression  = ListLit (Just (App treeType dtype)) (DS.fromList children)

instance Inject d => Inject (Tree d) where
    injectWith options = InputType {..}
      where
        embed = normalize . cata embedAlg
        embedAlg (TreeF d l) = treeToDhall declaredIn (embedIn d) l
        declared = normalize $ App treeType declaredIn
        InputType embedIn declaredIn = injectWith options


