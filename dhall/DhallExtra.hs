{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DhallExtra where

import Data.Functor.Foldable hiding (embed)
import Data.Sequence as DS
import Data.Tree
import Dhall
import Dhall.Core as DC
import Dhall.TH
import Dhall.Parser
import Dhall.TypeCheck

import DataFunctorFoldableExtra

treeType :: Expr Src X
treeType = $(staticDhallExpression "./Tree/Type")

-- treeSteppable = $(staticDhallExpression "./Tree/steppable")

treeToDhallExpression :: Expr Src X
treeToDhallExpression = $(staticDhallExpression "let Tree = ./Tree/Type \
                                                \let List/map = ./List/map \
                                                \in λ(dtype : Type) \
                                                \ → λ(d : dtype) \
                                                \ → λ(children : List (Tree dtype)) \
                                                \ → λ(a : Type) \
                                                \ → λ(f : { data : dtype, subtrees : List a } → a) \
                                                \ → f { data = d, subtrees = List/map (Tree dtype) a (λ(tree : Tree dtype) → tree a f) children }"
                        )


listToDhallList :: Expr Src X -> [Expr Src X] -> Expr Src X
listToDhallList dtype dlist = ListLit (Just dtype) (DS.fromList dlist)

treeToDhall :: Expr Src X -> Expr Src X -> [Expr Src X] -> Expr Src X
treeToDhall dtype d children = App (App (App treeToDhallExpression dtype) d) (listToDhallList (App treeType dtype) children)

instance Inject d => Inject (Tree d) where
    injectWith options = InputType {..}
      where
        embed = normalize . cata embedAlg
        embedAlg (TreeF d l) = treeToDhall declaredIn (embedIn d) l
        declared = normalize $ App treeType declaredIn
        InputType embedIn declaredIn = injectWith options


