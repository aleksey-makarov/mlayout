{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import XTree

treeType :: Expr Src X
treeType = $(staticDhallExpression "./Tree/Type")

treeEmbed :: Expr Src X
treeEmbed = $(staticDhallExpression "let Tree = ./Tree/Type \
                                    \in λ(dtype : Type) \
                                    \ → λ(d : dtype) \
                                    \ → λ(children : List (Tree dtype)) \
                                    \ → ((./Tree/steppable dtype).embed { data = d, subtrees = children })"
             )

listToDhallList :: Expr Src X -> [Expr Src X] -> Expr Src X
listToDhallList dtype dlist = ListLit (Just dtype) (DS.fromList dlist)

treeToDhall :: Expr Src X -> Expr Src X -> [Expr Src X] -> Expr Src X
treeToDhall dtype d children = App (App (App treeEmbed dtype) d) (listToDhallList (App treeType dtype) children)

instance Inject d => Inject (Tree d) where
    injectWith options = InputType {..}
      where
        embed = normalize . cata embedAlg
        embedAlg (TreeF d l) = treeToDhall declaredIn (embedIn d) l
        declared = normalize $ App treeType declaredIn
        InputType embedIn declaredIn = injectWith options

xtreeType :: Expr Src X
xtreeType = $(staticDhallExpression "./XTree/Type")

xtreeEmbed :: Expr Src X
xtreeEmbed = $(staticDhallExpression "let XTree = ./XTree/Type \
                                     \in λ(ltype : Type) \
                                     \ → λ(ntype : Type) \
                                     \ → λ(children : List < leaf : ltype | node : { node : ntype, subtrees : XTree ltype ntype } >) \
                                     \ → ((./XTree/steppable ltype ntype).embed children)"
              )

xtreeToDhall :: Expr Src X -> Expr Src X -> [Expr Src X] -> Expr Src X
xtreeToDhall ltype ntype children = App (App (App xtreeEmbed ltype) ntype) (listToDhallList (App (App xtreeType ltype) ntype) children)

mkLeaf :: Expr Src X
mkLeaf = $(staticDhallExpression "let XTree = ./XTree/Type \
                                 \in λ(ltype : Type) \
                                 \ → λ(ntype : Type) \
                                 \ → λ(l : ltype) \
                                 \ → < leaf : ltype | node : { node : ntype, subtrees : XTree ltype ntype } >.leaf l"
          )

mkNode :: Expr Src X
mkNode = $(staticDhallExpression "let XTree = ./XTree/Type \
                                 \in λ(ltype : Type) \
                                 \ → λ(ntype : Type) \
                                 \ → λ(n : ntype) \
                                 \ → λ(t : XTree ltype ntype) \
                                 \ → < leaf : ltype | node : { node : ntype, subtrees : XTree ltype ntype } >.node { node = n, subtrees = t }"
          )


instance (Inject l, Inject n) => Inject (XTree l n) where
    injectWith options = InputType {..}
      where
        embed = normalize . cata embedAlg
        embedAlg (XTreeF l) = xtreeToDhall declaredL declaredN (eitherToDhall <$> l)
        declared = normalize $ App (App xtreeType declaredL) declaredN
        eitherToDhall (Left l) = App (App (App mkLeaf declaredL) declaredN) (embedL l)
        eitherToDhall (Right (n, e)) = App (App (App (App mkNode declaredL) declaredN) (embedN n)) e
        InputType embedL declaredL = injectWith options
        InputType embedN declaredN = injectWith options
