{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module DhallExtra where

import Data.List.NonEmpty as LNE
import Data.Sequence as DS
import Data.Tree
import Dhall
import Dhall.Core as DC
import Dhall.TH
import Dhall.Map as DM
import Dhall.Parser
import Dhall.TypeCheck

--    → λ(a : Type)
--    → λ(f : { data : dtype, subtrees : List a } → a)
--    → f { data = d, subtrees = List/map (Tree dtype) a (λ(tree : Tree dtype) → tree a f) children }

treeToDhall :: Expr Src X -> Expr Src X -> [Expr Src X] -> Expr Src X
treeToDhall dtype d children =
                    Lam "a" (Const DC.Type) $
                    Lam "f" (Pi "_" (Record $ DM.fromList [("data", dtype), ("subtrees", App List (v "a"))]) (v "a")) $
                    App (v "f") (RecordLit $ DM.fromList [("data", d), ("subtrees", s)])
  where
    v n = Var (V n 0)
    f   = Lam "tree" (App (v "Tree") dtype) (App (App (v "tree") (v "a")) (v "f"))
    s   = App (App (App (App (v "List/map") (App (v "Tree") dtype)) (v "a")) f) c'
    c'  = ListLit (Just (App (v "Tree") dtype)) (DS.fromList children)

appendLets :: Expr Src X -> Expr Src X
appendLets e = Let (LNE.fromList [Binding "Tree" Nothing treeType, Binding "List/map" Nothing listMap]) e
  where
    treeType = $(staticDhallExpression "./Tree/Type")
    listMap = $(staticDhallExpression "./List/map")

instance Inject d => Inject (Tree d) where
    injectWith options = InputType {..}
      where
        -- FIXME: add normalize
        embed t = appendLets $ embed' t
        embed' (Node d ns) = treeToDhall declaredIn (embedIn d) (fmap embed' ns)
        -- ∀(a : Type) → ({ data : t, subtrees : List a } -> a) -> a
        declared = Pi "a" (Const DC.Type) $ Pi "_" (Pi "_" (Record $ DM.fromList [("data", declaredIn), ("subtrees", (App List (v "a")))]) (v "a")) (v "a")
        InputType embedIn declaredIn = injectWith options
        v n = Var (V n 0)


