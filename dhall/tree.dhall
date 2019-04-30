
  λ(undefined : ∀(t : Type) → t)
→ let TreeBase = ./TreeBase/Type
  
  let Tree = ./Tree/Type
  
  let treeBase1
      : TreeBase Natural Natural
      = { data = 42, subtrees = [ 12, 34 ] }
  
  let treeCreate
      : ∀(t : Type) → t → List (Tree t) → Tree t
      =   λ(t : Type)
        → λ(data : t)
        → λ(children : List (Tree t))
        → λ(a : Type)
        → λ(f : { data : t, subtrees : List a } → a)
        → let fx = λ(t : Tree t) → t a f
          
          in  f { data = data, subtrees = ./List/map (Tree t) a fx children }
  
  let textTreeCreate = treeCreate Text
  
  let tree1
      : Tree Text
      = textTreeCreate
        "top"
        [ textTreeCreate "child1" ([] : List (Tree Text))
        , textTreeCreate
          "child2"
          [ textTreeCreate "grandChild1" ([] : List (Tree Text))
          , textTreeCreate "grandChild2" ([] : List (Tree Text))
          ]
        , textTreeCreate "child3" ([] : List (Tree Text))
        ]
  
  let foldTree1
      : TreeBase Text Text → Text
      =   λ(t : TreeBase Text Text)
        → ./Text/concat ([ "<", t.data ] # t.subtrees # [ ">" ])
  
  in  { test01 =
          treeBase1
      , test02 =
          (./TreeBase/functor Natural).map Natural Text Natural/show treeBase1
      , test03 =
          tree1
      , test04 =
          tree1 Text foldTree1
      }
