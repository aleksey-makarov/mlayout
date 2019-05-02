
  λ(undefined : ∀(t : Type) → t)
→ let TreeBase = ./TreeBase/Type
  
  let TreeBase/functor = ./TreeBase/functor
  
  let TreeBase/bifunctor = ./TreeBase/bifunctor
  
  let Tree = ./Tree/Type
  
  let Tree/create = ./Tree/create
  
  let Tree/functor = ./Tree/functor
  
  let Tree/foldable = ./Tree/foldable
  
  let treeBase1
      : TreeBase Natural Natural
      = { data = 42, subtrees = [ 12, 34 ] }
  
  let naturalTreeCreate = Tree/create Natural
  
  let tree1
      : Tree Natural
      = naturalTreeCreate
        1
        [ naturalTreeCreate 11 ([] : List (Tree Natural))
        , naturalTreeCreate
          12
          [ naturalTreeCreate 121 ([] : List (Tree Natural))
          , naturalTreeCreate 122 ([] : List (Tree Natural))
          ]
        , naturalTreeCreate 13 ([] : List (Tree Natural))
        ]
  
  let tree1text : Tree Text = Tree/functor.map Natural Text Natural/show tree1
  
  let foldTree1
      : TreeBase Natural Text → Text
      =   λ(t : TreeBase Natural Text)
        → ./Text/concat ([ "<", Natural/show t.data ] # t.subtrees # [ ">" ])
  
  let foldTree1text
      : TreeBase Text Text → Text
      =   λ(t : TreeBase Text Text)
        → ./Text/concat ([ "<", t.data ] # t.subtrees # [ ">" ])
  
  in  { test01 =
          treeBase1
      , test02 =
          (TreeBase/functor Natural).map Natural Text Natural/show treeBase1
      , test03 =
          TreeBase/bifunctor.bimap
          Natural
          Natural
          Text
          Text
          Natural/show
          Natural/show
          treeBase1
      , test04 =
          tree1
      , test05 =
          tree1 Text foldTree1
      , test06 =
          tree1text Text foldTree1text
      , test07 =
          Tree/foldable.fold
      }
