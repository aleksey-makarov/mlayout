
  λ(undefined : ∀(t : Type) → t)
→ let TreeBase = ./TreeBase/Type
  
  let TreeBase/functor = ./TreeBase/functor
  
  let TreeBase/bifunctor = ./TreeBase/bifunctor
  
  let Tree = ./Tree/Type
  
  let Tree/create = ./Tree/create
  
  let Tree/functor = ./Tree/functor
  
  let Tree/foldable = ./Tree/foldable
  
  let Tree/traversable = ./Tree/traversable
  
  let Foldable/foldMap = ./Foldable/foldMap
  
  let Function/id = ./Function/id
  
  let Function/flip = ./Function/flip
  
  let Text/monoid = ./Text/monoid
  
  let Traversable = ./Traversable/Type
  
  let List/traversable = ./List/traversable
  
  let State = ./State/Type
  
  let State/applicative = ./State/applicative
  
  let State/eval = ./State/eval
  
  let treeBase1
      : TreeBase Natural Natural
      = { data = 42, subtrees = [ 12, 34 ] }
  
  let tree1
      : Tree Natural
      = let mk = Tree/create Natural
        
        let mkLeaf = λ(n : Natural) → mk n ([] : List (Tree Natural))
        
        in  mk 1 [ mkLeaf 11, mk 12 [ mkLeaf 121, mkLeaf 122 ], mkLeaf 13 ]
  
  let tree1text : Tree Text = Tree/functor.map Natural Text Natural/show tree1
  
  let foldTree1
      : TreeBase Natural Text → Text
      =   λ(t : TreeBase Natural Text)
        → ./Text/concat ([ "<", Natural/show t.data ] # t.subtrees # [ ">" ])
  
  let foldTree1text
      : TreeBase Text Text → Text
      =   λ(t : TreeBase Text Text)
        → ./Text/concat ([ "<", t.data ] # t.subtrees # [ ">" ])
  
  let enumerate =
          λ(t : Type → Type)
        → λ(traversable : Traversable t)
        → λ(container : t Text)
        → let f =
                  λ(txt : Text)
                → λ(s : Natural)
                → { val = "${Natural/show s}:${txt}", state = s + 1 }
          
          in  State/eval
              Natural
              (t Text)
              ( traversable.traverse
                (State Natural)
                (State/applicative Natural)
                Text
                Text
                f
                container
              )
              0
  
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
          Foldable/foldMap
          Text
          Text/monoid
          Tree
          Tree/foldable
          Text
          (Function/id Text)
          (Tree/functor.map Text Text (λ(x : Text) → "<" ++ x ++ ">") tree1text)
      , test08a =
          enumerate List List/traversable [ "one", "two", "three" ]
      , test08 =
          enumerate Tree Tree/traversable tree1text
      }
