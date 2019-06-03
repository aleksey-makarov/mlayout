
  λ(undefined : ∀(t : Type) → t)
→ let TreeBase = ./TreeBase/Type
  
  let TreeBase/functor = ./TreeBase/functor
  
  let TreeBase/bifunctor = ./TreeBase/bifunctor
  
  let Tree = ./Tree/Type
  
  let Tree/functor = ./Tree/functor
  
  let Tree/foldable = ./Tree/foldable
  
  let Tree/traversable = ./Tree/traversable
  
  let Tree/recite = ./Tree/recite
  
  let Tree/steppable = ./Tree/steppable
  
  let Tree/recursive = ./Tree/recursive
  
  let Foldable/foldMap = ./Foldable/foldMap
  
  let Function/id = ./Function/id
  
  let Function/flip = ./Function/flip
  
  let Function/compose = ./Function/compose
  
  let Text/monoid = ./Text/monoid
  
  let Traversable = ./Traversable/Type
  
  let List/traversable = ./List/traversable
  
  let List/map = ./List/map
  
  let State = ./State/Type
  
  let State/applicative = ./State/applicative
  
  let State/eval = ./State/eval
  
  let Text/concat = ./Text/concat
  
  let Tree/create =
          λ(t : Type)
        → λ(data : t)
        → λ(children : List (Tree t))
        → (Tree/steppable t).embed { data = data, subtrees = children }
  
  let Tree/destruct = λ(t : Type) → (Tree/steppable t).project
  
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
  
  let Tree/prettyPrint =
          λ(a : Type)
        → λ(show : a → Text)
        → λ(tree : Tree a)
        → let f = λ(a : a) → show a ++ "\n"
          
          let showA = λ(t : Tree a) → Tree/functor.map a Text f t
          
          let foldTextTree =
                  λ(t : Tree Text)
                → Foldable/foldMap
                  Text
                  Text/monoid
                  Tree
                  Tree/foldable
                  Text
                  (Function/id Text)
                  t
          
          let shift =
                  λ(t : Tree Text)
                → Tree/functor.map Text Text (λ(t : Text) → "  " ++ t) t
          
          let indent =
                  λ(t : Tree Text)
                → t
                  (Tree Text)
                  (   λ(n : TreeBase Text (Tree Text))
                    → Tree/create
                      Text
                      n.data
                      (List/map (Tree Text) (Tree Text) shift n.subtrees)
                  )
          
          in  foldTextTree (indent (showA tree))
  
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
      , test05rec =
          (Tree/recursive Natural).cata Text foldTree1 tree1
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
          Tree/prettyPrint
          Text
          (Function/id Text)
          (enumerate Tree Tree/traversable tree1text)
      , test09 =
          Tree/recite Text tree1text
      , test10 =
          let r = Tree/destruct Text tree1text
          
          let prettyPrintTextTree = Tree/prettyPrint Text (Function/id Text)
          
          let prettyPrintListOfTextTrees =
                Function/compose
                (List (Tree Text))
                (List Text)
                Text
                (List/map (Tree Text) Text prettyPrintTextTree)
                Text/concat
          
          in  ''
              data : ${r.data}
              subtrees:
              ${prettyPrintListOfTextTrees r.subtrees}''
      }
