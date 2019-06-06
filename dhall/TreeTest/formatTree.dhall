let DirEntry = ./DirEntry

let DirInfo = ./DirInfo

let Foldable/foldMap = ../Foldable/foldMap

let List/foldable = ../List/foldable

let NonEmptyList = ../NonEmptyList/Type

let Text/monoid = ../Text/monoid

let Tree = ../Tree/Type

let Tree/recite = ../Tree/recite

in    λ(tree : Tree DirInfo)
    → let printOneDir = λ(di : NonEmptyList DirInfo) → "lalala"
      
      in  Foldable/foldMap
          Text
          Text/monoid
          List
          List/foldable
          (NonEmptyList DirInfo)
          printOneDir
          (Tree/recite DirInfo tree)
