let DirEntry = ./DirEntry

let DirInfo = ./DirInfo

let Foldable/foldMap = ../Foldable/foldMap

let Function/id = ../Function/id

let List/intersperse = ../List/intersperse

let List/foldable = ../List/foldable

let NonEmptyList = ../NonEmptyList/Type

let NonEmptyList/functor = ../NonEmptyList/functor

let NonEmptyList/toList = ../NonEmptyList/toList

let Text/monoid = ../Text/monoid

let Tree = ../Tree/Type

let Tree/recite = ../Tree/recite

in    λ(tree : Tree DirInfo)
    → let printOneDir =
              λ(dil : NonEmptyList DirInfo)
            →     Foldable/foldMap
                  Text
                  Text/monoid
                  List
                  List/foldable
                  Text
                  (Function/id Text)
                  ( List/intersperse
                    Text
                    "/"
                    ( NonEmptyList/toList
                      Text
                      ( NonEmptyList/functor.map
                        DirInfo
                        Text
                        (λ(di : DirInfo) → di.name)
                        dil
                      )
                    )
                  )
              ++  ''
                  
                  ''
      
      in  Foldable/foldMap
          Text
          Text/monoid
          List
          List/foldable
          (NonEmptyList DirInfo)
          printOneDir
          (Tree/recite DirInfo tree)
