
let DirEntry = < aFile : Text | otherFile : Text >

let DirInfo = { name : Text, dirEntries : List DirEntry }

let formatDirEntry =
        λ(info : DirEntry)
      → merge
        { aFile = λ(t : Text) → "!!!${t}", otherFile = λ(t : Text) → t }
        info

let List/map = ../List/map

let List/foldable = ../List/foldable

let Text/monoid = ../Text/monoid

let Foldable/foldMap = ../Foldable/foldMap

let Function/id = ../Function/id

let appendComma = λ(a : Text) → "${a}, "

let entriesText =
        λ(l : List DirEntry)
      → Foldable/foldMap
        Text
        Text/monoid
        List
        List/foldable
        Text
        (Function/id Text)
        ( List/map
          Text
          Text
          appendComma
          (List/map DirEntry Text formatDirEntry l)
        )

in  λ(info : DirInfo) → "- ${info.name}: ${entriesText info.dirEntries}"
