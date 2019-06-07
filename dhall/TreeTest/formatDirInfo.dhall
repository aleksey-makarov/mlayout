
let DirEntry = ./DirEntry

let DirInfo = ./DirInfo

let formatDirEntry = ./formatDirEntry

let List/map = ../List/map

let List/foldable = ../List/foldable

let List/intersperse = ../List/intersperse

let Text/monoid = ../Text/monoid

let Foldable/foldMap = ../Foldable/foldMap

let Function/id = ../Function/id

let entriesText =
        λ(l : List DirEntry)
      → Foldable/foldMap
        Text
        Text/monoid
        List
        List/foldable
        Text
        (Function/id Text)
        (List/intersperse Text ", " (List/map DirEntry Text formatDirEntry l))

in  λ(info : DirInfo) → "- ${info.name}: ${entriesText info.dirEntries}"
