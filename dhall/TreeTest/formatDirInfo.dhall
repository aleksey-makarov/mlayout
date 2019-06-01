-- data DirEntry = AFile Text | OtherFile Text deriving (Show, Generic, Inject)
-- data DirInfo = DirInfo Text [DirEntry] deriving (Show, Generic, Inject)

let DirEntry = < AFile : { _1 : Text } | OtherFile : { _1 : Text } >

let DirInfo = { _name : Text, _entries : List DirEntry }

in  λ(info : DirInfo) → "hi there"
