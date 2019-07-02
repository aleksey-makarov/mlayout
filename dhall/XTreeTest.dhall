-- recited : XTreeDir → List { head : List Text, tail : Optional Text }
-- let recursiveDir = ./XTree/recursive Text Text

let XTreeDir = ./XTree/Type Text Text

let reciteDir = ./XTree/recite Text Text

let Function/id = ./Function/id

let Text/concatMap = ./Text/concatMap

let Text/concat = ./Text/concat

let List/map = ./List/map

let List/intersperse = ./List/intersperse

let List/null = ./List/null

in    λ(t : XTreeDir)
    → let fOpt =
              λ(ox : Optional Text)
            → Optional/fold Text ox Text (Function/id Text) "-"
      
      let fList =
              λ(lx : List Text)
            →       if List/null Text lx
              
              then  "."
              
              else  Text/concat (List/intersperse Text "/" lx)
      
      let f =
              λ(x : { head : List Text, tail : Optional Text })
            → fList x.head ++ ": " ++ fOpt x.tail ++ "\n"
      
      in  Text/concatMap
          { head : List Text, tail : Optional Text }
          f
          (reciteDir t)
