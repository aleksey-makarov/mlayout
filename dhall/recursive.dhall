let Person
    : Type
    =   ∀(Person : Type)
      → ∀(MakePerson : { children : List Person, name : Text } → Person)
      → Person

let personCreate
    : Text → Person
    =   λ(name : Text)
      → λ(Person : Type)
      → λ(MakePerson : { children : List Person, name : Text } → Person)
      → MakePerson { children = [] : List Person, name = name }

let personAddChild
    : Person → Person → Person
    =   λ(person : Person)
      → λ(child : Person)
      → λ(_Person : Type)
      → λ(MakePerson : { children : List _Person, name : Text } → _Person)
      → MakePerson

let example
    : Person
    = personAddChild
      (personAddChild (personCreate "John") (personCreate "Mary"))
      (personCreate "Jane")

let everybody
    : Person → List Text
    = let concat = http://prelude.dhall-lang.org/List/concat
      
      in    λ(x : Person)
          → x
            (List Text)
            (   λ(p : { children : List (List Text), name : Text })
              → [ p.name ] # concat Text p.children
            )

let result : List Text = everybody example

in  result
