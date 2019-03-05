let PersonFunctor
    : Type → Type
    = λ(A : Type) → { children : List A, name : Text }

let Person : Type = ∀(A : Type) → (PersonFunctor A → A) → A

let personCreate
    : Text → List Person → Person
    =   λ(name : Text)
      → λ(children : List Person)
      → λ(A : Type)
      → λ(f : PersonFunctor A → A)
      → let mapF = λ(p : Person) → p A f
        
        in  f
            { children =
                ./List/map Person A mapF children : List A
            , name =
                name
            }

let example
    : Person
    = personCreate
      "John"
      [ personCreate
        "Mary"
        [ personCreate "MaryChild1" ([] : List Person)
        , personCreate "MaryChild2" ([] : List Person)
        ]
      , personCreate "Jane" ([] : List Person)
      ]

let everybody
    : Person → List Text
    =   λ(x : Person)
      → x
        (List Text)
        (   λ(p : { children : List (List Text), name : Text })
          → [ p.name ] # ./List/concat Text p.children
        )

let result : List Text = everybody example

in  result
