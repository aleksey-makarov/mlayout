let PersonFunctor
    : Type → Type
    = λ(A : Type) → { children : List A, name : Text }

let Person : Type = ∀(A : Type) → (PersonFunctor A → A) → A

let personCreate
    : Text → List Person → Person
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

let personDestruct
    : ∀(B : Type) → Person → (Text → List Person → B) → B
    =   λ(B : Type)
      → λ(p : Person)
      → λ(f : Text → List Person → B)
      → let extractName
            : PersonFunctor Text → Text
            = λ(pf : PersonFunctor Text) → pf.name
        
        let extractChildren
            : PersonFunctor Person → Person
            = λ(pf : PersonFunctor Person) → personCreate pf.name pf.children
        
        in  f (p Text extractName) ([] : List Person)

let getChildren1 : Person → List Person = λ(p : Person) → [] : List Person

let personAddChild
    : Person → Person → Person
    =   λ(p : Person)
      → λ(c : Person)
      → λ(A : Type)
      → λ(f : PersonFunctor A → A)
      → let ca : A = c A f
        
        let ff
            : PersonFunctor A → A
            =   λ(pf : PersonFunctor A)
              → f { children = pf.children # [ ca ], name = pf.name ++ "." }
        
        in  p A ff

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

let extractName
    : Text → List Person → Text
    = λ(name : Text) → λ(_ : List Person) → name

let getName : Person → Text = λ(p : Person) → personDestruct Text p extractName

let extractChildren
    : Text → List Person → List Person
    = λ(name : Text) → λ(children : List Person) → children

let getChildren
    : Person → List Person
    = λ(p : Person) → personDestruct (List Person) p extractChildren

in  { everybody =
        everybody example
    , nameOfTheTop =
        getName example
    , namesOfChildren =
        ./List/map Person Text getName (getChildren example)
    , namesOfChildren1 =
        ./List/map Person Text getName (getChildren1 example)
    , withNewChild =
        personAddChild example (personCreate "NewChild" ([] : List Person))
    }
