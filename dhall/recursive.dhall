let PersonFunctor
    : Type → Type
    = λ(A : Type) → { children : List A, name : Text }

let Person : Type = ∀(A : Type) → (PersonFunctor A → A) → A

let personCreate
    : Text → Person
    =   λ(name : Text)
      → λ(A : Type)
      → λ(f : PersonFunctor A → A)
      → f { children = [] : List A, name = name }

let personAddChild
    : Person → Person → Person
    =   λ(p : Person)
      → λ(c : Person)
      → λ(A : Type)
      → λ(f : PersonFunctor A → A)
      → let cx : A = c A f
        
        let ff
            : PersonFunctor A → A
            =   λ(pf : PersonFunctor A)
              → f { children = pf.children # [ cx ], name = pf.name ++ "." }
        
        in  p A ff

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
