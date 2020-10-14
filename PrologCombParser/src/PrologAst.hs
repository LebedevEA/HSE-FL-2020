module PrologAst where

type Ident = String

data Atom = Id Ident
          | AtList List
          | Seq Atom Atom
          deriving (Eq, Show)

type Head = Atom

data Def = DefJust Head
         | Rel Head Body
         deriving (Eq, Show)

data Body = BodyJust Atom
          | Disj Body Body
          | Conj Body Body
          deriving (Eq, Show)

data Module = Name Ident
            deriving (Eq, Show)

data TypeBody = TypeJust Atom
              | TypeSeq TypeBody TypeBody
              deriving (Eq, Show)

type TypeName = Ident

data Type = TypeDef TypeName TypeBody
          deriving (Eq, Show)

data Var = Variable Ident
         deriving (Eq, Show)

data DefList = DefConsV Var DefList
             | DefConsA Atom DefList
             | DefConsL List DefList
             | DefNil
             deriving (Eq, Show)

data HTList = HTListVarTail Var Var
            | HTListAtomTail Atom Var
            | HTListListTail List Var
            deriving (Eq, Show)

data List = ListDefHT HTList
          | ListDefDef DefList
          deriving (Eq, Show)

data Prolog = Pr Module [Type] [Def]
            deriving (Eq, Show)
