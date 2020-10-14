module Test.PrologParser where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Text.ParserCombinators.Parsec
import Data.Either (isLeft)

import PrologParser
import PrologAst

parseString :: Parser a -> String -> Either ParseError a
parseString p =
  parse (do r <- p; eof; return r) ""

testParserSuccess :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParserSuccess p inp exp =
  parseString p inp @?= Right exp

testParserFailure :: (Eq a, Show a) => Parser a -> String -> Assertion
testParserFailure p inp =
  assertLeft $ parseString p inp

assertLeft :: (Show a, Show b) => Either a b -> Assertion
assertLeft x =
  assertBool ("expected: Left\n but got: " ++ show x) (isLeft x)

unit_ident :: Assertion
unit_ident = do
  let parser = parseIdentNotVar
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "abc" "abc"
  success "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
          "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
  fail "123abc"
  fail "Xyz"

unit_var :: Assertion
unit_var = do
  let parser = parseVar
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "Abc" (Variable "Abc")
  success "H" (Variable "H")
  success "AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
          (Variable "AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890")
  fail "123abc"
  fail "xyz"

unit_manyIdent :: Assertion
unit_manyIdent = do
  let parser = many parseIdent
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a b c" ["a", "b", "c"]

unit_atom :: Assertion
unit_atom = do
  let parser = parseAtom
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a" (Id "a")
  success "a b c" (Seq (Id "a") (Seq (Id "b") (Id "c")))
  success "a (b c)" (Seq (Id "a") (Seq (Id "b") (Id "c")))
  success "a ((b c))" (Seq (Id "a") (Seq (Id "b") (Id "c")))
  success "a ((b c)) d" (Seq (Id "a") (Seq (Seq (Id "b") (Id "c")) (Id "d")))
  success "a ((b c))  (d)" (Seq (Id "a") (Seq (Seq (Id "b") (Id "c")) (Id "d")))
  success "a ((b  c))  (d)" (Seq (Id "a") (Seq (Seq (Id "b") (Id "c")) (Id "d")))
  success "a ((b  c) )  ( d )" (Seq (Id "a") (Seq (Seq (Id "b") (Id "c")) (Id "d")))
  success "a((b c))(d)" (Seq (Id "a") (Seq (Seq (Id "b") (Id "c")) (Id "d")))
  fail "a (a"
  fail "X a"
  fail "(a)"

l = Left
r = Right

a = Id "a"
b = Id "b"
c = Id "c"
d = Id "d"

unit_relation :: Assertion
unit_relation = do
  let parser = parseDef
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a." (DefJust a)
  success "a b." (DefJust (Seq a b))
  success "a:-a." (Rel a (BodyJust a))
  success "a :-a." (Rel a (BodyJust a))
  success "a:-a b." (Rel a (BodyJust (Seq a b)))
  success "a b:- (a b)  ." (Rel (Seq a b) (BodyJust (Seq a b)))
  success "a b:- a;b,c." (Rel (Seq a b) (Disj (BodyJust a) (Conj (BodyJust b) (BodyJust c))))
  success "a b:- a;(b,c)." (Rel (Seq a b) (Disj (BodyJust a) (Conj (BodyJust b) (BodyJust c))))
  success "a b:- (a;b),c." (Rel (Seq a b) (Conj (Disj (BodyJust a) (BodyJust b)) (BodyJust c)))
  success "a b:- a;b;c." (Rel (Seq a b) (Disj (BodyJust a) (Disj (BodyJust b) (BodyJust c))))
  success "a b:- a,b,c." (Rel (Seq a b) (Conj (BodyJust a) (Conj (BodyJust b) (BodyJust c))))
  success "a (b (c))  :- (a b) ." (Rel (Seq a (Seq b c)) (BodyJust (Seq a b)))

unit_typeExpr :: Assertion
unit_typeExpr = do
  let parser = parseTypeBody
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a" (TypeJust a)
  success "a -> b" (TypeSeq (TypeJust a) (TypeJust b))
  success "(a -> b)" (TypeSeq (TypeJust a) (TypeJust b))
  success "(a -> b) -> c" (TypeSeq (TypeSeq (TypeJust a) (TypeJust b)) (TypeJust c))
  success "a -> b -> c" (TypeSeq (TypeJust a) (TypeSeq (TypeJust b) (TypeJust c)))
  success "list (list a) -> list a -> o" (TypeSeq (TypeJust $ Seq (Id "list") (Seq (Id "list") a)) (TypeSeq (TypeJust $ Seq (Id "list") a) (TypeJust $ Id "o")))
  success "pair a b -> (a -> c) -> (b -> d) -> pair c d"
          (TypeSeq
              (TypeJust $ Seq (Id "pair") (Seq a b))
              (TypeSeq
                (TypeSeq (TypeJust a) (TypeJust c))
                (TypeSeq
                  (TypeSeq 
                    (TypeJust b)
                    (TypeJust d)
                  )
                  (TypeJust $ Seq (Id "pair") (Seq c d))
                )
              )
          )

unit_type :: Assertion
unit_type = do
  let parser = parseType
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "type a b." (TypeDef "a" (TypeJust b))
  success "type a b -> c." (TypeDef "a" (TypeSeq (TypeJust b) (TypeJust c)))
  success "type filter (a -> o) -> list a -> list a -> o." (TypeDef "filter" (TypeSeq (TypeSeq (TypeJust a) ((TypeJust (Id "o")))) (TypeSeq (TypeJust (Seq (Id "list") a)) (TypeSeq (TypeJust $ Seq (Id "list") a) (TypeJust (Id "o"))))))
  success "type a (((b)))." (TypeDef "a" (TypeJust b))
  success "type d a -> (((b)))." (TypeDef "d" (TypeSeq (TypeJust a) (TypeJust b)))

  fail "type type type -> type."
  fail "type x -> y -> z."
  fail "tupe x o."

unit_module :: Assertion
unit_module = do
  let parser = parseModule
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "module name." (Name "name")
  success " \t\nmodule\n\n  name_123." (Name "name_123")
  fail "modulo name."
  fail "module module."
  fail "modulename."
  fail "mod ule name."
  fail "module 123name."
  fail "module name!"

unit_list :: Assertion
unit_list = do
  let parser = parseList
  let success = testParserSuccess parser
  let fail = testParserFailure parser
  success "[]" (ListDefDef DefNil)
  success "[a]" (ListDefDef $ DefConsA a DefNil)
  success "[A,B]" (ListDefDef (DefConsV (Variable "A") (DefConsV (Variable "B") DefNil)))
  success "[a (b c), B, C]" (ListDefDef $ DefConsA (Seq a (Seq b c)) (DefConsV (Variable "B") (DefConsV (Variable "C") DefNil)))
  success "[a | T]" (ListDefHT $ HTListAtomTail a (Variable "T"))
  success "[ [a] | T ]" (ListDefHT $ HTListListTail (ListDefDef $ DefConsA a DefNil) (Variable "T"))
  success "[ [H | T], a ]" (ListDefDef $ DefConsL (ListDefHT $ HTListVarTail (Variable "H") (Variable "T")) (DefConsA a DefNil))
  fail "[a | a]"
  fail "[A,B,]"
  fail "[A,B"
  fail "]["
  
