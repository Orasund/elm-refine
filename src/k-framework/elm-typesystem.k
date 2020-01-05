require "unification.k"
require "elm-syntax.k"


module ELM-TYPESYSTEM
  imports UNIFICATION
  imports DOMAINS
  imports ELM-SYNTAX

  syntax KResult ::= Type

  configuration <k> $PGM:Exp </k>
                <tenv> .Map </tenv>

  //inference Rules for patterns
  //TODO

  //inference Rules for Expressions
 
  syntax Exp ::= Type //The following rules replace Exp with Type
  rule foldl
    => ?A ~> ?B ~> (?A -> ?B -> ?B) -> ?B -> ( list ?A ) -> ?B
  rule (::)
    => ?A ~> ( list ?A ) -> ( list ?A )
  rule (+)
    => int -> int -> int
  rule (-)
    => int -> int -> int
  rule (*)
    => int -> int -> int
  rule (/)
    => int -> int -> int
  rule (<)
    => int -> int -> bool
  rule (==)
    => int -> int -> bool
  rule not
    => bool -> bool
  rule (&&)
    => bool -> bool -> bool
  rule (||)
    => bool -> bool -> bool
  rule T1:Type |> T2:Type
    => ( T2 = (T1 -> ?T:Type) ~> ?T )
  rule T1:Type >> T2:Type
    => ( T1 = (?T3:Type -> ?T4:Type) ) ~> (T2 = (?T4 -> ?T5:Type) ) ~> (?T3 -> ?T5)

  syntax KItem ::= Type "=" Type
  rule T = T => .
endmodule