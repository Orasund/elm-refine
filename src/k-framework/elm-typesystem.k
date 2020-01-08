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
  syntax ListExpField ::= ListTypeFields
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
  rule E1:Type |> E2:Type
    => ( E2 = (E1 -> ?T:Type) ~> ?T )
  rule E1:Type >> E2:Type
    => ( E1 = (?T1:Type -> ?T2:Type) ) ~> (E2 = (?T2 -> ?T3:Type) ) ~> (?T1 -> ?T3)
  rule if E1:Type then E2:Type else E3:Type
    => E1 = bool ~> E2 = E3 ~> E2
  //rule { LEF:ListTypeFields } => toMap { LEF }
  //rule {} => .Map
  //rule <k> { A:Id | M:Map }
  //  => T toMap ?M:Map ~> keys(M) <=Map keys(?M) ~> tenv(TEnv) ...</k>
  //     <tenv>... A |-> T:Type ...</tenv>

  //pattern matching
  syntax KItem ::= Type "=" Type
  rule T = T => .

  //rule { A:Id = T:Type } toMap (A |-> T) => .
  //rule { A:Id = T:Type , LEF:ListTypeFields } toMap M[A <- T]
  //  => LEF toMap M

  syntax Map ::= "toMap" Type 
  rule toMap {} => .Map
  //rule (toMap { A:Id = T:Type }) => (A |-> T)
  //rule toMap({ A:Id = T:Type , LEF:ListTypeFields }) => toMap ( { LEF } )
endmodule