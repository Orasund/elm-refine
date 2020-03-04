require "unification.k"
require "elm-syntax.k"

module ELM-TYPESYSTEM
  imports UNIFICATION
  imports ELM-SYNTAX

  configuration <k> $PGM:Exp </k>
                <tenv> .Map </tenv>

  syntax ListExpField ::= ListTypeFields
  rule A:Id = E:Type , => A : E ,
  rule A:Id = E:Type , LEF:ListTypeFields => A : E , LEF
  syntax KResult ::= ListTypeFields

  syntax ListExp ::= Type
  rule E:Type , => E
  rule E:Type , LE:Type 
    => E =Type LE 
      ~> E
  syntax KResult ::= Type

  //inference Rules for Expressions
  syntax Exp ::= Type //The following rules replace Exp with Type
  rule foldl
    => (?A:Type -> ?B:Type -> ?B) -> ?B:Type -> ( list ?A ) -> ?B
  rule (::)
    => ?A:Type -> ( list ?A ) -> ( list ?A )
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
    => ( E2 =Type (E1 -> ?T:Type) )
      ~> ?T
  rule E1:Type >> E2:Type
    => (E1 =Type (?T1 -> ?T2:Type))
      ~> (E2 =Type (?T2 -> ?T3))
      ~> (?T1:Type -> ?T3:Type)
  rule if E1:Type then E2:Type else E3:Type
    => E1 =Type bool 
      ~> E2 =Type E3
      ~> E2
  rule { LEF:ListTypeFields }Exp => { LEF }Type
  rule {}Exp => {}Type
  //rule <k> ( { A:Id | LEF:ListTypeFields } )
  //  => { LTF }Type
  //  ...</k>
  //  <tenv>... A |-> { LTF:ListTypeFields }Type ...</tenv>
  //  requires containsFields LTF LEF
  //rule <k> let A:Id = E1:Type in E2:Exp
  //  => E2
  //  ...</k>
  //  <tenv> TEnv:Map => TEnv [ A <- E1 ] </tenv>
  //rule [TCaseOf]
  rule E1:Type E2:Type
    => E1 =Type (E2 -> ?T:Type)
      ~> ?T
  rule boolExp B:Bool => bool
  rule intExp I:Int => int
  rule []Exp => list ?A:Type
  rule [ T:Type ]Exp => list T
  rule ( E1:Type , E2:Type )Exp => ( E1 , E2 )Type
  rule <k> fun A:Id -> E:Type => ?T:Type -> E ~> setTenv(TEnv) ...</k>
    <tenv> TEnv:Map => TEnv [ A <- ?T ] </tenv>
  //rule [TConstructor]
  //rule <k> variable A:Id => T ...</k>
  //  <tenv>... A |-> T:Type ...</tenv>

  syntax KResult ::= Type

  syntax PolyType ::= "forall" Set "." Type
  syntax Type ::= MetaVariable

  syntax KItem ::= ListExp "isType" Type
  rule (T:Type ,) isType (list T) => .
  rule (T:Type , LE:ListExp) isType (list T) => .
    requires LE isType (list T)

  syntax Bool ::= "containsFields" ListTypeFields ListTypeFields [function, functional]
  rule containsFields LTF1:ListTypeFields (A:Id : T:Type, )
    => contains LTF1 A T
  rule containsFields LTF1:ListTypeFields (A:Id : T:Type , LTF2:ListTypeFields)
    => (contains LTF1 A T) andBool (containsFields LTF1 LTF2)

  syntax Bool ::= "contains" ListTypeFields Id Type [function, functional]
  rule contains (A1:Id : T1:Type ,) A2 T2
    => T1 =Type T2
      ~> (A1 ==K A2)
  rule contains (A1:Id : T1:Type , LTF:ListTypeFields) A2 T2
    => T1 =Type T2
      ~> true
   requires A1 ==K A2
  rule contains (A1:Id : T1:Type , LTF:ListTypeFields) A2 T2
    => contains LTF A2 T2
    requires A1 =/=K A2

  syntax Map ::= "getField" ListTypeFields Id [function, functional]
  rule getField (A:Id : T:Type ,) A => T
  rule getField (A1:Id : T:Type , LTF:ListTypeFields) A2
    => T
    requires A1 ==K A2
  rule getField (A1:Id : T:Type , LTF:ListTypeFields) A2
    => getField LTF A2
    requires A1 =/=K A2

  
  rule <k> let X = T:Type in E => E ~> setTenv(TEnv) 
    ...</k>
    <tenv> TEnv 
      => TEnv[ X 
        <- forall (#metaKVariables(T) -Set #metaKVariables(setTenv(TEnv))) . 
          ( #freezeKVariables(T, setTenv(TEnv)):>Type
          )
      ]
    </tenv>
  rule <k> A1:Id get A2:Id
    => #renameMetaKVariables(getField LTF A2, Tvs)
    ...</k>
    <tenv>... A1 |-> forall Tvs . { LTF:ListTypeFields }Type ...</tenv>
  rule <k> ( { A:Id | LEF:ListTypeFields } )
    => #renameMetaKVariables({ LTF }Type,Tvs)
    ...</k>
    <tenv>... A |-> forall Tvs . { LTF:ListTypeFields }Type ...</tenv>
    //requires containsFields (#renameMetaKVariables(LTF,Tvs)):>ListTypeFields LEF
  rule <k> variable X:Id => #renameMetaKVariables(T, Tvs) ...</k>
    <tenv>... X |-> forall Tvs . T
    ...</tenv>

  syntax KItem ::= Type "=Type" Type
  rule T =Type T => .


  syntax KItem ::= setTenv(Map)
  rule <k> T:Type ~> (setTenv(TEnv) => .) ...</k>
   <tenv> _ => TEnv </tenv>
endmodule