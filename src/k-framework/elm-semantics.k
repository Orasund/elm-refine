require "unification.k"
require "substitution.k"
require "elm-syntax.k"

module ELM-SEMANTICS
  imports UNIFICATION
  imports SUBSTITUTION
  imports DOMAINS
  imports ELM-SYNTAX

  configuration <k> $PGM:Exp </k>
                <tenv> .Map </tenv>

  syntax ListValueField ::=
    "field" Id Value ","
    | "field" Id Value "," ListValueField

  syntax Value ::= Id
    | "intValue" Int
    | "boolValue" Bool
    | "{}Value"
    | "{" ListValueField "}Value"         [strict]
    | "(" Value "," Value ")Value"        [strict]
    | "[]Value"
    | "cons" Value Value                  [strict]
    | "lambda" Id "." Value               [binder,strict]
    | "fold" Value Value Value            [strict]
    | "add" Value Value                   [strict]
    | "sub" Value Value                   [strict]
    | "mult" Value Value                  [strict]
    | "div" Value Value                   [strict]
    | "eq" Value Value                    [strict]
    | "less" Value Value                  [strict]
    | "and" Value Value                   [strict]
    | "or" Value Value                    [strict]
    | "ifThenElse" Value Value Value      [strict]

  syntax ListExpField ::= ListValueField
  rule A:Id = E:Value , => field A E ,
  rule A:Id = E:Value , LEF:ListValueField => field A E , LEF
  syntax KResult ::= ListValueField

  syntax ListExp ::= Value
  rule E:Value , => cons E []Value
  rule E:Value , LE:Value => cons E LE
  syntax KResult ::= Type

  syntax Exp ::= Value
  rule foldl LE:Value 
    => lambda ?A:Id . lambda ?B:Id . lambda ?C:Id . 
      fold ?A ?B ?C
  rule (::) => lambda ?A:Id . lambda ?B:Id . cons ?A ?B
  rule (+) => lambda ?A:Id . lambda ?B:Id . add ?A ?B
  rule (-) => lambda ?A:Id . lambda ?B:Id . sub ?A ?B
  rule (*) => lambda ?A:Id . lambda ?B:Id . mult ?A ?B
  rule (/) 
    => lambda ?A:Id . lambda ?B:Id . 
      ifThenElse (eq ?B (intValue 0)) 
        (intValue 0)
        (div ?A ?B)
  rule (==) => lambda ?A:Id . lambda ?B:Id . eq ?A ?B
  rule (<) => lambda ?A:Id . lambda ?B:Id . less ?A ?B
  rule not
    => lambda ?A:Id . 
      ifThenElse ?A (boolValue false) (boolValue true)
  rule (&&)
    => lambda ?A:Id . lambda ?B:Id .
      ifThenElse ?A ?B (boolValue false)
  rule (||)
    => lambda ?A:Id . lambda ?B:Id .
      ifThenElse ?A (boolValue true) ?B
  rule E1:Exp |> E2:Exp => E2 E1 [macro]
  rule (E1:Exp >> E2:Exp) E3:Exp => E2 (E1 E3) [macro]
  rule if E1:Value then E2:Value else E3:Value => ifThenElse E1 E2 E3
  rule { LEF:ListValueField }Exp => { LEF }Value
  rule {}Exp => {}Value
  rule <k>{ A:Id | LEF1:ListValueField } 
    => setFields LEF2 LEF1
    ...</k>
    <tenv>... A |-> { LEF2:ListValueField }Value ...</tenv>
  rule <k> A1:Id get A2:Id
    => getField LTF A2
    ...</k>
    <tenv>... A1 |-> { LTF:ListValueField }Value ...</tenv>
  rule <k> let A:Id = E1:Value in E2:Exp
    => E2
    ...</k>
    <tenv> TEnv:Map => TEnv [ A <- E1 ] </tenv>
  //rule CaseOf
  //rule Call
  rule boolExp B:Bool => boolValue B
  rule intExp N:Int => intValue N
  rule []Exp => []Value
  rule [ LE:Value ]Exp => LE
  rule ( E1:Value , E2:Value )Exp => ( E1 , E2 )Value
  //rule \\ (varPattern A:Id) -> E:Value => lambda A . E
  rule <k> A:Id
    => E
    ...</k>
    <tenv>... A |-> E ...</tenv>
  rule (lambda A:Id . E:Value) E2:Value => E[E2 / A]

  syntax Value ::= "getField" ListValueField Id [function, functional]
  rule getField (field A1:Id E:Value , ) A2:Id
    => E
    //requires A1 ==K A2
  rule getField (field A1:Id E:Value , LEF:ListValueField) A2:Id
    => E
    requires A1 ==K A2
  rule getField (field A1:Id E:Value , LEF:ListValueField) A2:Id
    => getField LEF A2
    requires A1 =/=K A2

  syntax Value ::= "setFields" ListValueField ListValueField [function, functional]
  rule setFields LEF:ListValueField (field A:Id E:Value , )
    => { (setField LEF A E) }Value
  rule setFields LEF1:ListValueField (field A:Id E:Value , LEF2:ListValueField)
    => setFields (setField LEF1 A E) LEF2

  syntax ListValueField ::= "setField" ListValueField Id Value [function, functional]
  rule setField (field A1:Id E1:Value , ) A2:Id E2:Value
    => (field A1 E2 , )
    requires A1 ==K A2
  rule setField (field A1:Id E1:Value , LEF:ListValueField) A2:Id E2:Value
    => (field A1 E2 , LEF)
    requires A1 ==K A2
  rule setField (field A1:Id E1:Value , LEF:ListValueField) A2:Id E2:Value  
    => setField LEF A2 E2
    requires A1 =/=K A2

  syntax KVariable ::= Id
  syntax KResult ::= Value

  rule add (intValue N:Int) (intValue M:Int) => intValue (N +Int M)
  rule sub (intValue N:Int) (intValue M:Int) => intValue (N -Int M)
  rule mult (intValue N:Int) (intValue M:Int) => intValue (N *Int M)
  rule div (intValue N:Int) (intValue M:Int) => intValue (N /Int M)
  rule eq (intValue N:Int) (intValue M:Int) => true requires N ==Int M
  rule eq (intValue N:Int) (intValue M:Int) => true requires N =/=Int M
endmodule