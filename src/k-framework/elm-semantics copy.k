require "unification.k"
require "elm-syntax.k"

module ELM-SEMANTICS
  imports UNIFICATION
  imports DOMAINS
  imports ELM-SYNTAX

  configuration <k> $PGM:Exp </k>
                <tenv> .Map </tenv>
  
  rule ((foldl F:Exp) E:Exp) []Exp => E
  rule ((foldl F:Exp) E:Exp) [ LE:ListExp ]Exp => fold F E LE
  rule ((::) E:Exp) L:Exp
    => cons E L
  rule ((+) (intExp N:Int)) (intExp M:Int)
    => intExp (N +Int M)
  rule ((-) (intExp N:Int)) (intExp M:Int)
    => intExp (N -Int M)
  rule ((*) (intExp N:Int)) (intExp M:Int)
    => intExp (N *Int M)
  rule ((/) (intExp N:Int)) (intExp 0) => intExp 0
  rule ((/) (intExp N:Int)) (intExp M:Int)
    => intExp (N /Int M)
  rule ((<) (intExp N:Int)) (intExp M:Int)
    => boolExp (N <Int M)
  rule ((==) (intExp N:Int)) (intExp M:Int)
    => boolExp (N ==Int M)
  rule not (boolExp B:Bool)
    => boolExp (notBool B)
  rule ((&&) (boolExp B1:Bool)) (boolExp B2:Bool)
    => B1 andBool B2
  rule ((||) (boolExp B1:Bool)) (boolExp B2:Bool)
    => B1 orBool B2
  rule E1:Exp |> E2:Exp => E2 E1 [macro]
  rule (E1:Exp >> E2:Exp) E3:Exp => E2 (E1 E3) [macro]
  rule if (boolExp True) then E1:Exp else E2:Exp => E1
  rule if (boolExp False) then E1:Exp else E2:Exp => E2 
  rule <k>{ A:Id | LEF1:ListExpField } 
    => setFields LEF2 LEF1
    ...</k>
    <tenv>... A |-> { LEF2:ListExpField }Exp ...</tenv>
  rule <k> A1:Id get A2:Id
    => getField LTF A2
    ...</k>
    <tenv>... A1 |-> { LTF:ListExpField }Exp ...</tenv>
  rule <k> let A:Id = E1:Exp in E2:Exp
    => E2
    ...</k>
    <tenv> TEnv:Map => TEnv [ A <- E1 ] </tenv>
  //rule CaseOf
  //rule Call
  rule <k> A:Id
    => E
    ...</k>
    <tenv>... A |-> E ...</tenv>

  syntax Exp ::= "getField" ListExpField Id [function, functional]
  rule getField (A1:Id = E:Exp , ) A2:Id
    => E
    requires A1 ==K A2
  rule getField (A1:Id = E:Exp , LEF:ListExpField) A2:Id
    => E
    requires A1 ==K A2
  rule getField (A1:Id = E:Exp , LEF:ListExpField) A2:Id
    => getField LEF A2
    requires A1 =/=K A2

  syntax Exp ::= "setFields" ListExpField ListExpField [function, functional]
  rule setFields LEF:ListExpField (A:Id = E:Exp , ) => setField LEF A E
  rule setFields LEF1:ListExpField (A:Id = E:Exp , LEF2:ListExpField)
    => setFields (setField LEF1 A E) LEF2

  syntax ListExpField ::= "setField" ListExpField Id Exp [function, functional]
  rule setField (A1:Id = E1:Exp , ) A2:Id E2:Exp
    => (A1 = E2 , )
    requires A1 ==K A2
  rule setField (A1:Id = E1:Exp , LEF:ListExpField) A2:Id E2:Exp
    => (A1 = E2 , LEF)
    requires A1 ==K A2
  rule setField (A1:Id = E1:Exp , LEF:ListExpField) A2:Id E2:Exp  
    => setField LEF A2 E2
    requires A1 =/=K A2
    
  syntax Exp ::= "cons" Exp Exp [function, functional]
  rule cons E1 []Exp => [ E1 , ]Exp
  rule cons E1 [ LE ]Exp => [ E1 , LE ]Exp

  syntax Exp ::= "fold" Exp Exp ListExp [function, functional]
  rule fold F E1 (E2:Exp , ) => (F E1) E2
  rule fold F E1 (E2:Exp , L2:ListExp) => fold F ((F E1) E2 ) L2

  rule intExp N:Int => N

  syntax KResult ::= Int

  //pattern matching
  syntax KItem ::= Exp "=Exp" Exp
  rule E =Exp E => .
endmodule