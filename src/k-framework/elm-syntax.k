module ELM-SYNTAX
  syntax LowerVar
    ::= Id
  syntax UpperVar
    ::= Id
  syntax ListTypeFields
    ::= LowerVar ":" Type ","                         [strict]
      | LowerVar ":" Type "," ListTypeFields          [strict]
  syntax ListType
    ::= Type
      | Type ListType                                 [strict]
  syntax Type
    ::= "bool"
      | "int"
      | "list" Type                                   [strict]
      | "(" Type "," Type ")"                         [strict]
      | "{}Type"
      | "{" ListTypeFields "}Type"                    [strict]
      | Type "->" Type                                [strict,right]
      | UpperVar
      | UpperVar ListType                             [strict]
      | LowerVar
      | "(" Type ")"                                  [bracket]
  syntax ListPatternList
    ::= Pattern
      | Pattern "," ListPatternList                   [strict]
  syntax ListPatternSort
    ::= Pattern
      | Pattern ListPatternSort                       [right,strict]
  syntax ListPatternVars
    ::= LowerVar
      | LowerVar "," ListPatternVars                  [strict]
  syntax Pattern
    ::= Bool
      | Int
      | "[]Pattern"
      | "[" ListPatternList "]Pattern"                [strict]
      | "(" Pattern "," Pattern ")"                   [strict]
      | UpperVar
      | UpperVar ListPatternSort                      [strict]
      | LowerVar
      | Pattern "as" LowerVar                         [strict]
      | "{}Pattern"
      | "{" ListPatternVars "}Pattern"                [strict]
      | Pattern "::" Pattern                          [right,strict]
      | "_Pattern"
      | "(" Pattern ")"                               [bracket]
  syntax ListExpField
    ::= LowerVar "=" Exp ","                          [strict]
      | LowerVar "=" Exp "," ListExpField             [strict]
  syntax ListCase
    ::= Pattern "->" Exp                              [strict]
      | Pattern "->" Exp ";" ListCase                 [strict]
  syntax ListExp
    ::= Exp ","                                       [strict]                                   
      | Exp "," ListExp                               [strict]
  syntax Exp
    ::= "foldl"
      | "(::)"
      | "(+)" | "(-)" | "(*)" | "(/)"
      | "(<)" | "(==)"
      | "not" | "(&&)" | "(||)"
      | Exp "|>" Exp                                  [left,strict]
      | Exp ">>" Exp                                  [left,strict]
      | "if" Exp "then" Exp "else" Exp                [strict]
      | "{" ListExpField "}Exp"                       [strict]
      | "{}Exp"
      | "{" LowerVar "|" ListExpField "}"             [strict]
      | LowerVar "get" LowerVar                       [left,strict]
      | "let" LowerVar "=" Exp "in" Exp               [strict(2)]
      | "let" LowerVar ":" Type ";" LowerVar "=" Exp "in" Exp  [strict]
      | "case" Exp "of" "[" ListCase "]"              [strict]
      | Exp Exp                                       [left,strict]
      | Bool
      | Int
      | "[]Exp"
      | "[" ListExp "]Exp"                            [strict]
      | "(" Exp "," Exp ")"                           [strict]
      | "\\" Pattern "->" Exp                         [strict]
      | UpperVar
      | LowerVar
      | "(" Exp ")"                                   [bracket]
  syntax ListStatementVar
    ::= LowerVar
      | LowerVar ListStatementVar
  syntax ListStatementSort
    ::= UpperVar ListType
      | UpperVar ListType "|" ListStatementSort
  syntax ListStatement
    ::= Statement ";"
      | Statement ";" ListStatement
  syntax Statement
    ::= LowerVar ":" Type ";" LowerVar "=" Exp
      | LowerVar "=" Exp
      | "type alias" UpperVar "=" Type
      | "type alias" UpperVar ListStatementVar "=" Type
      | "type" UpperVar "=" ListStatementSort
      | "type" UpperVar ListStatementVar "=" ListStatementSort
  syntax Program
    ::= "main =" Exp
      | "main :" Type ";" "main =" Exp
      | ListStatement "main =" Exp
      | ListStatement "main :" Type ";" "main =" Exp
endmodule