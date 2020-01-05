module ELM-SYNTAX
  syntax LowerVar
    ::= Id
  syntax UpperVar
    ::= Id
  syntax ListTypeFields
    ::= LowerVar ":" Type
      | LowerVar ":" Type "," ListTypeFields
  syntax ListType
    ::= Type
      | Type ListType
  syntax Type
    ::= "bool"
      | "int"
      | "list" Type
      | "(" Type "," Type ")"
      | "{}"
      | "{" ListTypeFields "}"
      | Type "->" Type                                [right]
      | UpperVar
      | UpperVar ListType
      | LowerVar
      | "(" Type ")"                                  [bracket]
  syntax ListPatternList
    ::= Pattern
      | Pattern "," ListPatternList
  syntax ListPatternSort
    ::= Pattern
      | Pattern ListPatternSort
  syntax ListPatternVars
    ::= LowerVar
      | LowerVar "," ListPatternVars
  syntax Pattern
    ::= Bool
      | Int
      | "[]"
      | "[" ListPatternList "]"
      | "(" Pattern "," Pattern ")"
      | UpperVar
      | UpperVar ListPatternSort
      | LowerVar
      | Pattern "as" LowerVar
      | "{}"
      | "{" ListPatternVars "}"
      | Pattern "::" Pattern                          [right,strict]
      | "_"
      | "(" Pattern ")"                               [bracket]
  syntax ListExpField
    ::= LowerVar "=" Exp
      | LowerVar "=" Exp "," ListExpField
  syntax ListCase
    ::= Pattern "->" Exp
      | Pattern "->" Exp ";" ListCase
  syntax ListExp
    ::= Exp
      | Exp "," ListExp
  syntax Exp
    ::= "foldl"
      | "(::)"
      | "(+)" | "(-)" | "(*)" | "(/)"
      | "(<)" | "(==)"
      | "not" | "(&&)" | "(||)"
      | Exp "|>" Exp                                  [left,strict]
      | Exp ">>" Exp                                  [left,strict]
      | "if" Exp "then" Exp "else" Exp
      | "{" ListExpField "}"
      | "{}"
      | "{" LowerVar "|" ListExpField "}"
      | LowerVar "." LowerVar                         [left,strict]
      | "let" LowerVar "=" Exp "in" Exp
      | "let" LowerVar ":" Type ";" LowerVar "=" Exp "in" Exp
      | "case" Exp "of" "[" ListCase "]"
      | Exp Exp                                       [left,strict]
      | Bool
      | Int
      | "[]"
      | "[" ListExp "]"
      | "(" Exp "," Exp ")"
      | "\\" Pattern "->" Exp
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