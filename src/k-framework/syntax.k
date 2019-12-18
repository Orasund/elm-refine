module ELMSYNTAX
  syntax ListTypeFields
    ::= ""
      | LowerVar ":" Type "," ListTypeFields
  syntax ListType
    ::= ""
      | Type ListType
  syntax Type
    ::= "Bool"
      | "Int"
      | "List" Type
      | "(" Type "," Type ")"
      | "{" ListTypeFields "}"
      | Type "->" Type
      | UpperVar ListType
      | LowerVar
      | "(" Type ")"              [bracket]
  syntax ListPatternList
    ::= ""
      | Pattern
      | ListPatternList
  syntax ListPatternSort

endmodule