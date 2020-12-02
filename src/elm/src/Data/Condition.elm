module Data.Condition exposing (Condition, ConditionForm, Input(..), IntExp(..), LiquidType, Refinement(..), SimpleLiquidType, decode)

import Dict
import Parser exposing (Parser)
import Result.Extra as Result


type IntExp
    = Integer Int
    | Plus IntExp IntExp
    | Times IntExp Int
    | Var String


intExpDecoder : Parser IntExp
intExpDecoder =
    Parser.oneOf
        [ Parser.map Integer int
        , Parser.succeed Plus
            |. Parser.keyword "(+)"
            |. Parser.spaces
            |= intExpDecoder
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed Times
            |. Parser.keyword "(*)"
            |. Parser.spaces
            |= intExpDecoder
            |. Parser.spaces
            |= Parser.int
        , Parser.map Var
            (Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            )
        ]


type Refinement
    = IsTrue
    | IsFalse
    | IsSmaller String IntExp
    | IsBigger String IntExp
    | IsEqual String IntExp
    | EitherOr Refinement Refinement
    | AndAlso Refinement Refinement
    | IsNot Refinement


refinementDecoder : Parser Refinement
refinementDecoder =
    Parser.oneOf
        [ Parser.map (\_ -> IsTrue) (keyword "True")
        , Parser.map (\_ -> IsFalse) (keyword "False")
        , Parser.succeed IsSmaller
            |. Parser.keyword "(<)"
            |. Parser.spaces
            |= Parser.map Var
                (Parser.variable
                    { start = Char.isLower
                    , inner = Char.isAlphaNum
                    , reserved = Set.fromList [ "or", "and", "not" ]
                    }
                )
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed IsBigger
            |. Parser.keyword "(>)"
            |. Parser.spaces
            |= Parser.map Var
                (Parser.variable
                    { start = Char.isLower
                    , inner = Char.isAlphaNum
                    , reserved = Set.fromList [ "or", "and", "not" ]
                    }
                )
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed IsEqual
            |. Parser.keyword "(==)"
            |. Parser.spaces
            |= Parser.map Var
                (Parser.variable
                    { start = Char.isLower
                    , inner = Char.isAlphaNum
                    , reserved = Set.fromList [ "or", "and", "not" ]
                    }
                )
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed EitherOr
            |. Parser.keyword "or"
            |. Parser.spaces
            |= refinementDecoder
            |. Parser.spaces
            |= refinementDecoder
        , Parser.succeed AndAlso
            |. Parser.keyword "and"
            |. Parser.spaces
            |= refinementDecoder
            |. Parser.spaces
            |= refinementDecoder
        , Parser.succeed IsNot
            |. Parser.keyword "not"
            |. Parser.spaces
            |= refinementDecoder
        ]


decodeRefinement : String -> Result String Refinement
decodeRefinement =
    refinementDecoder
        |> Parser.run


type SimpleLiquidType
    = Integer Refinement
    | Variable Int


type alias LiquidType =
    ( SimpleLiquidType, List SimpleLiquidType )


type alias Condition =
    { smaller : LiquidType
    , bigger : LiquidType
    , guards : List Refinement
    , typVariales : Dict String LiquidType
    }


type Input
    = IntegerInput Int
    | StringInput String


decodeSimpleLiquidType : Input -> Result String SimpleLiquidType
decodeSimpleLiquidType input =
    case input of
        IntegerInput n ->
            Ok <| Variable n

        StringInput string ->
            string
                |> decodeRefinement
                |> Result.map Integer


type alias ConditionForm =
    { smaller : ( Input, List Input )
    , bigger : ( Input, List Input )
    , guards : List String
    , typVariales : Dict String ( Input, List Input )
    }


decodeLiquidType : ( Input, List Input ) -> Result String LiquidType
decodeLiquidType ( head, list ) =
    Result.map2
        (\a b -> ( a, b ))
        (head |> decodeSimpleLiquidType)
        (list
            |> List.map decodeSimpleLiquidType
            |> Result.combine
        )


decode : ConditionForm -> Result String Condition
decode { smaller, bigger, guards, typVariables } =
    Result.map4 Condition
        (smaller |> decodeLiquidType)
        (bigger |> decodeLiquidType)
        (guards
            |> List.map decodeRefinement
            |> Result.combine
        )
        (typVariables
            |> Dict.map (always decodeLiquidType)
            |> Dict.toList
            |> Result.combine
            |> Result.map Dict.fromList
        )
