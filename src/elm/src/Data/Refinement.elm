module Data.Refinement exposing (IntExp(..), Refinement(..), decode, init, toString)

import Parser exposing ((|.), (|=), Parser, Problem(..))
import Set


type IntExp
    = Integer Int
    | Plus IntExp IntExp
    | Times IntExp Int
    | Var String


type Refinement
    = IsTrue
    | IsFalse
    | IsSmaller String IntExp
    | IsBigger String IntExp
    | IsEqual String IntExp
    | EitherOr Refinement Refinement
    | AndAlso Refinement Refinement
    | IsNot Refinement


init : List String -> List Refinement
init vars =
    (vars
        |> List.concatMap
            (\v ->
                [ IsBigger "v" (Var v)
                , EitherOr (IsBigger "v" (Var v)) (IsEqual "v" (Var v))
                , IsSmaller "v" (Var v)
                , EitherOr (IsSmaller "v" (Var v)) (IsEqual "v" (Var v))
                , IsEqual "v" (Var v)
                , IsNot (IsEqual "v" (Var v))
                ]
            )
    )
        ++ [ IsBigger "v" (Integer 0)
           , EitherOr (IsBigger "v" (Integer 0)) (IsEqual "v" (Integer 0))
           , IsSmaller "v" (Integer 0)
           , EitherOr (IsSmaller "v" (Integer 0)) (IsEqual "v" (Integer 0))
           , IsEqual "v" (Integer 0)
           , IsNot (IsEqual "v" (Integer 0))
           ]


variableDecoder : Parser String
variableDecoder =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList [ "or", "and", "not" ]
        }


intExpDecoder : Parser IntExp
intExpDecoder =
    Parser.oneOf
        [ Parser.map Integer Parser.int
        , Parser.succeed Plus
            |. Parser.keyword "(+)"
            |. Parser.spaces
            |= Parser.lazy (\() -> intExpDecoder)
            |. Parser.spaces
            |= Parser.lazy (\() -> intExpDecoder)
        , Parser.succeed Times
            |. Parser.keyword "(*)"
            |. Parser.spaces
            |= Parser.lazy (\() -> intExpDecoder)
            |. Parser.spaces
            |= Parser.int
        , Parser.map Var variableDecoder
        , Parser.succeed identity
            |. Parser.symbol "("
            |= Parser.lazy (\() -> intExpDecoder)
            |. Parser.symbol ")"
        ]


decode : Parser Refinement
decode =
    Parser.oneOf
        [ Parser.map (\_ -> IsTrue) (Parser.keyword "True")
        , Parser.map (\_ -> IsFalse) (Parser.keyword "False")
        , Parser.succeed IsSmaller
            |. Parser.keyword "(<)"
            |. Parser.spaces
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed IsBigger
            |. Parser.keyword "(>)"
            |. Parser.spaces
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed IsEqual
            |. Parser.keyword "(==)"
            |. Parser.spaces
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed EitherOr
            |. Parser.keyword "or"
            |. Parser.spaces
            |= Parser.lazy (\() -> decode)
            |. Parser.spaces
            |= Parser.lazy (\() -> decode)
        , Parser.succeed AndAlso
            |. Parser.keyword "and"
            |. Parser.spaces
            |= Parser.lazy (\() -> decode)
            |. Parser.spaces
            |= Parser.lazy (\() -> decode)
        , Parser.succeed IsNot
            |. Parser.keyword "not"
            |. Parser.spaces
            |= Parser.lazy (\() -> decode)
        , Parser.succeed identity
            |. Parser.symbol "("
            |= Parser.lazy (\() -> decode)
            |. Parser.symbol ")"
        ]


intExpToString : IntExp -> String
intExpToString input =
    case input of
        Integer int ->
            String.fromInt int

        Plus intExp1 intExp2 ->
            "(+) " ++ intExpToString intExp1 ++ " " ++ intExpToString intExp2

        Times intExp i ->
            "(*) " ++ intExpToString intExp ++ " " ++ String.fromInt i

        Var string ->
            string


toString : Refinement -> String
toString refinement =
    case refinement of
        IsTrue ->
            "True"

        IsFalse ->
            "False"

        IsSmaller string intExp ->
            "(<) " ++ string ++ " " ++ intExpToString intExp

        IsBigger string intExp ->
            "(>) " ++ string ++ " " ++ intExpToString intExp

        IsEqual string intExp ->
            "(==) " ++ string ++ " " ++ intExpToString intExp

        EitherOr r1 r2 ->
            "Or (" ++ toString r1 ++ ") (" ++ toString r2 ++ ")"

        AndAlso r1 r2 ->
            "And (" ++ toString r1 ++ ") (" ++ toString r2 ++ ")"

        IsNot r ->
            "Not (" ++ toString r ++ ")"
