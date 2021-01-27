module Data.Refinement exposing (Refinement(..), conjunction, deadEndsToString, decode, decoder, init, rename, substitute, toSMTStatement, toString, variables)

import Data.IntExp as IntExp exposing (IntExp(..))
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))
import Set exposing (Set)


type Refinement
    = IsTrue
    | IsFalse
    | IsSmaller String IntExp
    | IsBigger String IntExp
    | IsEqual String IntExp
    | EitherOr Refinement Refinement
    | AndAlso Refinement Refinement
    | IsNot Refinement


variables : Refinement -> Set String
variables refinement =
    case refinement of
        IsTrue ->
            Set.empty

        IsFalse ->
            Set.empty

        IsSmaller string intExp ->
            IntExp.variables intExp
                |> Set.insert string

        IsBigger string intExp ->
            IntExp.variables intExp
                |> Set.insert string

        IsEqual string intExp ->
            IntExp.variables intExp
                |> Set.insert string

        EitherOr r1 r2 ->
            Set.union (variables r1) (variables r2)

        AndAlso r1 r2 ->
            Set.union (variables r1) (variables r2)

        IsNot r ->
            variables r


init : List String -> List Refinement
init vars =
    (vars
        |> Set.fromList
        |> Set.toList
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


decoder : Parser Refinement
decoder =
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
            |= IntExp.decoder
        , Parser.succeed IsBigger
            |. Parser.keyword "(>)"
            |. Parser.spaces
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            |. Parser.spaces
            |= IntExp.decoder
        , Parser.succeed IsEqual
            |. Parser.keyword "(==)"
            |. Parser.spaces
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            |. Parser.spaces
            |= IntExp.decoder
        , Parser.succeed EitherOr
            |. Parser.keyword "or"
            |. Parser.spaces
            |= Parser.lazy (\() -> decoder)
            |. Parser.spaces
            |= Parser.lazy (\() -> decoder)
        , Parser.succeed AndAlso
            |. Parser.keyword "and"
            |. Parser.spaces
            |= Parser.lazy (\() -> decoder)
            |. Parser.spaces
            |= Parser.lazy (\() -> decoder)
        , Parser.succeed IsNot
            |. Parser.keyword "not"
            |. Parser.spaces
            |= Parser.lazy (\() -> decoder)
        , Parser.succeed identity
            |. Parser.symbol "("
            |= Parser.lazy (\() -> decoder)
            |. Parser.symbol ")"
        ]


decode : String -> Result String Refinement
decode =
    Parser.run
        decoder
        >> Result.mapError deadEndsToString


rename : { find : String, replaceWith : String } -> Refinement -> Refinement
rename ({ find, replaceWith } as theta) refinement =
    case refinement of
        IsTrue ->
            IsTrue

        IsFalse ->
            IsFalse

        IsSmaller string intExp ->
            IsSmaller
                (if string == find then
                    replaceWith

                 else
                    string
                )
                intExp

        IsBigger string intExp ->
            IsBigger
                (if string == find then
                    replaceWith

                 else
                    string
                )
                intExp

        IsEqual string intExp ->
            IsEqual
                (if string == find then
                    replaceWith

                 else
                    string
                )
                intExp

        EitherOr r1 r2 ->
            EitherOr
                (r1 |> rename theta)
                (r2 |> rename theta)

        AndAlso r1 r2 ->
            AndAlso
                (r1 |> rename theta)
                (r2 |> rename theta)

        IsNot r ->
            IsNot (r |> rename theta)


substitute : { find : String, replaceWith : IntExp } -> Refinement -> Refinement
substitute theta refinement =
    case refinement of
        IsTrue ->
            IsTrue

        IsFalse ->
            IsFalse

        IsSmaller string intExp ->
            IsSmaller
                string
                (intExp |> IntExp.substitute theta)

        IsBigger string intExp ->
            IsBigger
                string
                (intExp |> IntExp.substitute theta)

        IsEqual string intExp ->
            IsEqual
                string
                (intExp |> IntExp.substitute theta)

        EitherOr r1 r2 ->
            EitherOr
                (r1 |> substitute theta)
                (r2 |> substitute theta)

        AndAlso r1 r2 ->
            AndAlso
                (r1 |> substitute theta)
                (r2 |> substitute theta)

        IsNot r ->
            IsNot (r |> substitute theta)


toString : Refinement -> String
toString refinement =
    case refinement of
        IsTrue ->
            "True"

        IsFalse ->
            "False"

        IsSmaller string intExp ->
            "(<) " ++ string ++ " (" ++ IntExp.toString intExp ++ ")"

        IsBigger string intExp ->
            "(>) " ++ string ++ " (" ++ IntExp.toString intExp ++ ")"

        IsEqual string intExp ->
            "(==) " ++ string ++ " (" ++ IntExp.toString intExp ++ ")"

        EitherOr r1 r2 ->
            "Or (" ++ toString r1 ++ ") (" ++ toString r2 ++ ")"

        AndAlso r1 r2 ->
            "And (" ++ toString r1 ++ ") (" ++ toString r2 ++ ")"

        IsNot r ->
            "Not (" ++ toString r ++ ")"


toSMTStatement : Refinement -> String
toSMTStatement refinement =
    case refinement of
        IsTrue ->
            "true"

        IsFalse ->
            "false"

        IsSmaller string intExp ->
            "(< " ++ string ++ " " ++ IntExp.toSMTStatement intExp ++ ")"

        IsBigger string intExp ->
            "(> " ++ string ++ " " ++ IntExp.toSMTStatement intExp ++ ")"

        IsEqual string intExp ->
            "(= " ++ string ++ " " ++ IntExp.toSMTStatement intExp ++ ")"

        EitherOr r1 r2 ->
            "(or " ++ toSMTStatement r1 ++ " " ++ toSMTStatement r2 ++ ")"

        AndAlso r1 r2 ->
            "(and " ++ toSMTStatement r1 ++ " " ++ toSMTStatement r2 ++ ")"

        IsNot r ->
            "(not " ++ toSMTStatement r ++ ")"


conjunction : List Refinement -> Refinement
conjunction l =
    case l of
        [] ->
            IsTrue

        head :: tail ->
            tail
                |> List.foldl AndAlso
                    head



-----


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    String.concat
        (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"
