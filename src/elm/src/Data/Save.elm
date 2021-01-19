module Data.Save exposing (load, max)

import Array
import Data.Condition exposing (ConditionForm)


max : List ConditionForm
max =
    [ { smaller = ( Array.empty, "(==) v b" )
      , bigger = ( Array.empty, "[k3]_{}" )
      , guards = [ "(<) a b" ] |> Array.fromList
      , typeVariables = [ ( "a", "True" ), ( "b", "True" ) ] |> Array.fromList
      }
    , { smaller = ( Array.empty, "(==) v a" )
      , bigger = ( Array.empty, "[k3]_{}" )
      , guards = [ "not ((<) a b)" ] |> Array.fromList
      , typeVariables = [ ( "a", "True" ), ( "b", "True" ) ] |> Array.fromList
      }
    , { smaller =
            ( [ { baseType = "[k1]_{}", name = "a" }, { baseType = "[k2]_{}", name = "b" } ]
                |> Array.fromList
            , "[k3]_{}"
            )
      , bigger =
            ( [ { baseType = "True", name = "a" }, { baseType = "True", name = "b" } ]
                |> Array.fromList
            , "[k4]_{}"
            )
      , guards = [ "not ((<) a b)" ] |> Array.fromList
      , typeVariables = [ ( "a", "True" ), ( "b", "True" ) ] |> Array.fromList
      }
    ]


abs : List ConditionForm
abs =
    [ { smaller = ( Array.empty, "(==) v b" )
      , bigger = ( Array.empty, "[k3]_{}" )
      , guards = [ "(<) a b" ] |> Array.fromList
      , typeVariables = [ ( "a", "True" ), ( "b", "True" ) ] |> Array.fromList
      }
    , { smaller = ( Array.empty, "(==) v a" )
      , bigger = ( Array.empty, "[k3]_{}" )
      , guards = [ "not ((<) a b)" ] |> Array.fromList
      , typeVariables = [ ( "a", "True" ), ( "b", "True" ) ] |> Array.fromList
      }
    , { smaller =
            ( [ { baseType = "[k1]_{}", name = "z" } ]
                |> Array.fromList
            , "[k3]_{(a,(*) z -1),(b,z)}"
            )
      , bigger =
            ( [ { baseType = "True", name = "z" } ]
                |> Array.fromList
            , "[k4]_{}"
            )
      , guards = [ "not ((<) a b)" ] |> Array.fromList
      , typeVariables = [ ( "a", "True" ), ( "b", "True" ) ] |> Array.fromList
      }
    ]


load : Int -> List ConditionForm
load int =
    case int of
        1 ->
            max

        2 ->
            abs

        _ ->
            []
