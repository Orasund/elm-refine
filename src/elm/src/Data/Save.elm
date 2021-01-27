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
            ( [ { refinement = "[k1]_{}", name = "a" }, { refinement = "[k2]_{}", name = "b" } ]
                |> Array.fromList
            , "[k3]_{}"
            )
      , bigger =
            ( [ { refinement = "True", name = "a" }, { refinement = "True", name = "b" } ]
                |> Array.fromList
            , "[k4]_{}"
            )
      , guards = [] |> Array.fromList
      , typeVariables = [] |> Array.fromList
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
            ( [ { refinement = "[k1]_{}", name = "z" } ]
                |> Array.fromList
            , "[k3]_{(a,(*) z -1),(b,z)}"
            )
      , bigger =
            ( [ { refinement = "True", name = "z" } ]
                |> Array.fromList
            , "[k4]_{}"
            )
      , guards = [] |> Array.fromList
      , typeVariables = [] |> Array.fromList
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
