"reverse" : List "a" -> List "a" ;
"reverse" = foldl (::) [] ;

main : Int ;
main =
  case [1,2,3] |> "reverse" of
  [ "a" :: _ -> "a" ;
    _ -> -1
  ]