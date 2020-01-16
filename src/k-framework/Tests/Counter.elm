let
  model = { counter = intExp 0 , }Exp
in
{ model | counter = model get counter |> (+) (intExp 1) , }