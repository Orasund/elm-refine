# elm-refine
elm-refine will be type checker for Elm that introduces refinement types.
It is developed as part of a master thesis in computer mathematics.

# What are Refinement Types?

Let's look at the following example.

``` elm
{-| @refine \int -> int /= 0
-}
type alias IntWithoutZero =
    Int

dividedBy : IntWithoutZero -> Int -> Int
dividedBy a b = b // a
```

` 3 |> dividedBy 0` will normally return `0`, but using my type-checker it will now throw an error instead:

```
dividedBy expected the first element to be an IntWithoutZero
but was given an Int instead.

Hint:
I can't convert 0 to IntWithoutZero because 0 /= 0 is false.
```

These kinds of types are called refinement types. Liquid types are refinement types, where the refinement (`int /= 0`) can only contain logical operators (`not, and, or`), constants (like `0`), comparisons (`<=,<,=, /=`) and `+`. Also it only works for `Bool` and `Int`.

Such a type-checker does already exist for Haskell([LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell-blog/)) but its not very beginner friendly.