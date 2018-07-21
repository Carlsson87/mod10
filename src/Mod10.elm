module Mod10 exposing (calculate, verify)

{-| This library helps you create and verify numeric strings according to the Modulus 10 algorithm.


# Verify

@docs verify


# Calculate

@docs calculate

-}


{-| Calculate the "check digit" for a numeric string.

    calculate "7992739871" == Just '3'
    calculate "nonsense" == Nothing

-}
calculate : String -> Maybe Char
calculate str =
    String.split "" str
        |> List.map String.toInt
        |> List.foldl (Maybe.map2 (::)) (Just [])
        |> Maybe.andThen calculateHelp


{-| Verfify a numeric string with the Modulus 10 algorithm.

    verify "79927398713" == True
    verify "79927398712" == False
    verify "nonsense" == False

-}
verify : String -> Bool
verify str =
    Maybe.map2 (==)
        (calculate (String.dropRight 1 str))
        (List.head (String.toList (String.right 1 str)))
        |> Maybe.withDefault False


calculateHelp : List Int -> Maybe Char
calculateHelp ints =
    List.indexedMap applyWeight (0 :: ints)
        |> List.sum
        |> (*) 9
        |> modBy 10
        |> String.fromInt
        |> String.uncons
        |> Maybe.map Tuple.first


applyWeight : Int -> Int -> Int
applyWeight index int =
    if modBy 2 index == 0 then
        int
    else if int > 4 then
        (int * 2) - 9
    else
        int * 2
