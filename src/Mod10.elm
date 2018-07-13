module Mod10 exposing (calculate, verify)

{-| This library helps you create and verify numeric strings according to the Modulus10 algorithm.


# Definition

@docs Mod10


# Verify

@docs verify


# Create

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


calculateHelp : List Int -> Maybe Char
calculateHelp ints =
    List.indexedMap applyWeight (0 :: ints)
        |> List.sum
        |> (*) 9
        |> modBy 10
        |> toChar


{-| Verfify a numeric string with the Modulus 10 algorithm.

    verify "79927398713" == True
    verify "79927398712" == False
    verify "nonsense" == False

-}
verify : String -> Bool
verify str =
    calculate (String.dropRight 1 str)
        == List.head (String.toList (String.right 1 str))


toChar : Int -> Maybe Char
toChar int =
    case int of
        0 ->
            Just '0'

        1 ->
            Just '1'

        2 ->
            Just '2'

        3 ->
            Just '3'

        4 ->
            Just '4'

        5 ->
            Just '5'

        6 ->
            Just '6'

        7 ->
            Just '7'

        8 ->
            Just '8'

        9 ->
            Just '9'

        _ ->
            Nothing


applyWeight : Int -> Int -> Int
applyWeight index int =
    if modBy 2 index == 0 then
        int
    else if int > 4 then
        (int * 2) - 9
    else
        int * 2
