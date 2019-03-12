module Mod10 exposing
    ( Error(..)
    , hasValidCheckDigit
    , calculateCheckDigit
    )

{-|


# Problems

@docs Error


# Validating

@docs hasValidCheckDigit


# Constructing

@docs calculateCheckDigit

-}


{-| The Modulus 10 algorithm works on sequences of digits, but since boths lists and integers
can represent values that are invalid in this context there are some cases that will result in
errors.
-}
type Error
    = NumbersOutOfRange
    | EmptySequence


{-| Check if the last digit in a sequence of digits is a valid check
digit for the sequence according to the Modulus 10 algorithm.

    hasValidCheckDigit [ 1, 2, 3 ] == Ok False

    hasValidCheckDigit [ -2, 3 4 ] == Err NumbersOutOfRange

    hasValidCheckDigit [] == Err EmptySequence

-}
hasValidCheckDigit : List Int -> Result Error Bool
hasValidCheckDigit numbers =
    case List.reverse numbers of
        [] ->
            Err EmptySequence

        head :: tail ->
            calculateCheckDigit (List.reverse tail)
                |> Result.map ((==) head)


{-| calculateCheckDigit the check digit for a given sequence of digits.
-}
calculateCheckDigit : List Int -> Result Error Int
calculateCheckDigit numbers =
    if List.isEmpty numbers then
        Err EmptySequence

    else if List.all isSingleDigit numbers then
        List.reverse numbers
            |> List.indexedMap applyWeight
            |> List.sum
            |> toCheckDigit
            |> Ok

    else
        Err NumbersOutOfRange


isSingleDigit : Int -> Bool
isSingleDigit n =
    n >= 0 && n <= 9


toCheckDigit : Int -> Int
toCheckDigit num =
    case remainderBy 10 num of
        0 ->
            0

        n ->
            10 - n


applyWeight : Int -> Int -> Int
applyWeight index num =
    case ( modBy 2 index == 0, num > 4 ) of
        ( False, _ ) ->
            num

        ( True, False ) ->
            num * 2

        ( True, True ) ->
            (num * 2) - 9
