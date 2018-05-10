module Main exposing (main)

import Goals exposing (Goal, equals)
import Html as H exposing (Html)
import Vars exposing (Value(..), Var(..))


main : Html msg
main =
    H.ul []
        (Goals.run conditions
            |> List.indexedMap viewPossibility
        )


conditions : Goal
conditions =
    Goals.with2
        (\a b ->
            Goals.orMany
                [ equals a b
                , Goals.and
                    (equals a (Vars.NumValue 8))
                    (equals b (Vars.NumValue 9))
                , Goals.and
                    (equals a b)
                    (equals b (Vars.NumValue 42))
                ]
        )


viewPossibility : Int -> List ( Var, Value ) -> Html msg
viewPossibility index possibility =
    H.li []
        [ H.text ("Possibility " ++ String.fromInt (index + 1))
        , H.ul []
            (possibility
                |> List.map
                    (\( Var varId, value ) ->
                        H.li []
                            [ H.text <|
                                "var "
                                    ++ String.fromInt varId
                                    ++ " = "
                                    ++ (case value of
                                            VarValue (Var varId2) ->
                                                "var " ++ String.fromInt varId2

                                            NumValue num ->
                                                String.fromInt num
                                       )
                            ]
                    )
            )
        ]
