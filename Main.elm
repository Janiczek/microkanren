module Main exposing (main)

import Goals exposing ((===), Goal)
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
                [ a === b
                , Goals.and
                    (a === Vars.NumValue 8)
                    (b === Vars.NumValue 9)
                , Goals.and
                    (a === b)
                    (b === Vars.NumValue 42)
                ]
        )


viewPossibility : Int -> List ( Var, Value ) -> Html msg
viewPossibility index possibility =
    H.li []
        [ H.text ("Possibility " ++ toString (index + 1))
        , H.ul []
            (possibility
                |> List.map
                    (\( Var varId, value ) ->
                        H.li []
                            [ H.text <|
                                "var "
                                    ++ toString varId
                                    ++ " = "
                                    ++ (case value of
                                            VarValue (Var varId2) ->
                                                "var " ++ toString varId2

                                            NumValue num ->
                                                toString num
                                       )
                            ]
                    )
            )
        ]
