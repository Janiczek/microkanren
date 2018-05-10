module Vars exposing (..)

import Dict exposing (Dict)


type Var
    = Var Int


type Value
    = VarValue Var
    | NumValue Int


type alias SubstMap =
    -- Nothing if the goal is impossible
    -- Dict of Var IDs to values otherwise
    Maybe (Dict Int Value)


emptySubstMap : SubstMap
emptySubstMap =
    Just Dict.empty


addSubstitution : Var -> Value -> SubstMap -> SubstMap
addSubstitution (Var varId) value substMap =
    -- TODO handle infinite loops
    substMap
        |> Maybe.map (Dict.insert varId value)


walk : SubstMap -> Value -> Maybe Value
walk substMap value =
    case value of
        VarValue (Var varId) ->
            substMap
                |> Maybe.andThen
                    (\substMap_ ->
                        case Dict.get varId substMap_ of
                            Nothing ->
                                Just value

                            Just value2 ->
                                walk substMap value2
                    )

        NumValue num ->
            Just value


varValue : Value -> Maybe Var
varValue value =
    case value of
        VarValue var ->
            Just var

        _ ->
            Nothing


unify : Value -> Value -> SubstMap -> SubstMap
unify a b substMap =
    let
        walkedA : Maybe Value
        walkedA =
            walk substMap a

        walkedB : Maybe Value
        walkedB =
            walk substMap b

        isVar : Value -> Bool
        isVar value =
            case value of
                VarValue _ ->
                    True

                _ ->
                    False

        processVarValue : Value -> Value -> SubstMap
        processVarValue xVarValue yValue =
            -- one is a var, the other is a num value
            -- just add an substitution!
            xVarValue
                |> varValue
                |> Maybe.andThen (\xVar -> addSubstitution xVar yValue substMap)
    in
    Maybe.map2
        (\wA wB ->
            if wA == wB then
                -- Var 0 == Var 0
                -- 42 == 42
                -- etc.
                substMap
            else if isVar wA then
                processVarValue wA wB
            else if isVar wB then
                processVarValue wB wA
            else
                -- two things that are not var values,
                -- thus (currently) are NumValues,
                -- and from the first check we know they aren't equal
                -- they don't unify!
                Nothing
        )
        walkedA
        walkedB
        |> maybeJoin


maybeJoin : Maybe (Maybe a) -> Maybe a
maybeJoin maybeMaybe =
    case maybeMaybe of
        Just maybe ->
            maybe

        Nothing ->
            Nothing
