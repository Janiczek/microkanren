module Goals exposing (..)

import Dict exposing (Dict)
import Stream exposing (Stream(..))
import Vars exposing (..)


type alias State =
    { substMap : SubstMap
    , nextId : Int
    }


emptyState : State
emptyState =
    { substMap = Vars.emptySubstMap
    , nextId = 0
    }


type alias Goal =
    State -> Stream State


(===) : Value -> Value -> Goal
(===) a b =
    \state ->
        state.substMap
            |> unify a b
            |> Maybe.map
                (\substMap ->
                    Stream.singleton { state | substMap = Just substMap }
                )
            |> Maybe.withDefault Stream.empty


{-| "callFresh"
-}
with1 : (Value -> Goal) -> Goal
with1 goalConstructor =
    \state ->
        let
            goal =
                goalConstructor (VarValue (Var state.nextId))
        in
        goal { state | nextId = state.nextId + 1 }


with2 : (Value -> Value -> Goal) -> Goal
with2 goalConstructor =
    \state ->
        let
            goal =
                goalConstructor
                    (VarValue (Var state.nextId))
                    (VarValue (Var (state.nextId + 1)))
        in
        goal { state | nextId = state.nextId + 2 }


with3 : (Value -> Value -> Value -> Goal) -> Goal
with3 goalConstructor =
    \state ->
        let
            goal =
                goalConstructor
                    (VarValue (Var state.nextId))
                    (VarValue (Var (state.nextId + 1)))
                    (VarValue (Var (state.nextId + 2)))
        in
        goal { state | nextId = state.nextId + 3 }


with4 : (Value -> Value -> Value -> Value -> Goal) -> Goal
with4 goalConstructor =
    \state ->
        let
            goal =
                goalConstructor
                    (VarValue (Var state.nextId))
                    (VarValue (Var (state.nextId + 1)))
                    (VarValue (Var (state.nextId + 2)))
                    (VarValue (Var (state.nextId + 3)))
        in
        goal { state | nextId = state.nextId + 4 }


with5 : (Value -> Value -> Value -> Value -> Value -> Goal) -> Goal
with5 goalConstructor =
    \state ->
        let
            goal =
                goalConstructor
                    (VarValue (Var state.nextId))
                    (VarValue (Var (state.nextId + 1)))
                    (VarValue (Var (state.nextId + 2)))
                    (VarValue (Var (state.nextId + 3)))
                    (VarValue (Var (state.nextId + 4)))
        in
        goal { state | nextId = state.nextId + 5 }


or : Goal -> Goal -> Goal
or goal1 goal2 =
    \state ->
        Stream.merge (goal1 state) (goal2 state)


and : Goal -> Goal -> Goal
and goal1 goal2 =
    \state ->
        Stream.concatMap goal2 (goal1 state)


delay : Goal -> Goal
delay goal =
    \state ->
        ImmatureStream (\() -> goal state)


orMany : List Goal -> Goal
orMany goals =
    case goals of
        [] ->
            -- shouldn't happen, don't know what to put here
            always EmptyStream

        [ goal ] ->
            delay goal

        goal :: rest ->
            or (delay goal) (orMany rest)


andMany : List Goal -> Goal
andMany goals =
    case goals of
        [] ->
            -- shouldn't happen, don't know what to put here
            always EmptyStream

        [ goal ] ->
            delay goal

        goal :: rest ->
            and (delay goal) (andMany rest)


{-| The `conde` macro. Disjunction of conjunctions.

    (conde
      [(=== a 1) (=== b 2)]
      [(=== a 7) (=== b 12)})

Can be read as:

    (a == 1 && b == 2) || (a == 7 && b == 12)

-}
conditions : List (List Goal) -> Goal
conditions possibilities =
    possibilities
        |> List.map andMany
        |> orMany


run : Goal -> List (List ( Var, Value ))
run goal =
    goal emptyState
        |> Stream.toList
        |> List.filterMap .substMap
        |> List.map walk
        |> List.map Dict.toList
        |> List.map (List.map (\( varId, value ) -> ( Var varId, value )))


walk : Dict Int Value -> Dict Int Value
walk substMap =
    let
        wrappedSubstMap =
            Just substMap
    in
    substMap
        |> Dict.map
            (\_ value ->
                Vars.walk wrappedSubstMap value
                    |> Maybe.withDefault value
            )
