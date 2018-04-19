module Stream exposing (..)


type Stream a
    = EmptyStream
    | MatureStream a (Stream a)
    | ImmatureStream (() -> Stream a)


singleton : a -> Stream a
singleton value =
    MatureStream value empty


empty : Stream a
empty =
    EmptyStream


merge : Stream a -> Stream a -> Stream a
merge a b =
    case a of
        EmptyStream ->
            b

        MatureStream aVal aRest ->
            MatureStream aVal (merge aRest b)

        ImmatureStream thunk ->
            ImmatureStream (\() -> merge b (thunk ()))


{-| bind, andThen, >== ...
-}
concatMap : (a -> Stream b) -> Stream a -> Stream b
concatMap fn stream =
    case stream of
        EmptyStream ->
            EmptyStream

        MatureStream val rest ->
            merge (fn val) (concatMap fn rest)

        ImmatureStream thunk ->
            ImmatureStream (\() -> concatMap fn (thunk ()))


realizeHead : Stream a -> Stream a
realizeHead stream =
    case stream of
        EmptyStream ->
            stream

        MatureStream _ _ ->
            stream

        ImmatureStream thunk ->
            realizeHead (thunk ())


toList : Stream a -> List a
toList stream =
    case stream of
        EmptyStream ->
            []

        MatureStream val rest ->
            val :: toList rest

        ImmatureStream _ ->
            toList (realizeHead stream)
