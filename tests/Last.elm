module Last exposing (last)

import Array


last : List a -> Maybe.Maybe a
last xs =
    List.head (List.reverse xs)
