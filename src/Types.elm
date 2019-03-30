module Types exposing (RelativePosition, SortOrder, maxInt, minInt)


type RelativePosition a
    = Before a
    | Between a a
    | After a
    | Only


type SortOrder
    = Manual
    | LastPulled
    | LastModified
    | LastCreated
    | Random Int


minInt =
    -2 ^ 31


maxInt =
    2 ^ 31 - 1
