module Types exposing (RelativePosition, SortOrder)


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
