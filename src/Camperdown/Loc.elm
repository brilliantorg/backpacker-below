module Camperdown.Loc exposing
    ( Loc, Location, Position
    , end, locate, location, map, start, todoDummyLocate, todoDummyLocation, todoDummyPosition, value
    )

{-|


# Types

@docs Loc, Location, Position


# Functions

@docs end, locate, location, map, start, todoDummyLocate, todoDummyLocation, todoDummyPosition, value

-}


{-| -}
type alias Position =
    { line : Int
    , column : Int
    }


{-| -}
type alias Location =
    { start : Position
    , end : Position
    }


{-| -}
type alias Loc a =
    ( Location, a )


{-| -}
locate : Position -> Position -> a -> Loc a
locate startLoc endLoc x =
    ( { start = startLoc, end = endLoc }, x )


{-| -}
map : (a -> b) -> Loc a -> Loc b
map f ( l, x ) =
    ( l, f x )


{-| -}
location : Loc a -> Location
location ( l, _ ) =
    l


{-| -}
value : Loc a -> a
value ( _, x ) =
    x


{-| -}
start : Loc a -> Position
start ( l, _ ) =
    l.start


{-| -}
end : Loc a -> Position
end ( l, _ ) =
    l.end


{-| -}
todoDummyPosition : Position
todoDummyPosition =
    { line = -1, column = -1 }


{-| -}
todoDummyLocation : Location
todoDummyLocation =
    { start = todoDummyPosition, end = todoDummyPosition }


{-| -}
todoDummyLocate : a -> Loc a
todoDummyLocate =
    locate todoDummyPosition todoDummyPosition
