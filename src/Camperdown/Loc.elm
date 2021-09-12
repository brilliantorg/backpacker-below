module Camperdown.Loc exposing
    ( Loc
    , Location
    , Position
    , end
    , locate
    , location
    , map
    , start
    , todoDummyLocate
    , todoDummyLocation
    , todoDummyPosition
    , value
    , encodeLocation
    )

{-|


# Types

@docs Loc
@docs Location
@docs Position


# Functions

@docs end
@docs locate
@docs location
@docs map
@docs start
@docs todoDummyLocate
@docs todoDummyLocation
@docs todoDummyPosition
@docs value
@docs encodeLocation

-}

import Json.Encode as Encode


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


{-| Encodes a Location as a JSON value.
-}
encodeLocation : Location -> Encode.Value
encodeLocation loc =
    Encode.object
        [ ( "start"
          , Encode.object
                [ ( "line", Encode.int loc.start.line )
                , ( "column", Encode.int loc.start.column )
                ]
          )
        , ( "end"
          , Encode.object
                [ ( "line", Encode.int loc.end.line )
                , ( "column", Encode.int loc.end.column )
                ]
          )
        ]
