module Camperdown.Parse.Syntax exposing (Command, Config, Divert(..), Document, Element(..), Label(..), Mark(..), Markup, Parameter, Section, Text(..), Value(..))

{-| Placeholder

@docs Command, Config, Divert, Document, Element, Label, Mark, Markup, Parameter, Section, Text, Value

-}

import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Problem as Problem


{-| Labels are used to reference sections of a document.
Unlabeled sections will be marked with their line number.
-}
type Label
    = Named (Loc String)
    | Anonymous Int


{-| A document is a prelude followed by a list of sections
-}
type alias Document =
    { prelude : List Element
    , sections : List Section
    }


{-| A document is a bunch of passages, organized hierarchically and, some of which may be
gathered into labeled passages with the `#` section markers.
-}
type alias Section =
    { level : Int
    , contents : List Element
    , label : Label
    }


{-| Placeholder
-}
type Mark
    = Bang
    | Huh


{-| Placeholder
-}
type Element
    = Paragraph { contents : Markup }
    | Preformatted
        { lines : { start : Int, end : Int }
        , indent : Int
        , contents : String
        }
    | Item
        { lines : { start : Int, end : Int }
        , indent : Int
        , markLoc : Loc.Location
        , children : List Element
        }
    | Command
        { lines : { start : Int, end : Int }
        , mark : Loc Mark
        , command : Command
        , child : Maybe Divert
        , indent : Int
        }
    | Problem
        { lines : { start : Int, end : Int }
        , indent : Int
        , loc : Loc.Location
        , problem : String
        }


{-| Placeholder
-}
type Divert
    = Nested (List Element)
    | Immediate (List Element)
    | Reference (Loc String)


{-| Placeholder
-}
type alias Command =
    ( Maybe (Loc String), Config )


{-| Placeholder
-}
type alias Config =
    ( List (Loc Value), List (Loc Parameter) )


{-| Placeholder
-}
type alias Parameter =
    ( Loc String, List (Loc Value) )


{-| Placeholder
-}
type Value
    = Variable String
    | String String
    | Int Int
    | Markup Markup


{-| Placeholder
-}
type alias Markup =
    List Text


{-| Placeholder
-}
type Text
    = Raw String
    | Verbatim Char (Loc String)
    | Annotation (Loc String) (List Text) (Maybe (Loc String)) (Maybe (Loc Command))
    | InlineProblem Problem.Inline
