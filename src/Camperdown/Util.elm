module Camperdown.Util exposing (Parser, loop, offsetIndexedMap, oneIndexedMap, return)

import Parser.Advanced as Parser


offsetIndexedMap : Int -> (Int -> a -> b) -> List a -> List b
offsetIndexedMap k f =
    List.indexedMap (\n x -> f (n + k) x)


oneIndexedMap : (Int -> a -> b) -> List a -> List b
oneIndexedMap =
    offsetIndexedMap 1


{-| Type of a parser where failure's not an option.
-}
type alias Parser value =
    Parser.Parser Never () value


loop : a -> Parser (Parser.Step a b)
loop =
    Parser.succeed << Parser.Loop


return : b -> Parser (Parser.Step a b)
return =
    Parser.succeed << Parser.Done
