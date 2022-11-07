module Yaml.Parser.Ast exposing (Property, Value(..), fold, fromString, map, toString)

import Dict exposing (Dict)



-- AST


{-| -}
type Value
    = String_ String
    | Float_ Float
    | Int_ Int
    | List_ (List Value)
    | Record_ (Dict String Value)
    | Bool_ Bool
    | Null_
    | Anchor_ String Value
    | Alias_ String


{-| -}
type alias Property =
    ( String, Value )


{-| -}
fromString : String -> Value
fromString string =
    let
        trimmed =
            String.trim string
    in
    case String.toLower trimmed of
        "" ->
            Null_

        "null" ->
            Null_

        "true" ->
            Bool_ True

        "false" ->
            Bool_ False

        ".nan" ->
            Float_ (0 / 0)

        ".inf" ->
            Float_ (1 / 0)

        "+.inf" ->
            Float_ (1 / 0)

        "-.inf" ->
            Float_ (-1 / 0)

        other ->
            case String.toInt other of
                Just int ->
                    Int_ int

                Nothing ->
                    case String.toFloat other of
                        Just float ->
                            Float_ float

                        Nothing ->
                            String_ trimmed



-- DISPLAY


{-| -}
toString : Value -> String
toString value =
    case value of
        String_ string ->
            "\"" ++ string ++ "\""

        Float_ float ->
            String.fromFloat float ++ " (float)"

        Int_ int ->
            String.fromInt int ++ " (int)"

        List_ list ->
            "[ " ++ String.join ", " (List.map toString list) ++ " ]"

        Record_ properties ->
            "{ " ++ String.join ", " (List.map toStringProperty (Dict.toList properties)) ++ " }"

        Bool_ True ->
            "True (bool)"

        Bool_ False ->
            "False (bool)"

        Null_ ->
            "Null"

        Anchor_ name r_val ->
            "&" ++ name ++ " " ++ toString r_val

        Alias_ name ->
            "*" ++ name


toStringProperty : Property -> String
toStringProperty ( name, value ) =
    name ++ ": " ++ toString value


fold : (Value -> b -> b) -> Value -> b -> b
fold f value z =
    case value of
        String_ _ ->
            f value z

        Float_ _ ->
            f value z

        Int_ _ ->
            f value z

        Bool_ _ ->
            f value z

        Null_ ->
            f value z

        Alias_ _ ->
            f value z

        List_ l ->
            f value (List.foldl (fold f) z l)

        Record_ r ->
            f value (List.foldl (fold f) z (Dict.values r))

        Anchor_ nm a ->
            f value (fold f a z)


map : (Value -> Value) -> Value -> Value
map f value =
    case value of
        String_ _ ->
            f value

        Float_ _ ->
            f value

        Int_ _ ->
            f value

        Bool_ _ ->
            f value

        Null_ ->
            f value

        Alias_ _ ->
            f value

        List_ l ->
            f (List_ (List.map (map f) l))

        Record_ r ->
            f (Record_ <| Dict.fromList (List.map (\( k, v ) -> ( k, map f v )) (Dict.toList r)))

        Anchor_ name a ->
            f (Anchor_ name (map f a))
