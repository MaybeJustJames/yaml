module Format exposing (formatFloat, formatFloatOutput)


formatFloat : Float -> String
formatFloat n =
    let
        formatted =
            formatFloatOutput n
    in
    if String.contains "." formatted then
        formatted

    else
        formatted ++ ".0"


formatFloatOutput : Float -> String
formatFloatOutput n =
    if isInfinite n then
        if n < 0 then
            "-.inf"

        else
            ".inf"

    else if isNaN n then
        ".nan"

    else
        String.fromFloat n
