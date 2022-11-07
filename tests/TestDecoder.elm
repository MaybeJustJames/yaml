module TestDecoder exposing (suite)

import Dict
import Expect
import Expectations exposing (expectCloseTo, expectFail, toExpect)
import Format exposing (formatFloat)
import Fuzz exposing (bool, float, int, list, string)
import Test
import Yaml.Decode as Yaml
import Yaml.Parser.Util exposing (postProcessString)


sanitiseString : String -> String
sanitiseString s =
    let
        replaceChar : Char -> Char
        replaceChar c =
            if List.member c [ '[', ']', '{', '}', '\'', '"', '#', ':', '-', 'Y', 'y', 'N', 'n' ] then
                ' '

            else
                case String.toInt <| String.fromChar c of
                    Just _ ->
                        ' '

                    _ ->
                        c
    in
    String.map replaceChar s
        |> String.filter ((/=) '\n')


quoteString : String -> String
quoteString s =
    "'" ++ s ++ "'"


suite : Test.Test
suite =
    Test.describe "Decoding"
        [ Test.describe "String values"
            [ Test.test "unquoted string" <|
                \_ -> given "string" Yaml.string |> Expect.equal (Ok "string")
            , Test.test "single-quoted string" <|
                \_ -> given "'string'" Yaml.string |> Expect.equal (Ok "string")
            , Test.test "double-quoted string" <|
                \_ -> given "\"string\"" Yaml.string |> Expect.equal (Ok "string")
            , Test.test "quoted number" <|
                \_ -> given "'5'" Yaml.string |> Expect.equal (Ok "5")
            , Test.test "unquoted number" <|
                \_ -> given "0" Yaml.string |> expectFail "Expected string, got: 0 (int)"
            , Test.test "unquoted bool" <|
                \_ -> given "true" Yaml.string |> expectFail "Expected string, got: True (bool)"
            , Test.fuzz (Fuzz.map sanitiseString string) "random string" <|
                \s -> given s Yaml.string |> Expect.equal (Ok (String.trim s))
            , Test.test "string continaing a colon with no trailing space" <|
                \_ -> given "a:b" Yaml.string |> Expect.equal (Ok "a:b")
            ]
        , Test.describe "boolean values"
            [ Test.test "boolean true" <|
                \_ -> given "true" Yaml.bool |> Expect.equal (Ok True)
            , Test.test "boolean True" <|
                \_ -> given "True" Yaml.bool |> Expect.equal (Ok True)
            , Test.test "boolean TRUE" <|
                \_ -> given "TRUE" Yaml.bool |> Expect.equal (Ok True)
            , Test.test "boolean false" <|
                \_ -> given "false" Yaml.bool |> Expect.equal (Ok False)
            , Test.test "boolean False" <|
                \_ -> given "False" Yaml.bool |> Expect.equal (Ok False)
            , Test.test "boolean FALSE" <|
                \_ -> given "FALSE" Yaml.bool |> Expect.equal (Ok False)
            , Test.test "empty" <|
                \_ -> given "" Yaml.bool |> expectFail "Expected bool, got: Null"
            , Test.test "non-boolean string" <|
                \_ -> given "rubbish" Yaml.bool |> expectFail "Expected bool, got: \"rubbish\""
            , Test.test "non-boolean number" <|
                \_ -> given "3" Yaml.bool |> expectFail "Expected bool, got: 3 (int)"
            ]
        , Test.describe "numeric values"
            [ Test.fuzz int "integers" <|
                \x -> given (String.fromInt x) Yaml.int |> Expect.equal (Ok x)
            , Test.test "float as integer" <|
                \_ -> given "2.1" Yaml.int |> expectFail "Expected int, got: 2.1 (float)"
            , Test.test "rubbish as integer" <|
                \_ -> given "rubbish" Yaml.int |> expectFail "Expected int, got: \"rubbish\""
            , Test.test "empty string as integer" <|
                \_ -> given "" Yaml.int |> expectFail "Expected int, got: Null"
            , Test.fuzz float "floats" <|
                \x ->
                    Yaml.fromString Yaml.float (formatFloat x)
                        |> Result.map (expectCloseTo x)
                        |> toExpect
            , Test.test "integer as float" <|
                \_ -> given "0" Yaml.float |> Expect.equal (Ok 0.0)
            , Test.test "rubbish as float" <|
                \_ -> given "rubbish" Yaml.float |> expectFail "Expected float, got: \"rubbish\""
            , Test.test "Empty string as float" <|
                \_ -> given "" Yaml.float |> expectFail "Expected float, got: Null"
            ]
        , Test.describe "null and nullable"
            [ Test.test "empty string as null" <|
                \_ -> given "" Yaml.null |> Expect.equal (Ok Maybe.Nothing)
            , Test.test "whitespace as null" <|
                \_ -> given "  " Yaml.null |> Expect.equal (Ok Maybe.Nothing)
            , Test.test "null as null" <|
                \_ -> given " null " Yaml.null |> Expect.equal (Ok Maybe.Nothing)
            , Test.test "non-empty string as null" <|
                \_ -> given "str" Yaml.null |> expectFail "Expected null, got: \"str\""
            , Test.test "nullable string" <|
                \_ -> given "" (Yaml.nullable Yaml.string) |> Expect.equal (Ok Maybe.Nothing)
            , Test.test "nullable bool" <|
                \_ -> given "" (Yaml.nullable Yaml.bool) |> Expect.equal (Ok Maybe.Nothing)
            , Test.test "nullable int" <|
                \_ -> given "" (Yaml.nullable Yaml.int) |> Expect.equal (Ok Maybe.Nothing)
            , Test.test "nullable float" <|
                \_ -> given "" (Yaml.nullable Yaml.float) |> Expect.equal (Ok Maybe.Nothing)
            ]
        , Test.describe "record primitives"
            [ Test.test "access first existing field" <|
                \_ -> given "hello: 5\nworld:6" (Yaml.field "hello" Yaml.int) |> Expect.equal (Ok 5)
            , Test.test "access second existing field" <|
                \_ -> given "hello: 5\nworld:6" (Yaml.field "world" Yaml.int) |> Expect.equal (Ok 6)
            , Test.test "access a field that does not exist" <|
                \_ -> given "hello: 5\nworld:6" (Yaml.field "fake" Yaml.int) |> expectFail "Expected property: fake"
            , Test.test "access an existing nested field" <|
                \_ -> given "hello:\n  world: 2" (Yaml.at [ "hello", "world" ] Yaml.int) |> Expect.equal (Ok 2)
            , Test.test "access a nested field that does not exist" <|
                \_ ->
                    given "hello:\n  world: 2"
                        (Yaml.at [ "hello", "world", "foo" ] Yaml.int)
                        |> expectFail "Expected record"
            ]
        , Test.describe "inconsistent structure"
            [ Test.test "try 2 fields where the first exists" <|
                \_ ->
                    given "  aaa: 0"
                        (Yaml.oneOf
                            [ Yaml.field "aaa" Yaml.int
                            , Yaml.field "bbb" Yaml.int
                            ]
                        )
                        |> Expect.equal (Ok 0)
            , Test.test "try 2 fields where the second exists" <|
                \_ ->
                    given "zzz: 2\nbbb: 0"
                        (Yaml.oneOf
                            [ Yaml.field "aaa" Yaml.int
                            , Yaml.field "bbb" Yaml.int
                            ]
                        )
                        |> Expect.equal (Ok 0)
            , Test.test "try 2 fields where both exist" <|
                \_ ->
                    given "  aaa: 0\n  bbb: 1  "
                        (Yaml.oneOf
                            [ Yaml.field "aaa" Yaml.int
                            , Yaml.field "bbb" Yaml.int
                            ]
                        )
                        |> Expect.equal (Ok 0)
            , Test.test "try 2 fields where neither exist" <|
                \_ ->
                    given "  aaa: 1\n  bbb: 2  \n"
                        (Yaml.oneOf
                            [ Yaml.field "ddd" Yaml.int
                            , Yaml.field "eee" Yaml.int
                            ]
                        )
                        |> expectFail "Empty"
            , Test.test "try one of several decoders" <|
                \_ ->
                    given ""
                        (Yaml.oneOf
                            [ Yaml.succeed "a", Yaml.succeed "b", Yaml.succeed "c" ]
                        )
                        |> Expect.equal (Ok "a")
            , Test.test "try one of several decoders, all of which fail" <|
                \_ ->
                    given "hello"
                        (Yaml.oneOf
                            [ Yaml.fail "no", Yaml.fail "nope", Yaml.fail "no way" ]
                        )
                        |> expectFail "Empty"
            ]
        , Test.describe "lists"
            [ Test.test "empty list" <|
                \_ -> given "[]" (Yaml.list Yaml.null) |> Expect.equal (Ok [])
            , Test.test "decode nothing into an empty list" <|
                \_ -> given "" (Yaml.list Yaml.int) |> Expect.equal (Ok [])
            , Test.fuzz (list int) "list of integers" <|
                \xs ->
                    let
                        strList : String
                        strList =
                            "[" ++ String.join "," (List.map String.fromInt xs) ++ "]"
                    in
                    given
                        strList
                        (Yaml.list Yaml.int)
                        |> Expect.equal (Ok xs)
            , Test.fuzz (list string) "list of strings" <|
                \xs ->
                    let
                        sanitised =
                            List.map sanitiseString xs

                        strList : String
                        strList =
                            "["
                                ++ String.join ", "
                                    (List.map quoteString sanitised)
                                ++ "]"
                    in
                    given strList (Yaml.list Yaml.string)
                        |> Expect.equal
                            (Ok
                                (List.map
                                    (postProcessString << String.replace "\\" "\\\\")
                                    sanitised
                                )
                            )
            , Test.fuzz (list bool) "list of boolean values" <|
                \xs ->
                    let
                        strList : String
                        strList =
                            "["
                                ++ String.join ", "
                                    (List.map
                                        (\b ->
                                            if b then
                                                "true"

                                            else
                                                "false"
                                        )
                                        xs
                                    )
                                ++ "]"
                    in
                    given strList (Yaml.list Yaml.bool) |> Expect.equal (Ok xs)
            , Test.test "multiline list" <|
                \_ ->
                    let
                        strList =
                            """
                            - hello
                            - world
                            - foo
                            - bar
                            """
                    in
                    given strList (Yaml.list Yaml.string)
                        |> Expect.equal (Ok [ "hello", "world", "foo", "bar" ])
            ]
        , Test.describe "dictionaries"
            [ Test.test "inline record" <|
                \_ ->
                    given "{a: 1}" (Yaml.dict Yaml.int)
                        |> Expect.equal (Ok (Dict.singleton "a" 1))
            , Test.test "inline record with multiple values" <|
                \_ ->
                    given "{aaa: hello, bbb: world}" (Yaml.dict Yaml.string)
                        |> Expect.equal
                            (Ok
                                (Dict.fromList [ ( "aaa", "hello" ), ( "bbb", "world" ) ])
                            )
            , Test.test "record" <|
                \_ ->
                    given "---\naaa: 1\nbbb: 2" (Yaml.dict Yaml.int)
                        |> Expect.equal (Ok (Dict.fromList [ ( "aaa", 1 ), ( "bbb", 2 ) ]))
            , Test.test "record on a single line" <|
                \_ ->
                    given "aaa: bbb" (Yaml.dict Yaml.string)
                        |> Expect.equal (Ok (Dict.singleton "aaa" "bbb"))
            , Test.test "record with sub-record" <|
                \_ ->
                    given "\nparent:\n  childA: 1\n  childB: 2\n"
                        (Yaml.dict <| Yaml.dict Yaml.int)
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "parent"
                                    (Dict.fromList [ ( "childA", 1 ), ( "childB", 2 ) ])
                                )
                            )
            , Test.test "record with sub-record of inline list" <|
                \_ ->
                    given "parent:\n  childA: [1, 2, 3]\n  childB: - 4\n          - 5\n"
                        (Yaml.dict <| Yaml.dict (Yaml.list Yaml.int))
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "parent"
                                    (Dict.fromList
                                        [ ( "childA", [ 1, 2, 3 ] ), ( "childB", [ 4, 5 ] ) ]
                                    )
                                )
                            )
            , Test.test "record of list with bad indentation" <|
                \_ ->
                    given "parent:\n  - aaa\n   - bbb\n"
                        (Yaml.dict <| Yaml.list Yaml.string)
                        |> Expect.equal
                            (Ok (Dict.singleton "parent" [ "aaa - bbb" ]))
            , Test.test "record of list with bad indentation and a comment" <|
                \_ ->
                    given "parent:\n  - aaa #   A comment \n   - bbb\n"
                        (Yaml.dict <| Yaml.list Yaml.string)
                        |> Expect.equal
                            (Ok (Dict.singleton "parent" [ "aaa - bbb" ]))
            ]
        , Test.describe "mapping"
            [ Test.fuzz (Fuzz.map sanitiseString string) "map a single value" <|
                \s ->
                    given s (Yaml.map String.length Yaml.string)
                        |> Expect.equal (Ok (String.length <| String.trim s))
            , Test.fuzz (Fuzz.map2 (\f1 f2 -> ( f1, f2 )) float float) "map 2 values" <|
                \fs ->
                    let
                        ( f1, f2 ) =
                            fs
                    in
                    given
                        ("aaa: "
                            ++ formatFloat f1
                            ++ "  \nbbb: "
                            ++ formatFloat f2
                        )
                        (Yaml.map2 (\y1 y2 -> y1 + y2)
                            (Yaml.field "aaa" Yaml.float)
                            (Yaml.field "bbb" Yaml.float)
                        )
                        |> Result.map (expectCloseTo (f1 + f2))
                        |> toExpect
            ]
        ]


{-| Utility function that sets up a test.
-}
given : String -> Yaml.Decoder a -> Result Yaml.Error a
given input decoder =
    Yaml.fromString decoder input
