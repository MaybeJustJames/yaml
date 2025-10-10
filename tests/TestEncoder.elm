module TestEncoder exposing (suite)

import Dict
import Expect
import Format exposing (formatFloatOutput)
import Fuzz exposing (bool, float, int, list, map2, string)
import Test
import Yaml.Decode as Decode
import Yaml.Encode as Encode


suite : Test.Test
suite =
    Test.describe "Encoding"
        [ Test.describe "String values"
            [ Test.test "simple string" <|
                \_ ->
                    Expect.equal "string" (Encode.toString 0 (Encode.string "string"))
            ]
        , Test.describe "Numeric values"
            [ Test.fuzz int "integer" <|
                \x ->
                    Expect.equal (String.fromInt x) (Encode.toString 0 (Encode.int x))
            , Test.test "NaN" <|
                \_ ->
                    Expect.equal ".nan" (Encode.toString 0 (Encode.float (0 / 0)))
            , Test.test "Infinity" <|
                \_ ->
                    Expect.equal ".inf" (Encode.toString 0 (Encode.float (1 / 0)))
            , Test.test "-Infinity" <|
                \_ ->
                    Expect.equal "-.inf" (Encode.toString 0 (Encode.float -(1 / 0)))
            ]
        , Test.describe "Boolean values"
            [ Test.fuzz bool "Bool" <|
                \x ->
                    Expect.equal
                        (if x then
                            "true"

                         else
                            "false"
                        )
                        (Encode.toString 0 (Encode.bool x))
            ]
        , Test.describe "Lists"
            [ Test.test "Empty list" <|
                \_ -> Expect.equal "[]" (Encode.toString 2 (Encode.list Encode.int []))
            , Test.fuzz (list int) "inline list of integers" <|
                \xs ->
                    let
                        expected =
                            "[" ++ String.join ", " (xs |> List.map String.fromInt) ++ "]"
                    in
                    Expect.equal expected
                        (Encode.toString 0 (Encode.list Encode.int xs))
            , Test.test "list of integers indent 2" <|
                \_ ->
                    let
                        expected =
                            "- 1\n- 2\n- 3"
                    in
                    Expect.equal expected
                        (Encode.toString 2 (Encode.list Encode.int [ 1, 2, 3 ]))
            , Test.test "list of bool indent 1" <|
                \_ ->
                    let
                        expected =
                            "- true\n- true\n- false"
                    in
                    Expect.equal expected
                        (Encode.toString 1 (Encode.list Encode.bool [ True, True, False ]))
            , Test.test "list of lists of int" <|
                \_ ->
                    let
                        expected =
                            "- - 1\n  - 2\n- - 3\n  - 4\n- - 5\n  - 6"

                        encoder =
                            Encode.list (Encode.list Encode.int)
                    in
                    Expect.equal expected
                        (Encode.toString 2
                            (encoder
                                [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
                            )
                        )
            , Test.test "list of lists of int indented 5" <|
                \_ ->
                    let
                        expected =
                            "-    -    1\n     -    2\n-    -    3\n     -    4\n-    -    5\n     -    6"

                        encoder =
                            Encode.list (Encode.list Encode.int)
                    in
                    Expect.equal expected
                        (Encode.toString 5
                            (encoder
                                [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
                            )
                        )
            , Test.test "list of lists of int indented 3" <|
                \_ ->
                    let
                        expected =
                            "-  -  1\n   -  2\n-  -  3\n   -  4\n-  -  5\n   -  6"

                        encoder =
                            Encode.list (Encode.list Encode.int)
                    in
                    Expect.equal expected
                        (Encode.toString 3
                            (encoder
                                [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
                            )
                        )
            , Test.test "list of lists of list of int" <|
                \_ ->
                    let
                        expected =
                            "-  -  -  1\n      -  2\n-  -  -  3\n      -  4"

                        encoder =
                            Encode.list <| Encode.list <| Encode.list Encode.int
                    in
                    Expect.equal expected
                        (Encode.toString 3
                            (encoder
                                [ [ [ 1, 2 ] ], [ [ 3, 4 ] ] ]
                            )
                        )
            , Test.test "Empty sub-list" <|
                \_ ->
                    Expect.equal "test: []"
                        (Encode.toString 2
                            (Encode.record [ ( "test", Encode.list Encode.int [] ) ])
                        )
            , Test.test "List of records" <|
                \_ ->
                    let
                        rencoder r =
                            Encode.record [ ( "a", Encode.int r.a ), ( "b", Encode.int r.b ) ]
                    in
                    Expect.equal "- a: 1\n  b: 2\n- a: 42\n  b: 24"
                        (Encode.toString 2
                            (Encode.list rencoder
                                [ { a = 1, b = 2 }
                                , { a = 42, b = 24 }
                                ]
                            )
                        )
            ]
        , Test.describe "Dicts"
            [ Test.test "Empty dict" <|
                \_ ->
                    Expect.equal "{}"
                        (Encode.toString 2
                            (Encode.dict identity Encode.int Dict.empty)
                        )
            , Test.fuzz int "singleton inline record of ints" <|
                \x ->
                    Expect.equal
                        ("{x: "
                            ++ String.fromInt x
                            ++ "}"
                        )
                        (Encode.toString
                            0
                            (Encode.dict identity
                                Encode.int
                                (Dict.singleton "x" x)
                            )
                        )
            , Test.fuzz float "singleton inline record of floats" <|
                \x ->
                    Expect.equal
                        ("{x: "
                            ++ formatFloatOutput x
                            ++ "}"
                        )
                        (Encode.toString
                            0
                            (Encode.dict identity
                                Encode.float
                                (Dict.singleton "x" x)
                            )
                        )
            , Test.test "record of strings" <|
                \_ ->
                    let
                        expected =
                            "aaa: aaa\nbbb: bbb"

                        encoder =
                            Encode.dict identity Encode.string
                    in
                    Expect.equal expected
                        (Encode.toString 2
                            (encoder <|
                                Dict.fromList
                                    [ ( "aaa", "aaa" ), ( "bbb", "bbb" ) ]
                            )
                        )
            , Test.test "record of floats" <|
                \_ ->
                    let
                        expected =
                            "aaa: 0\nbbb: 1.1\nccc: -3.1415"

                        encoder =
                            Encode.dict identity Encode.float
                    in
                    Expect.equal expected
                        (Encode.toString 2
                            (encoder <|
                                Dict.fromList
                                    [ ( "aaa", 0.0 ), ( "bbb", 1.1 ), ( "ccc", -3.1415 ) ]
                            )
                        )
            , Test.test "record of bools" <|
                \_ ->
                    let
                        expected =
                            "aaa: true\nbbb: true\nccc: false"
                    in
                    Expect.equal expected
                        (Encode.toString 2
                            (Encode.dict identity Encode.bool <|
                                Dict.fromList
                                    [ ( "aaa", True )
                                    , ( "bbb", True )
                                    , ( "ccc", False )
                                    ]
                            )
                        )
            , Test.test "record of record of floats" <|
                \_ ->
                    let
                        expected =
                            "a a a:\n    bbb: 1\nc c c:\n    ddd: 0.1"

                        encoder =
                            Encode.dict identity <|
                                Encode.dict identity Encode.float
                    in
                    Expect.equal expected
                        (Encode.toString 4
                            (encoder <|
                                Dict.fromList
                                    [ ( "a a a", Dict.singleton "bbb" 1.0 )
                                    , ( "c c c", Dict.singleton "ddd" 0.1 )
                                    ]
                            )
                        )
            , Test.test "record of record of multiple floats" <|
                \_ ->
                    let
                        expected =
                            "a a a:\n    bbb: 1\n    ccc: 3.14\nc c c:\n    ddd: 0.1"

                        encoder =
                            Encode.dict identity <|
                                Encode.dict identity Encode.float
                    in
                    Expect.equal expected
                        (Encode.toString 4
                            (encoder <|
                                Dict.fromList
                                    [ ( "a a a", Dict.fromList [ ( "bbb", 1.0 ), ( "ccc", 3.14 ) ] )
                                    , ( "c c c", Dict.singleton "ddd" 0.1 )
                                    ]
                            )
                        )
            , Test.test "record of list of strings" <|
                \_ ->
                    let
                        expected =
                            "aaa:\n  - abc\n  - def\nzzz:\n  - ghi\n  - jkl"

                        encoder =
                            Encode.dict identity <|
                                Encode.list Encode.string
                    in
                    Expect.equal expected
                        (Encode.toString 2
                            (encoder <|
                                Dict.fromList
                                    [ ( "aaa", [ "abc", "def" ] )
                                    , ( "zzz", [ "ghi", "jkl" ] )
                                    ]
                            )
                        )
            , Test.test "Inline dict of lists" <|
                \_ ->
                    Expect.equal "[{a: 1, b: 2, c: 3}, {e: 21, g: 42, z: 101}]"
                        (Encode.toString 0
                            (Encode.list
                                (Encode.dict identity Encode.int)
                                [ Dict.fromList [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ) ]
                                , Dict.fromList [ ( "e", 21 ), ( "g", 42 ), ( "z", 101 ) ]
                                ]
                            )
                        )
            ]
        , Test.describe "Records"
            [ Test.test "Empty record" <|
                \_ ->
                    Expect.equal "{}" (Encode.toString 2 (Encode.record []))
            , Test.fuzz int "singleton inline record of int" <|
                \x ->
                    Expect.equal
                        ("{x: "
                            ++ String.fromInt x
                            ++ "}"
                        )
                        (Encode.toString
                            0
                            (Encode.record [ ( "x", Encode.int x ) ])
                        )
            , Test.fuzz bool "singleton inline record of bool" <|
                \x ->
                    let
                        boolToString : Bool -> String
                        boolToString b =
                            if b then
                                "true"

                            else
                                "false"
                    in
                    Expect.equal
                        ("{x: "
                            ++ boolToString x
                            ++ "}"
                        )
                        (Encode.toString
                            0
                            (Encode.record [ ( "x", Encode.bool x ) ])
                        )
            , Test.fuzz string "singleton record of strings" <|
                \s ->
                    let
                        quotedS =
                            "'" ++ s ++ "'"

                        expected =
                            "x: " ++ quotedS

                        encoder =
                            Encode.record [ ( "x", Encode.string quotedS ) ]
                    in
                    Expect.equal expected
                        (Encode.toString 2 encoder)
            , Test.fuzz (list <| map2 Tuple.pair string float) "records of floats" <|
                \pairs ->
                    let
                        quote : String -> String
                        quote s =
                            "'" ++ s ++ "'"

                        expected =
                            if List.isEmpty pairs then
                                "{}"

                            else
                                pairs
                                    |> List.map
                                        (\( key, val ) ->
                                            quote key ++ ": " ++ formatFloatOutput val
                                        )
                                    |> String.join "\n"

                        encoder =
                            Encode.record <|
                                List.map
                                    (\( key, val ) ->
                                        ( quote key, Encode.float val )
                                    )
                                    pairs
                    in
                    Expect.equal expected
                        (Encode.toString 2 encoder)
            , Test.test "Empty sub-record" <|
                \_ ->
                    Expect.equal "test: {}"
                        (Encode.toString 2
                            (Encode.record [ ( "test", Encode.record [] ) ])
                        )
            , Test.test "Inline record of lists" <|
                \_ ->
                    let
                        rencoder r =
                            Encode.record
                                [ ( "a", Encode.int r.a )
                                , ( "b", Encode.int r.b )
                                , ( "c", Encode.int r.c )
                                ]
                    in
                    Expect.equal "[{a: 1, b: 2, c: 3}, {a: 21, b: 42, c: 101}]"
                        (Encode.toString 0
                            (Encode.list
                                rencoder
                                [ { a = 1, b = 2, c = 3 }, { a = 21, b = 42, c = 101 } ]
                            )
                        )
            ]
        , Test.describe "A Document"
            [ Test.test "A document containing an int" <|
                \_ ->
                    Expect.equal "---\n5\n..."
                        (Encode.toString 0 (Encode.document <| Encode.int 5))
            ]
        , Test.describe "Raw decoder values"
            [ Test.test "A null value roundtrip" <|
                \_ ->
                    let
                        null : Maybe Decode.Value
                        null =
                            Decode.fromString Decode.value "null"
                                |> Result.toMaybe
                    in
                    Expect.equal
                        (Just "null")
                        (Maybe.map (\val -> Encode.toString 0 (Encode.value val)) null)
            , Test.test "A list roundtrip" <|
                \_ ->
                    let
                        data : String
                        data =
                            "[1, 3, 42, 11]"

                        mylist : Maybe Decode.Value
                        mylist =
                            Decode.fromString Decode.value data
                                |> Result.toMaybe
                    in
                    Expect.equal
                        (Just data)
                        (Maybe.map (\val -> Encode.toString 0 (Encode.value val)) mylist)
            , Test.test "A record containing lists roundtrip" <|
                \_ ->
                    let
                        data : String
                        data =
                            "{a: [1], b: [2]}"

                        myrec : Maybe Decode.Value
                        myrec =
                            Decode.fromString Decode.value data
                                |> Result.toMaybe
                    in
                    Expect.equal
                        (Just data)
                        (Maybe.map (\val -> Encode.toString 0 (Encode.value val)) myrec)
            ]
        ]
