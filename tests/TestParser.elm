module TestParser exposing (suite)

import Dict
import Expect
import Expectations exposing (expectCloseTo)
import Format exposing (formatFloat)
import Fuzz exposing (float, int)
import Parser as P
import Test
import Yaml.Parser as Parser
import Yaml.Parser.Ast as Ast


suite : Test.Test
suite =
    Test.describe "The parser"
        [ Test.test "nothing" <|
            \_ ->
                expectValue "" <|
                    Ast.Null_
        , Test.test "nothing with document start" <|
            \_ ->
                expectValue "---" <|
                    Ast.Null_
        , Test.test "nothing with document end" <|
            \_ ->
                expectValue "..." <|
                    Ast.Null_
        , Test.test "nothing with document start and end" <|
            \_ ->
                -- TODO is this the right response?
                expectValue "---..." <|
                    Ast.Null_
        , Test.test "null" <|
            \_ ->
                expectValue "" <|
                    Ast.Null_
        , Test.fuzz int "an int" <|
            \x ->
                expectValue (String.fromInt x) <|
                    Ast.Int_ x
        , Test.fuzz float "a float" <|
            \x ->
                expectValue
                    (if x == 0.0 then
                        "0.0"

                     else if isNaN x then
                        ".NaN"

                     else
                        formatFloat x
                    )
                <|
                    Ast.Float_ x
        , Test.test "a single-quoted string" <|
            \_ ->
                -- TODO is this right?
                expectValue """'hey
          i am a 

          parser'""" <|
                    Ast.String_ "hey i am a\nparser"
        , Test.test "a single quoted string with no closing quote" <|
            \_ ->
                expectErr "'hello"
        , Test.test "a double-quoted string" <|
            \_ ->
                expectValue """"hey
          i am a
          parser" """ <|
                    Ast.String_ "hey i am a parser"
        , Test.test "a double-quoted string with no closing quote" <|
            \_ ->
                expectErr "\"hello"
        , Test.test "a string containing a colon" <|
            \_ ->
                expectValue ":!" <| Ast.String_ ":!"
        , Test.test "another string containing a colon" <|
            \_ ->
                expectValue "a:b" <| Ast.String_ "a:b"
        , Test.test "a single-line string" <|
            \_ ->
                expectValue "hey i am a parser" <|
                    Ast.String_ "hey i am a parser"
        , Test.test "a multi-line string" <|
            \_ ->
                expectValue
                    """how does one teach self-respect?
            how does one teach curiousity?
            if you have the answers, call me
            """
                <|
                    Ast.String_ "how does one teach self-respect? how does one teach curiousity? if you have the answers, call me"
        , Test.test "a multi-line string with three dot ending" <|
            \_ ->
                expectValue
                    """how does one teach self-respect?
            how does one teach curiousity?
            if you have the answers, call me
...



            """
                <|
                    Ast.String_ "how does one teach self-respect? how does one teach curiousity? if you have the answers, call me"
        , Test.test "a empty inline list" <|
            \_ ->
                expectValue "[]" <|
                    Ast.List_ []
        , Test.test "an inline list with an empty single-quoted string" <|
            \_ ->
                expectValue "[ '' ]" <|
                    Ast.List_ [ Ast.String_ "" ]
        , Test.test "an inline list with an empty double-quoted string" <|
            \_ ->
                expectValue "[\"\"]" <| Ast.List_ [ Ast.String_ "" ]
        , Test.test "an inline list with a backslash" <|
            \_ ->
                expectValue "[ \\ ]" <| Ast.List_ [ Ast.String_ "\\\\" ]
        , Test.test "an inline list with a quoted backslash" <|
            \_ ->
                expectValue "['\\']" <| Ast.List_ [ Ast.String_ "\\\\" ]
        , Test.test "a list with an empty quoted string" <|
            \_ ->
                expectValue " - ''" <| Ast.List_ [ Ast.String_ "" ]
        , Test.test "an inline list with no closing ]" <|
            \_ ->
                expectErr "["
        , Test.test "an inline list with ints and no spaces" <|
            \_ ->
                expectValue "[0,1,2]" <|
                    Ast.List_ [ Ast.Int_ 0, Ast.Int_ 1, Ast.Int_ 2 ]
        , Test.test "an inline list with ints and with spaces" <|
            \_ ->
                expectValue "[ 0, 1, 2 ]" <|
                    Ast.List_ [ Ast.Int_ 0, Ast.Int_ 1, Ast.Int_ 2 ]
        , Test.test "an inline list with strings and no spaces" <|
            \_ ->
                expectValue "[aaa,bbb,ccc]" <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
        , Test.test "an inline list with strings and with spaces" <|
            \_ ->
                expectValue "[ aaa, bbb, ccc ]" <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
        , Test.test "an inline list with strings and with new lines" <|
            \_ ->
                expectValue """[
           aaa,
           bbb, 
           ccc 
           ]""" <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
        , Test.test "an inline list with strings and with new lines and comments" <|
            \_ ->
                expectValue """[ aaa,# A comment
           bbb, # a dumb comment
           ccc
           ]""" <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
        , Test.test "an inline list with strings, with new lines, and multi-line strings" <|
            \_ ->
                expectValue """[ aaa,
           bbb, 
           ccc ccc
           ccc 
           ]""" <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc ccc\n           ccc" ]
        , Test.test "an inline list with an inline list inside" <|
            \_ ->
                expectValue "[ aaa, [ bbb, aaa, ccc ], ccc ]" <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.List_ [ Ast.String_ "bbb", Ast.String_ "aaa", Ast.String_ "ccc" ], Ast.String_ "ccc" ]
        , Test.test "an inline list with a record inside" <|
            \_ ->
                expectValue "[ aaa, {bbb: bbb, aaa: aaa, ccc: ccc}, ccc ]" <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.Record_ (Dict.fromList [ ( "bbb", Ast.String_ "bbb" ), ( "aaa", Ast.String_ "aaa" ), ( "ccc", Ast.String_ "ccc" ) ]), Ast.String_ "ccc" ]
        , Test.test "an inline record" <|
            \_ ->
                expectValue "{bbb: bbb, aaa: aaa, ccc: ccc}" <|
                    Ast.Record_ (Dict.fromList [ ( "bbb", Ast.String_ "bbb" ), ( "aaa", Ast.String_ "aaa" ), ( "ccc", Ast.String_ "ccc" ) ])
        , Test.test "empty inline record" <|
            \_ ->
                expectValue "{}" <| Ast.Record_ Dict.empty
        , Test.test "inline record without closing }" <|
            \_ ->
                expectErr "{"
        , Test.test "empty inline record with whitespace" <|
            \_ ->
                expectValue "  {    } " <| Ast.Record_ Dict.empty
        , Test.test "a short inline record" <|
            \_ ->
                expectValue "{a: 1}" <| Ast.Record_ (Dict.singleton "a" (Ast.Int_ 1))
        , Test.test "an inline record with weird spacing" <|
            \_ ->
                expectValue
                    """
            {bbb:
             bbb , aaa: aaa
                  , ccc: ccc}
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "bbb", Ast.String_ "bbb" ), ( "aaa", Ast.String_ "aaa" ), ( "ccc", Ast.String_ "ccc" ) ])
        , Test.test "an inline record with an inline record inside" <|
            \_ ->
                expectValue "{bbb: bbb, aaa: {bbb: bbb, aaa: aaa, ccc: ccc}, ccc: ccc}" <|
                    Ast.Record_ (Dict.fromList [ ( "bbb", Ast.String_ "bbb" ), ( "aaa", Ast.Record_ (Dict.fromList [ ( "bbb", Ast.String_ "bbb" ), ( "aaa", Ast.String_ "aaa" ), ( "ccc", Ast.String_ "ccc" ) ]) ), ( "ccc", Ast.String_ "ccc" ) ])
        , Test.test "a list" <|
            \_ ->
                expectValue
                    """
            - aaa
            - bbb
            - ccc
            """
                <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ]
        , Test.test "a list with null" <|
            \_ ->
                expectValue
                    """
          -
          - bbb
          - ccc
          """
                <|
                    Ast.List_ [ Ast.Null_, Ast.String_ "bbb", Ast.String_ "ccc" ]
        , Test.test "a list with multi-line string" <|
            \_ ->
                expectValue
                    """
          -
          - bbb
           bbb bbb
           bbb
          - ccc
          """
                <|
                    Ast.List_ [ Ast.Null_, Ast.String_ "bbb bbb bbb bbb", Ast.String_ "ccc" ]
        , Test.test "a list with a list inside" <|
            \_ ->
                expectValue """
          - aaa
          -
            - aaa
            - bbb
            - ccc
          - ccc
          """ <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ], Ast.String_ "ccc" ]
        , Test.test "a list with a list inside on same line" <|
            \_ ->
                expectValue """
          - aaa
          - - aaa
            - bbb
            - ccc
          - ccc
          """ <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ], Ast.String_ "ccc" ]
        , Test.test "a list with smaller trailing indentation" <|
            \_ ->
                expectValue """
            - aaa
            - bbb
      """ <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb" ]
        , Test.test "a list with larger trailing indentation" <|
            \_ ->
                expectValue """
 - aaa
 - bbb
    """ <|
                    Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb" ]
        , Test.test "a record" <|
            \_ ->
                expectValue
                    """
          aaa: aaa
          bbb: bbb
          ccc: ccc
          """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.String_ "aaa" ), ( "bbb", Ast.String_ "bbb" ), ( "ccc", Ast.String_ "ccc" ) ])
        , Test.test "a record with quoted property names and values" <|
            \_ ->
                expectValue
                    """
            'aaa': aaa
            "bbb": "bbb"
            'ccc': 'ccc'
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.String_ "aaa" ), ( "bbb", Ast.String_ "bbb" ), ( "ccc", Ast.String_ "ccc" ) ])
        , Test.test "a record with a quoted property name and a space before the colon" <|
            \_ ->
                expectValue "'aaa' : 1" <|
                    Ast.Record_ (Dict.singleton "aaa" (Ast.Int_ 1))
        , Test.test "a record with a quoted property name containing whitespace" <|
            \_ ->
                expectValue " ' hello  world  '  :" <|
                    Ast.Record_ (Dict.singleton " hello  world  " Ast.Null_)
        , Test.test "an inline list containing an inline record with a quoted property name containing whitespace" <|
            \_ ->
                expectValue " [ {'   hello  world  ' : 3} ]   " <|
                    Ast.List_ [ Ast.Record_ (Dict.singleton "   hello  world  " (Ast.Int_ 3)) ]

        -- TODO: This is temporarily removed because it is a valid test case that should pass
        -- , Test.test "an inline list containing a record with a quoted property name containing whitespace" <|
        --     \_ ->
        --       expectValue " [ ' hello  world  ' : 3 ]   " <|
        --         Ast.List_ [ Ast.Record_ (Dict.singleton " hello  world  " (Ast.Int_ 3)) ]
        , Test.test "a record with a record inside" <|
            \_ ->
                expectValue
                    """
            aaa: aaa
            bbb:
              aaa: aaa
              bbb: bbb
              ccc: ccc
            ccc: ccc
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.String_ "aaa" ), ( "bbb", Ast.Record_ (Dict.fromList [ ( "aaa", Ast.String_ "aaa" ), ( "bbb", Ast.String_ "bbb" ), ( "ccc", Ast.String_ "ccc" ) ]) ), ( "ccc", Ast.String_ "ccc" ) ])
        , Test.test "a record with a list inside" <|
            \_ ->
                expectValue
                    """
            aaa: aaa
            bbb:
              - aaa
              - bbb
              - ccc
            ccc: ccc
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.String_ "aaa" ), ( "bbb", Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ] ), ( "ccc", Ast.String_ "ccc" ) ])
        , Test.test "a record with a list inside on the same level" <|
            \_ ->
                expectValue
                    """
            aaa: aaa
            bbb:
            - aaa
            - bbb
            - ccc
            ccc: ccc
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.String_ "aaa" ), ( "bbb", Ast.List_ [ Ast.String_ "aaa", Ast.String_ "bbb", Ast.String_ "ccc" ] ), ( "ccc", Ast.String_ "ccc" ) ])
        , Test.test "a record with strings comments" <|
            \_ ->
                expectValue
                    """
            aaa: aaa# hey
            bbb: bbb # hey
            ccc: ccc   # hey
            ddd: ddd  # hey
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.String_ "aaa" ), ( "bbb", Ast.String_ "bbb" ), ( "ccc", Ast.String_ "ccc" ), ( "ddd", Ast.String_ "ddd" ) ])
        , Test.test "a record with mixed values" <|
            \_ ->
                expectValue
                    """
            aaa: 1
            bbb: bbb
            ccc: 1.0
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.Int_ 1 ), ( "bbb", Ast.String_ "bbb" ), ( "ccc", Ast.Float_ 1.0 ) ])
        , Test.test "a record with numbers and comments" <|
            \_ ->
                expectValue
                    """
            aaa: 1# First
            bbb:  2.0 # A comment
            ccc:   3  #   Another comment
            ddd:    4.5  #
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.Int_ 1 ), ( "bbb", Ast.Float_ 2.0 ), ( "ccc", Ast.Int_ 3 ), ( "ddd", Ast.Float_ 4.5 ) ])
        , Test.test "a record on a single line" <|
            \_ ->
                expectValue
                    "aaa: aaa"
                <|
                    Ast.Record_ (Dict.singleton "aaa" <| Ast.String_ "aaa")
        , Test.test "a record on a single line with a quoted value" <|
            \_ ->
                expectValue
                    "aaa: 'aaa'"
                <|
                    Ast.Record_ (Dict.singleton "aaa" <| Ast.String_ "aaa")
        , Test.fuzz int "a record on a single line with an integer value" <|
            \x ->
                expectValue
                    ("aaa: " ++ String.fromInt x)
                <|
                    Ast.Record_ (Dict.singleton "aaa" <| Ast.Int_ x)
        , Test.test "a # character in a quoted string" <|
            \_ ->
                expectValue
                    """
            aaa: '# a string'
            bbb:# a comment
            """
                <|
                    Ast.Record_ (Dict.fromList [ ( "aaa", Ast.String_ "# a string" ), ( "bbb", Ast.Null_ ) ])
        , Test.test "trailing spaces after record values" <|
            \_ ->
                expectValue "  a: 1    \n  b: 2     " <|
                    Ast.Record_ (Dict.fromList [ ( "a", Ast.Int_ 1 ), ( "b", Ast.Int_ 2 ) ])
        , Test.test "Ast for anchor and reference in inline list" <|
            \_ ->
                expectRawAst "[ &1 test, *1]" <|
                    Ast.List_
                        [ Ast.Anchor_ "1" (Ast.String_ "test")
                        , Ast.Alias_ "1"
                        ]
        , Test.test "Anchor and reference in inline list" <|
            \_ ->
                expectValue "[ &1 test, *1]" <|
                    Ast.List_
                        [ Ast.String_ "test"
                        , Ast.String_ "test"
                        ]
        , Test.test "Ast for anchor and reference in list" <|
            \_ ->
                expectRawAst "- &1 test\n- *1" <|
                    Ast.List_
                        [ Ast.Anchor_ "1" (Ast.String_ "test")
                        , Ast.Alias_ "1"
                        ]
        , Test.test "Anchor and reference in list" <|
            \_ ->
                expectValue "- &1 test\n- *1" <|
                    Ast.List_
                        [ Ast.String_ "test"
                        , Ast.String_ "test"
                        ]
        , Test.test "Ast for anchor and reference in inline record" <|
            \_ ->
                expectRawAst "{a: &1 test, b: *1}" <|
                    Ast.Record_
                        (Dict.fromList
                            [ ( "a", Ast.Anchor_ "1" (Ast.String_ "test") )
                            , ( "b", Ast.Alias_ "1" )
                            ]
                        )
        , Test.test "Anchor and reference in inline record" <|
            \_ ->
                expectValue "{a: &1 test, b: *1}" <|
                    Ast.Record_
                        (Dict.fromList
                            [ ( "a", Ast.String_ "test" )
                            , ( "b", Ast.String_ "test" )
                            ]
                        )
        , Test.test "Ast for anchor and reference in record" <|
            \_ ->
                expectRawAst "a: &1 test\nb: *1" <|
                    Ast.Record_
                        (Dict.fromList
                            [ ( "a", Ast.Anchor_ "1" (Ast.String_ "test") )
                            , ( "b", Ast.Alias_ "1" )
                            ]
                        )
        , Test.test "Anchor and reference in record" <|
            \_ ->
                expectValue "a: &1 test\nb: *1" <|
                    Ast.Record_
                        (Dict.fromList
                            [ ( "a", Ast.String_ "test" )
                            , ( "b", Ast.String_ "test" )
                            ]
                        )
        , Test.test "Anchor and reference for complex record" <|
            \_ ->
                expectValue "a: &myref\n  foo: bar\nb: *myref\n" <|
                    Ast.Record_
                        (Dict.fromList
                            [ ( "a", Ast.Record_ (Dict.singleton "foo" (Ast.String_ "bar")) )
                            , ( "b", Ast.Record_ (Dict.singleton "foo" (Ast.String_ "bar")) )
                            ]
                        )
        , Test.test "Real data with anchor and alias" <|
            \_ ->
                expectValue testYamlData <|
                    Ast.Record_
                        (Dict.singleton
                            "graph"
                            (Ast.Record_
                                (Dict.singleton
                                    "processors"
                                    (Ast.Record_
                                        (Dict.fromList
                                            [ ( "ripple_filter1"
                                              , Ast.Record_
                                                    (Dict.singleton "options"
                                                        (Ast.Record_
                                                            (Dict.singleton
                                                                "filter"
                                                                (Ast.Record_
                                                                    (Dict.singleton
                                                                        "file"
                                                                        (Ast.String_ "path/test/f.ext")
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                              )
                                            , ( "ripple_filter2"
                                              , Ast.Record_
                                                    (Dict.singleton "options"
                                                        (Ast.Record_
                                                            (Dict.singleton
                                                                "filter"
                                                                (Ast.Record_
                                                                    (Dict.singleton
                                                                        "file"
                                                                        (Ast.String_ "path/test/f.ext")
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                              )
                                            ]
                                        )
                                    )
                                )
                            )
                        )
        , Test.test "an inline record with 2 duplicated keys" <|
            \_ ->
                expectErr
                    """aaa: aaa
aaa: bbb"""
        , Test.test "a record with 2 duplicated keys" <|
            \_ ->
                expectErr
                    """
                aaa:
                    key1: value1
                    key1: value2
                """
        , Test.test "an inline record with 2 duplicated keys, version 2" <|
            \_ ->
                expectErr
                    """
                aaa: { key1: value1, key1: value2 }
                """

        -- TODO: This is temporarily removed because it is a valid test case that should pass
        -- , Test.test "weird colon record" <|
        --     \_ ->
        --       expectValue "::" <| Ast.Record_ (Dict.singleton ":" Ast.Null_)
        ]


expectRawAst : String -> Ast.Value -> Expect.Expectation
expectRawAst subject expected =
    P.run Parser.parser subject
        |> Expect.equal (Ok expected)


expectValue : String -> Ast.Value -> Expect.Expectation
expectValue subject expected =
    case ( Parser.fromString subject, expected ) of
        ( Ok (Ast.Float_ got), Ast.Float_ want ) ->
            expectCloseTo got want

        ( got, _ ) ->
            Expect.equal got (Ok expected)


expectErr : String -> Expect.Expectation
expectErr subject =
    Parser.fromString subject
        |> Expect.err


testYamlData : String
testYamlData =
    """
graph:
  processors:
    ripple_filter1:
      options: &1
        filter:
          file: path/test/f.ext
    ripple_filter2:
      options: *1
"""
