module Expectations exposing (expectCloseTo, expectFail, toExpect)

import Debug exposing (toString)
import Expect
import Yaml.Decode as Decode


{-| Utility function that checks that floats are within a range
-}
expectCloseTo : Float -> Float -> Expect.Expectation
expectCloseTo expected got =
    if isNaN expected && isNaN got then
        Expect.pass

    else
        Expect.within (Expect.Absolute 0.00000001) expected got


{-| Utility function that checks the failure mode of a Decoder
-}
expectFail : String -> Result Decode.Error a -> Expect.Expectation
expectFail expected got =
    Expect.equal (Err (Decode.Decoding expected)) got


{-| Utility to convery a Result into an Expectation
-}
toExpect : Result err Expect.Expectation -> Expect.Expectation
toExpect from =
    case from of
        Ok expect ->
            expect

        Err e ->
            Expect.fail (toString e)
