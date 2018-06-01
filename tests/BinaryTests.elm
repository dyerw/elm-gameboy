module BinaryTests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Binary exposing (..)


bit : Fuzzer Bit
bit =
    oneOf [ constant O, constant I ]


byte : Fuzzer Byte
byte =
    map2
        (\( bit1, bit2, bit3, bit4, bit5 ) ( bit6, bit7, bit8 ) ->
            ( bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8 )
        )
        (tuple5 ( bit, bit, bit, bit, bit ))
        (tuple3 ( bit, bit, bit ))


suite : Test
suite =
    describe "the Binary module"
        [ describe "bitAnd"
            [ test "1 & 1 = 1" (\_ -> bitAnd I I |> Expect.equal I)
            , test "1 & 0 = 0" (\_ -> bitAnd I O |> Expect.equal O)
            , test "0 & 0 = 0" (\_ -> bitAnd O O |> Expect.equal O)
            , test "0 & 1 = 0" (\_ -> bitAnd O I |> Expect.equal O)
            ]
        ]
