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
            Byte bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8
        )
        (tuple5 ( bit, bit, bit, bit, bit ))
        (tuple3 ( bit, bit, bit ))


suite : Test
suite =
    describe "the Binary module"
        [ describe "Binary.bitAnd"
            [ test "1 & 1 = 1" <|
                \_ -> bitAnd I I |> Expect.equal I
            , test "1 & 0 = 0" <|
                \_ -> bitAnd I O |> Expect.equal O
            , test "0 & 0 = 0" <|
                \_ -> bitAnd O O |> Expect.equal O
            , test "0 & 1 = 0" <|
                \_ -> bitAnd O I |> Expect.equal O
            ]
        , describe "Binary.toList && Binary.toByte"
            [ fuzz byte "doing toList and toTuple should return the same original input" <|
                \generatedByte ->
                    generatedByte
                        |> toList
                        |> toByte
                        |> Expect.equal generatedByte
            ]
        , describe "Binary.byteAnd"
            [ fuzz byte "and with a zeroByte should always be just the zeroByte" <|
                \generatedByte ->
                    generatedByte
                        |> byteAnd zeroByte
                        |> Expect.equal zeroByte
            , fuzz byte "and with a fullByte should always just be the original byte" <|
                \generatedByte ->
                    generatedByte
                        |> byteAnd fullByte
                        |> Expect.equal generatedByte
            ]
        , describe "Binary.byteOr"
            [ fuzz byte "or with a fullByte should always be a fullByte" <|
                \generatedByte ->
                    generatedByte
                        |> byteOr fullByte
                        |> Expect.equal fullByte
            , fuzz byte "or with a zero byte should always be the original byte" <|
                \generatedByte ->
                    generatedByte
                        |> byteOr zeroByte
                        |> Expect.equal generatedByte
            ]
        ]
