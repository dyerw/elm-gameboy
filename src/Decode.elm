module Decode exposing (..)

import CPU exposing (Instruction(..), RegisterName(..))
import Binary exposing (Word, toHexString)


-- An op code can include just the op code or
-- an eight bit or sixteen bit immediate value


type OpCode
    = NullaryOpCode Byte
    | OneByteOpCode Byte Byte
    | TwoByteOpcode Byte Byte Byte


decode : OpCode -> Instruction
decode opCode =
    case opCode of
        NullaryOpCode byte ->
            case (toHexString byte) of
                "00" ->
                    NOP
                "02" ->
                    LDRegister BC A

       OneByteOpCode byte byte2 ->
            NOP

        TwoByteOpcode byte byte2 byte3 ->
            NOP
