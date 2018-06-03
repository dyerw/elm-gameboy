module Decode exposing (..)

import CPU exposing (Instruction(..), RegisterArgument(..), ImmediateAddress(..))
import Binary as B


-- An op code can include just the op code or
-- an eight bit or sixteen bit immediate value


type OpCode
    = NullaryOpCode B.Byte
    | OneByteOpCode B.Byte B.Byte
    | TwoByteOpcode B.Byte B.Byte B.Byte


decode : OpCode -> Instruction
decode opCode =
    case opCode of
        NullaryOpCode byte ->
            case (B.binaryByteToHexByte byte) of
                B.HexByte B.H0 B.H0 ->
                    NOP

                B.HexByte B.H0 B.H2 ->
                    LDRegister (Address16 CPU.BC) (RegArg8 CPU.A)

                B.HexByte B.H0 B.H3 ->
                    INC (RegArg16 CPU.BC)

                B.HexByte B.H0 B.H4 ->
                    INC (RegArg8 CPU.B)

                B.HexByte B.H0 B.H5 ->
                    DEC (RegArg8 CPU.B)

                B.HexByte B.H0 B.H7 ->
                    RLCA

                B.HexByte B.H0 B.H9 ->
                    ADDHL (RegArg16 CPU.BC)

                B.HexByte B.H1 B.H9 ->
                    ADDHL (RegArg16 CPU.DE)

                B.HexByte B.H2 B.H9 ->
                    ADDHL (RegArg16 CPU.HL)

                B.HexByte B.H3 B.H9 ->
                    ADDHL (RegArg16 CPU.SP)

                B.HexByte B.H0 B.HA ->
                    LDRegister (RegArg16 CPU.BC) (RegArg8 CPU.A)

                _ ->
                    NOP

        OneByteOpCode byte byte2 ->
            case (B.binaryByteToHexByte byte) of
                ---- STOP ----
                -- 0x10
                B.HexByte B.H1 B.H0 ->
                    -- STOP 0
                    STOP

                ---- LD ----
                -- 0x06
                B.HexByte B.H0 B.H6 ->
                    -- LD B,d8
                    LDEightBitValue (RegArg8 CPU.B) byte2

                -- 0x16
                B.HexByte B.H1 B.H6 ->
                    -- LD D,d8
                    LDEightBitValue (RegArg8 CPU.D) byte2

                -- 0x26
                B.HexByte B.H2 B.H6 ->
                    -- LD H,d8
                    LDEightBitValue (RegArg8 CPU.H) byte2

                -- 0x36
                B.HexByte B.H3 B.H6 ->
                    -- LD (HL),d8
                    LDEightBitValue (Address16 CPU.HL) byte2

                -- 0x0E
                B.HexByte B.H0 B.HE ->
                    -- LD C,d8
                    LDEightBitValue (RegArg8 CPU.C) byte2

                -- 0x1E
                B.HexByte B.H1 B.HE ->
                    -- LD E,d8
                    LDEightBitValue (RegArg8 CPU.E) byte2

                -- 0x2E
                B.HexByte B.H2 B.HE ->
                    -- LD L,d8
                    LDEightBitValue (RegArg8 CPU.L) byte2

                -- 0x3E
                B.HexByte B.H3 B.HE ->
                    --LD A,d8
                    LDEightBitValue (RegArg8 CPU.A) byte2

                -- 0xE2
                B.HexByte B.HE B.H2 ->
                    -- LD (C),A
                    LDRegister (Address8 CPU.C) (RegArg8 CPU.A)

                -- 0xF2
                B.HexByte B.HF B.H2 ->
                    --LD A,(c)
                    LDRegister (RegArg8 CPU.A) (Address8 CPU.C)

                ---- LDH ----
                -- 0xE0
                B.HexByte B.HE B.H0 ->
                    --LDH (a8),A
                    LDHRegisterToImmediateAddress (ImmediateAddress8 byte2) (RegArg8 CPU.A)

                -- 0xF0
                B.HexByte B.HF B.H0 ->
                    --LDH A,(a8)
                    LDHImmediateAddressToRegister (RegArg8 CPU.A) (ImmediateAddress8 byte2)

                ---- PREFIX CB ----
                B.HexByte B.HC B.HB ->
                    case (B.binaryByteToHexByte byte2) of
                        ---- RLC ----
                        -- 0x00
                        B.HexByte B.H0 B.H0 ->
                            -- RLC B
                            RLC (RegArg8 CPU.B)

                        -- 0x01
                        B.HexByte B.H0 B.H1 ->
                            -- RLC C
                            RLC (RegArg8 CPU.C)

                        -- 0x02
                        B.HexByte B.H0 B.H2 ->
                            -- RLC D
                            RLC (RegArg8 CPU.D)

                        -- 0x03
                        B.HexByte B.H0 B.H3 ->
                            -- RLC E
                            RLC (RegArg8 CPU.E)

                        -- 0x04
                        B.HexByte B.H0 B.H4 ->
                            -- RLC H
                            RLC (RegArg8 CPU.H)

                        -- 0x05
                        B.HexByte B.H0 B.H5 ->
                            -- RLC L
                            RLC (RegArg8 CPU.L)

                        -- 0x06
                        B.HexByte B.H0 B.H6 ->
                            -- RLC (HL)
                            RLC (Address16 CPU.HL)

                        -- 0x07
                        B.HexByte B.H0 B.H7 ->
                            -- RLC A
                            RLC (RegArg8 CPU.A)

                        _ ->
                            NOP

                ---- RRC ----
                -- B.HexByte B.H0 B.H8 ->
                --     -- RRC B
                --     RRC Vkk
                _ ->
                    NOP

        TwoByteOpcode byte byte2 byte3 ->
            NOP
