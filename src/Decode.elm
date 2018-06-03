module Decode exposing (..)

import CPU
    exposing
        ( Instruction(..)
        , RegisterArgument(RegArg8, RegArg16, Address8, Address16)
        )
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
                -- 0x00
                B.HexByte B.H0 B.H0 ->
                    -- NOP
                    NOP

                ---- LD ----
                -- 0x02
                B.HexByte B.H0 B.H2 ->
                    -- LD (BC), A
                    LDRegister (Address16 CPU.BC) (RegArg8 CPU.A)

                -- 0x12
                B.HexByte B.H1 B.H2 ->
                    -- LD (DE), A
                    LDRegister (Address16 CPU.DE) (RegArg8 CPU.A)

                -- 0x22
                B.HexByte B.H2 B.H2 ->
                    -- LDI (HL), A
                    LDI (Address16 CPU.HL) (RegArg8 CPU.A)

                -- 0x32
                B.HexByte B.H3 B.H2 ->
                    -- LDD (HL), A
                    LDD (Address16 CPU.HL) (RegArg8 CPU.A)

                -- 0x40
                B.HexByte B.H4 B.H0 ->
                    -- LD B, B
                    LDRegister (RegArg8 CPU.B) (RegArg8 CPU.B)

                -- 0x41
                B.HexByte B.H4 B.H1 ->
                    -- LD B, C
                    LDRegister (RegArg8 CPU.B) (RegArg8 CPU.C)

                -- 0x42
                B.HexByte B.H4 B.H2 ->
                    -- LD B, D
                    LDRegister (RegArg8 CPU.B) (RegArg8 CPU.D)

                -- 0x43
                B.HexByte B.H4 B.H3 ->
                    -- LD B, E
                    LDRegister (RegArg8 CPU.B) (RegArg8 CPU.E)

                -- 0x44
                B.HexByte B.H4 B.H4 ->
                    -- LD B, H
                    LDRegister (RegArg8 CPU.B) (RegArg8 CPU.H)

                -- 0x45
                B.HexByte B.H4 B.H5 ->
                    -- LD B, L
                    LDRegister (RegArg8 CPU.B) (RegArg8 CPU.L)

                -- 0x46
                B.HexByte B.H4 B.H6 ->
                    -- LD B, (HL)
                    LDRegister (RegArg8 CPU.B) (Address16 CPU.HL)

                -- 0x47
                B.HexByte B.H4 B.H7 ->
                    -- LD B, A
                    LDRegister (RegArg8 CPU.B) (RegArg8 CPU.A)

                -- 0x48
                B.HexByte B.H4 B.H8 ->
                    -- LD C, B
                    LDRegister (RegArg8 CPU.C) (RegArg8 CPU.B)

                -- 0x49
                B.HexByte B.H4 B.H9 ->
                    -- LD C, C
                    LDRegister (RegArg8 CPU.C) (RegArg8 CPU.C)

                -- 0x4A
                B.HexByte B.H4 B.HA ->
                    -- LD C, D
                    LDRegister (RegArg8 CPU.C) (RegArg8 CPU.D)

                -- 0x4B
                B.HexByte B.H4 B.HB ->
                    -- LD C, E
                    LDRegister (RegArg8 CPU.C) (RegArg8 CPU.E)

                -- 0x4C
                B.HexByte B.H4 B.HC ->
                    -- LD C, H
                    LDRegister (RegArg8 CPU.C) (RegArg8 CPU.H)

                -- 0x4D
                B.HexByte B.H4 B.HD ->
                    -- LD C, L
                    LDRegister (RegArg8 CPU.C) (RegArg8 CPU.L)

                -- 0x4E
                B.HexByte B.H4 B.HE ->
                    -- LD C, (HL)
                    LDRegister (RegArg8 CPU.C) (Address16 CPU.HL)

                -- 0x4F
                B.HexByte B.H4 B.HF ->
                    -- LD C, A
                    LDRegister (RegArg8 CPU.C) (RegArg8 CPU.A)

                ---- INC ----
                -- 0x03
                B.HexByte B.H0 B.H3 ->
                    -- INC BC
                    INC (RegArg16 CPU.BC)

                -- 0x13
                B.HexByte B.H1 B.H3 ->
                    -- INC DE
                    INC (RegArg16 CPU.DE)

                -- 0x04
                B.HexByte B.H0 B.H4 ->
                    -- INC B
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
            NOP

        TwoByteOpcode byte byte2 byte3 ->
            NOP
