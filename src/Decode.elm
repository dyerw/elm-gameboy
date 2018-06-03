module Decode exposing (..)

import CPU
    exposing
        ( Instruction(..)
        , RegisterArgument(..)
        , ImmediateAddress(..)
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

                -- 0x50
                B.HexByte B.H5 B.H0 ->
                    -- LD D, B
                    LDRegister (RegArg8 CPU.D) (RegArg8 CPU.B)

                -- 0x51
                B.HexByte B.H5 B.H1 ->
                    -- LD D, C
                    LDRegister (RegArg8 CPU.D) (RegArg8 CPU.C)

                -- 0x52
                B.HexByte B.H5 B.H2 ->
                    -- LD D, D
                    LDRegister (RegArg8 CPU.D) (RegArg8 CPU.D)

                -- 0x53
                B.HexByte B.H5 B.H3 ->
                    -- LD D, E
                    LDRegister (RegArg8 CPU.D) (RegArg8 CPU.E)

                -- 0x54
                B.HexByte B.H5 B.H4 ->
                    -- LD D, H
                    LDRegister (RegArg8 CPU.D) (RegArg8 CPU.H)

                -- 0x55
                B.HexByte B.H5 B.H5 ->
                    -- LD D, L
                    LDRegister (RegArg8 CPU.D) (RegArg8 CPU.L)

                -- 0x56
                B.HexByte B.H5 B.H6 ->
                    -- LD D, (HL)
                    LDRegister (RegArg8 CPU.D) (Address16 CPU.HL)

                -- 0x57
                B.HexByte B.H5 B.H7 ->
                    -- LD D, A
                    LDRegister (RegArg8 CPU.D) (RegArg8 CPU.A)

                -- 0x58
                B.HexByte B.H5 B.H8 ->
                    -- LD E, B
                    LDRegister (RegArg8 CPU.E) (RegArg8 CPU.B)

                -- 0x59
                B.HexByte B.H5 B.H9 ->
                    -- LD E, C
                    LDRegister (RegArg8 CPU.E) (RegArg8 CPU.C)

                -- 0x5A
                B.HexByte B.H5 B.HA ->
                    -- LD E, D
                    LDRegister (RegArg8 CPU.E) (RegArg8 CPU.D)

                -- 0x5B
                B.HexByte B.H5 B.HB ->
                    -- LD E, E
                    LDRegister (RegArg8 CPU.E) (RegArg8 CPU.E)

                -- 0x5C
                B.HexByte B.H5 B.HC ->
                    -- LD E, H
                    LDRegister (RegArg8 CPU.E) (RegArg8 CPU.H)

                -- 0x5D
                B.HexByte B.H5 B.HD ->
                    -- LD E, L
                    LDRegister (RegArg8 CPU.E) (RegArg8 CPU.L)

                -- 0x5E
                B.HexByte B.H5 B.HE ->
                    -- LD E, (HL)
                    LDRegister (RegArg8 CPU.E) (Address16 CPU.HL)

                -- 0x5F
                B.HexByte B.H5 B.HF ->
                    -- LD E, A
                    LDRegister (RegArg8 CPU.E) (RegArg8 CPU.A)

                -- 0x60
                B.HexByte B.H6 B.H0 ->
                    -- LD H, B
                    LDRegister (RegArg8 CPU.H) (RegArg8 CPU.B)

                -- 0x61
                B.HexByte B.H6 B.H1 ->
                    -- LD H, C
                    LDRegister (RegArg8 CPU.H) (RegArg8 CPU.C)

                -- 0x62
                B.HexByte B.H6 B.H2 ->
                    -- LD H, D
                    LDRegister (RegArg8 CPU.H) (RegArg8 CPU.D)

                -- 0x63
                B.HexByte B.H6 B.H3 ->
                    -- LD H, E
                    LDRegister (RegArg8 CPU.H) (RegArg8 CPU.E)

                -- 0x64
                B.HexByte B.H6 B.H4 ->
                    -- LD H, H
                    LDRegister (RegArg8 CPU.H) (RegArg8 CPU.H)

                -- 0x65
                B.HexByte B.H6 B.H5 ->
                    -- LD H, L
                    LDRegister (RegArg8 CPU.H) (RegArg8 CPU.L)

                -- 0x66
                B.HexByte B.H6 B.H6 ->
                    -- LD H, (HL)
                    LDRegister (RegArg8 CPU.H) (Address16 CPU.HL)

                -- 0x67
                B.HexByte B.H6 B.H7 ->
                    -- LD H, A
                    LDRegister (RegArg8 CPU.H) (RegArg8 CPU.A)

                -- 0x68
                B.HexByte B.H6 B.H8 ->
                    -- LD L, B
                    LDRegister (RegArg8 CPU.L) (RegArg8 CPU.B)

                -- 0x69
                B.HexByte B.H6 B.H9 ->
                    -- LD L, C
                    LDRegister (RegArg8 CPU.L) (RegArg8 CPU.C)

                -- 0x6A
                B.HexByte B.H6 B.HA ->
                    -- LD L, D
                    LDRegister (RegArg8 CPU.L) (RegArg8 CPU.D)

                -- 0x6B
                B.HexByte B.H6 B.HB ->
                    -- LD L, E
                    LDRegister (RegArg8 CPU.L) (RegArg8 CPU.E)

                -- 0x6C
                B.HexByte B.H6 B.HC ->
                    -- LD L, H
                    LDRegister (RegArg8 CPU.L) (RegArg8 CPU.H)

                -- 0x6D
                B.HexByte B.H6 B.HD ->
                    -- LD L, L
                    LDRegister (RegArg8 CPU.L) (RegArg8 CPU.L)

                -- 0x6E
                B.HexByte B.H6 B.HE ->
                    -- LD L, (HL)
                    LDRegister (RegArg8 CPU.L) (Address16 CPU.HL)

                -- 0x6F
                B.HexByte B.H6 B.HF ->
                    -- LD L, A
                    LDRegister (RegArg8 CPU.L) (RegArg8 CPU.A)

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
