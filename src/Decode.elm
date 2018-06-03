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

                -- 0x70
                B.HexByte B.H7 B.H0 ->
                    -- LD (HL), B
                    LDRegister (Address16 CPU.HL) (RegArg8 CPU.B)

                -- 0x71
                B.HexByte B.H7 B.H1 ->
                    -- LD (HL), C
                    LDRegister (Address16 CPU.HL) (RegArg8 CPU.C)

                -- 0x72
                B.HexByte B.H7 B.H2 ->
                    -- LD (HL), D
                    LDRegister (Address16 CPU.HL) (RegArg8 CPU.D)

                -- 0x73
                B.HexByte B.H7 B.H3 ->
                    -- LD (HL), E
                    LDRegister (Address16 CPU.HL) (RegArg8 CPU.E)

                -- 0x74
                B.HexByte B.H7 B.H4 ->
                    -- LD (HL), H
                    LDRegister (Address16 CPU.HL) (RegArg8 CPU.H)

                -- 0x75
                B.HexByte B.H7 B.H5 ->
                    -- LD (HL), L
                    LDRegister (Address16 CPU.HL) (RegArg8 CPU.L)

                -- 0x76
                B.HexByte B.H7 B.H6 ->
                    -- HALT
                    HALT

                -- 0x77
                B.HexByte B.H7 B.H7 ->
                    -- LD (HL), A
                    LDRegister (Address16 CPU.HL) (RegArg8 CPU.A)

                -- 0x78
                B.HexByte B.H7 B.H8 ->
                    -- LD A, B
                    LDRegister (RegArg8 CPU.A) (RegArg8 CPU.B)

                -- 0x79
                B.HexByte B.H7 B.H9 ->
                    -- LD A, C
                    LDRegister (RegArg8 CPU.A) (RegArg8 CPU.C)

                -- 0x7A
                B.HexByte B.H7 B.HA ->
                    -- LD A, D
                    LDRegister (RegArg8 CPU.A) (RegArg8 CPU.D)

                -- 0x7B
                B.HexByte B.H7 B.HB ->
                    -- LD A, E
                    LDRegister (RegArg8 CPU.A) (RegArg8 CPU.E)

                -- 0x7C
                B.HexByte B.H7 B.HC ->
                    -- LD A, H
                    LDRegister (RegArg8 CPU.A) (RegArg8 CPU.H)

                -- 0x7D
                B.HexByte B.H7 B.HD ->
                    -- LD A, L
                    LDRegister (RegArg8 CPU.A) (RegArg8 CPU.L)

                -- 0x7E
                B.HexByte B.H7 B.HE ->
                    -- LD A, (HL)
                    LDRegister (RegArg8 CPU.A) (Address16 CPU.HL)

                -- 0x7F
                B.HexByte B.H7 B.HF ->
                    -- LD A, A
                    LDRegister (RegArg8 CPU.A) (RegArg8 CPU.A)

                ---- ADD ----
                -- 0x80
                B.HexByte B.H8 B.H0 ->
                    -- ADD A, B
                    ADD (RegArg8 CPU.B)

                -- 0x81
                B.HexByte B.H8 B.H1 ->
                    -- ADD A, C
                    ADD (RegArg8 CPU.C)

                -- 0x82
                B.HexByte B.H8 B.H2 ->
                    -- ADD A, D
                    ADD (RegArg8 CPU.D)

                -- 0x83
                B.HexByte B.H8 B.H3 ->
                    -- ADD A, E
                    ADD (RegArg8 CPU.E)

                -- 0x84
                B.HexByte B.H8 B.H4 ->
                    -- ADD A, H
                    ADD (RegArg8 CPU.H)

                -- 0x85
                B.HexByte B.H8 B.H5 ->
                    -- ADD A, L
                    ADD (RegArg8 CPU.L)

                -- 0x86
                B.HexByte B.H8 B.H6 ->
                    -- ADD A, (HL)
                    ADD (Address16 CPU.HL)

                -- 0x87
                B.HexByte B.H8 B.H7 ->
                    -- ADD A, A
                    ADD (RegArg8 CPU.A)

                -- 0x88
                B.HexByte B.H8 B.H8 ->
                    -- ADC A, B
                    ADC (RegArg8 CPU.B)

                -- 0x89
                B.HexByte B.H8 B.H9 ->
                    -- ADC A, C
                    ADC (RegArg8 CPU.C)

                -- 0x8A
                B.HexByte B.H8 B.HA ->
                    -- ADC A, D
                    ADC (RegArg8 CPU.D)

                -- 0x8B
                B.HexByte B.H8 B.HB ->
                    -- ADC A, E
                    ADC (RegArg8 CPU.E)

                -- 0x8C
                B.HexByte B.H8 B.HC ->
                    -- ADC A, H
                    ADC (RegArg8 CPU.H)

                -- 0x8D
                B.HexByte B.H8 B.HD ->
                    -- ADC A, L
                    ADC (RegArg8 CPU.L)

                -- 0x8E
                B.HexByte B.H8 B.HE ->
                    -- ADC A, (HL)
                    ADC (Address16 CPU.HL)

                -- 0x8F
                B.HexByte B.H8 B.HF ->
                    -- ADC A, A
                    ADC (RegArg8 CPU.C)

                ---- INC ----
                -- 0x03
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

                        ---- RRC ----
                        -- 0x08
                        B.HexByte B.H0 B.H8 ->
                            -- RRC B
                            RRC (RegArg8 CPU.B)

                        -- 0x09
                        B.HexByte B.H0 B.H9 ->
                            -- RRC C
                            RRC (RegArg8 CPU.C)

                        -- 0x0A
                        B.HexByte B.H0 B.HA ->
                            -- RRC D
                            RRC (RegArg8 CPU.D)

                        -- 0x0B
                        B.HexByte B.H0 B.HB ->
                            -- RRC E
                            RRC (RegArg8 CPU.E)

                        -- 0x0C
                        B.HexByte B.H0 B.HC ->
                            -- RRC H
                            RRC (RegArg8 CPU.H)

                        -- 0x0D
                        B.HexByte B.H0 B.HD ->
                            -- RRC L
                            RRC (RegArg8 CPU.L)

                        -- 0x0E
                        B.HexByte B.H0 B.HE ->
                            -- RRC (HL)
                            RRC (Address16 CPU.HL)

                        -- 0x0F
                        B.HexByte B.H0 B.HF ->
                            -- RRC A
                            RRC (RegArg8 CPU.A)

                        ---- RL ----
                        -- 0x10
                        B.HexByte B.H1 B.H0 ->
                            NOP

                        B.HexByte B.H1 B.H1 ->
                            NOP

                        -- 0x12
                        B.HexByte B.H1 B.H2 ->
                            NOP

                        -- 0x13
                        B.HexByte B.H1 B.H3 ->
                            NOP

                        -- 0x14
                        B.HexByte B.H1 B.H4 ->
                            NOP

                        -- 0x15
                        B.HexByte B.H1 B.H5 ->
                            NOP

                        -- 0x16
                        B.HexByte B.H1 B.H6 ->
                            NOP

                        -- 0x17
                        B.HexByte B.H1 B.H7 ->
                            NOP

                        ---- RR ----
                        -- 0x18
                        B.HexByte B.H1 B.H8 ->
                            NOP

                        -- 0x19
                        B.HexByte B.H1 B.H9 ->
                            NOP

                        -- 0x1A
                        B.HexByte B.H1 B.HA ->
                            NOP

                        -- 0x1B
                        B.HexByte B.H1 B.HB ->
                            NOP

                        -- 0x1C
                        B.HexByte B.H1 B.HC ->
                            NOP

                        -- 0x1D
                        B.HexByte B.H1 B.HD ->
                            NOP

                        -- 0x1E
                        B.HexByte B.H1 B.HE ->
                            NOP

                        -- 0x1F
                        B.HexByte B.H1 B.HF ->
                            NOP

                        -- 0x20
                        B.HexByte B.H2 B.H0 ->
                            NOP

                        -- 0x21
                        B.HexByte B.H2 B.H1 ->
                            NOP

                        -- 0x22
                        B.HexByte B.H2 B.H2 ->
                            NOP

                        -- 0x23
                        B.HexByte B.H2 B.H3 ->
                            NOP

                        -- 0x24
                        B.HexByte B.H2 B.H4 ->
                            NOP

                        -- 0x25
                        B.HexByte B.H2 B.H5 ->
                            NOP

                        -- 0x26
                        B.HexByte B.H2 B.H6 ->
                            NOP

                        -- 0x27
                        B.HexByte B.H2 B.H7 ->
                            NOP

                        -- 0x28
                        B.HexByte B.H2 B.H8 ->
                            NOP

                        -- 0x29
                        B.HexByte B.H2 B.H9 ->
                            NOP

                        -- 0x2A
                        B.HexByte B.H2 B.HA ->
                            NOP

                        -- 0x2B
                        B.HexByte B.H2 B.HB ->
                            NOP

                        -- 0x2C
                        B.HexByte B.H2 B.HC ->
                            NOP

                        -- 0x2D
                        B.HexByte B.H2 B.HD ->
                            NOP

                        -- 0x2E
                        B.HexByte B.H2 B.HE ->
                            NOP

                        -- 0x2F
                        B.HexByte B.H2 B.HF ->
                            NOP

                        -- 0x30
                        B.HexByte B.H3 B.H0 ->
                            NOP

                        -- 0x31
                        B.HexByte B.H3 B.H1 ->
                            NOP

                        -- 0x32
                        B.HexByte B.H3 B.H2 ->
                            NOP

                        -- 0x33
                        B.HexByte B.H3 B.H3 ->
                            NOP

                        -- 0x34
                        B.HexByte B.H3 B.H4 ->
                            NOP

                        -- 0x35
                        B.HexByte B.H3 B.H5 ->
                            NOP

                        -- 0x36
                        B.HexByte B.H3 B.H6 ->
                            NOP

                        -- 0x37
                        B.HexByte B.H3 B.H7 ->
                            NOP

                        ---- SRL ----
                        -- 0x38
                        B.HexByte B.H3 B.H8 ->
                            NOP

                        -- 0x39
                        B.HexByte B.H3 B.H9 ->
                            NOP

                        -- 0x3A
                        B.HexByte B.H3 B.HA ->
                            NOP

                        -- 0x3B
                        B.HexByte B.H3 B.HB ->
                            NOP

                        -- 0x3C
                        B.HexByte B.H3 B.HC ->
                            NOP

                        -- 0x3D
                        B.HexByte B.H3 B.HD ->
                            NOP

                        -- 0x3E
                        B.HexByte B.H3 B.HE ->
                            NOP

                        -- 0x3F
                        B.HexByte B.H3 B.HF ->
                            NOP

                        ---- BIT ----
                        -- 0x40
                        B.HexByte B.H4 B.H0 ->
                            NOP

                        -- 0x41
                        B.HexByte B.H4 B.H1 ->
                            NOP

                        -- 0x42
                        B.HexByte B.H4 B.H2 ->
                            NOP

                        -- 0x43
                        B.HexByte B.H4 B.H3 ->
                            NOP

                        -- 0x44
                        B.HexByte B.H4 B.H4 ->
                            NOP

                        -- 0x45
                        B.HexByte B.H4 B.H5 ->
                            NOP

                        -- 0x46
                        B.HexByte B.H4 B.H6 ->
                            NOP

                        -- 0x47
                        B.HexByte B.H4 B.H7 ->
                            NOP

                        -- 0x48
                        B.HexByte B.H4 B.H8 ->
                            NOP

                        -- 0x49
                        B.HexByte B.H4 B.H9 ->
                            NOP

                        -- 0x4A
                        B.HexByte B.H4 B.HA ->
                            NOP

                        -- 0x4B
                        B.HexByte B.H4 B.HB ->
                            NOP

                        -- 0x4C
                        B.HexByte B.H4 B.HC ->
                            NOP

                        -- 0x4D
                        B.HexByte B.H4 B.HD ->
                            NOP

                        -- 0x4E
                        B.HexByte B.H4 B.HE ->
                            NOP

                        -- 0x4F
                        B.HexByte B.H4 B.HF ->
                            NOP

                        -- 0x50
                        B.HexByte B.H5 B.H0 ->
                            NOP

                        -- 0x51
                        B.HexByte B.H5 B.H1 ->
                            NOP

                        -- 0x52
                        B.HexByte B.H5 B.H2 ->
                            NOP

                        -- 0x53
                        B.HexByte B.H5 B.H3 ->
                            NOP

                        -- 0x54
                        B.HexByte B.H5 B.H4 ->
                            NOP

                        -- 0x55
                        B.HexByte B.H5 B.H5 ->
                            NOP

                        -- 0x56
                        B.HexByte B.H5 B.H6 ->
                            NOP

                        -- 0x57
                        B.HexByte B.H5 B.H7 ->
                            NOP

                        -- 0x58
                        B.HexByte B.H5 B.H8 ->
                            NOP

                        -- 0x59
                        B.HexByte B.H5 B.H9 ->
                            NOP

                        -- 0x5A
                        B.HexByte B.H5 B.HA ->
                            NOP

                        -- 0x5B
                        B.HexByte B.H5 B.HB ->
                            NOP

                        -- 0x5C
                        B.HexByte B.H5 B.HC ->
                            NOP

                        -- 0x5D
                        B.HexByte B.H5 B.HD ->
                            NOP

                        -- 0x5E
                        B.HexByte B.H5 B.HE ->
                            NOP

                        -- 0x5F
                        B.HexByte B.H5 B.HF ->
                            NOP

                        -- 0x60
                        B.HexByte B.H6 B.H0 ->
                            NOP

                        -- 0x61
                        B.HexByte B.H6 B.H1 ->
                            NOP

                        -- 0x62
                        B.HexByte B.H6 B.H2 ->
                            NOP

                        -- 0x63
                        B.HexByte B.H6 B.H3 ->
                            NOP

                        -- 0x64
                        B.HexByte B.H6 B.H4 ->
                            NOP

                        -- 0x65
                        B.HexByte B.H6 B.H5 ->
                            NOP

                        -- 0x66
                        B.HexByte B.H6 B.H6 ->
                            NOP

                        -- 0x67
                        B.HexByte B.H6 B.H7 ->
                            NOP

                        -- 0x68
                        B.HexByte B.H6 B.H8 ->
                            NOP

                        -- 0x69
                        B.HexByte B.H6 B.H9 ->
                            NOP

                        -- 0x6A
                        B.HexByte B.H6 B.HA ->
                            NOP

                        -- 0x6B
                        B.HexByte B.H6 B.HB ->
                            NOP

                        -- 0x6C
                        B.HexByte B.H6 B.HC ->
                            NOP

                        -- 0x6D
                        B.HexByte B.H6 B.HD ->
                            NOP

                        -- 0x6E
                        B.HexByte B.H6 B.HE ->
                            NOP

                        -- 0x6F
                        B.HexByte B.H6 B.HF ->
                            NOP

                        -- 0x70
                        B.HexByte B.H7 B.H0 ->
                            NOP

                        -- 0x71
                        B.HexByte B.H7 B.H1 ->
                            NOP

                        -- 0x72
                        B.HexByte B.H7 B.H2 ->
                            NOP

                        -- 0x73
                        B.HexByte B.H7 B.H3 ->
                            NOP

                        -- 0x74
                        B.HexByte B.H7 B.H4 ->
                            NOP

                        -- 0x75
                        B.HexByte B.H7 B.H5 ->
                            NOP

                        -- 0x76
                        B.HexByte B.H7 B.H6 ->
                            NOP

                        -- 0x77
                        B.HexByte B.H7 B.H7 ->
                            NOP

                        -- 0x78
                        B.HexByte B.H7 B.H8 ->
                            NOP

                        -- 0x79
                        B.HexByte B.H7 B.H9 ->
                            NOP

                        -- 0x7A
                        B.HexByte B.H7 B.HA ->
                            NOP

                        -- 0x7B
                        B.HexByte B.H7 B.HB ->
                            NOP

                        -- 0x7C
                        B.HexByte B.H7 B.HC ->
                            NOP

                        -- 0x7D
                        B.HexByte B.H7 B.HD ->
                            NOP

                        -- 0x7E
                        B.HexByte B.H7 B.HE ->
                            NOP

                        -- 0x7F
                        B.HexByte B.H7 B.HF ->
                            NOP

                        ---- RES ----
                        -- 0x80
                        B.HexByte B.H8 B.H0 ->
                            NOP

                        -- 0x81
                        B.HexByte B.H8 B.H1 ->
                            NOP

                        -- 0x82
                        B.HexByte B.H8 B.H2 ->
                            NOP

                        -- 0x83
                        B.HexByte B.H8 B.H3 ->
                            NOP

                        -- 0x84
                        B.HexByte B.H8 B.H4 ->
                            NOP

                        -- 0x85
                        B.HexByte B.H8 B.H5 ->
                            NOP

                        -- 0x86
                        B.HexByte B.H8 B.H6 ->
                            NOP

                        -- 0x87
                        B.HexByte B.H8 B.H7 ->
                            NOP

                        -- 0x88
                        B.HexByte B.H8 B.H8 ->
                            NOP

                        -- 0x89
                        B.HexByte B.H8 B.H9 ->
                            NOP

                        -- 0x8A
                        B.HexByte B.H8 B.HA ->
                            NOP

                        -- 0x8B
                        B.HexByte B.H8 B.HB ->
                            NOP

                        -- 0x8C
                        B.HexByte B.H8 B.HC ->
                            NOP

                        -- 0x8D
                        B.HexByte B.H8 B.HD ->
                            NOP

                        -- 0x8E
                        B.HexByte B.H8 B.HE ->
                            NOP

                        -- 0x8F
                        B.HexByte B.H8 B.HF ->
                            NOP

                        -- 0x90
                        B.HexByte B.H9 B.H0 ->
                            NOP

                        -- 0x91
                        B.HexByte B.H9 B.H1 ->
                            NOP

                        -- 0x92
                        B.HexByte B.H9 B.H2 ->
                            NOP

                        -- 0x93
                        B.HexByte B.H9 B.H3 ->
                            NOP

                        -- 0x94
                        B.HexByte B.H9 B.H4 ->
                            NOP

                        -- 0x95
                        B.HexByte B.H9 B.H5 ->
                            NOP

                        -- 0x96
                        B.HexByte B.H9 B.H6 ->
                            NOP

                        -- 0x97
                        B.HexByte B.H9 B.H7 ->
                            NOP

                        -- 0x98
                        B.HexByte B.H9 B.H8 ->
                            NOP

                        -- 0x99
                        B.HexByte B.H9 B.H9 ->
                            NOP

                        -- 0x9A
                        B.HexByte B.H9 B.HA ->
                            NOP

                        -- 0x9B
                        B.HexByte B.H9 B.HB ->
                            NOP

                        -- 0x9C
                        B.HexByte B.H9 B.HC ->
                            NOP

                        -- 0x9D
                        B.HexByte B.H9 B.HD ->
                            NOP

                        -- 0x9E
                        B.HexByte B.H9 B.HE ->
                            NOP

                        -- 0x9F
                        B.HexByte B.H9 B.HF ->
                            NOP

                        -- 0xA0
                        B.HexByte B.HA B.H0 ->
                            NOP

                        -- 0xA1
                        B.HexByte B.HA B.H1 ->
                            NOP

                        -- 0xA2
                        B.HexByte B.HA B.H2 ->
                            NOP

                        -- 0xA3
                        B.HexByte B.HA B.H3 ->
                            NOP

                        -- 0xA4
                        B.HexByte B.HA B.H4 ->
                            NOP

                        -- 0xA5
                        B.HexByte B.HA B.H5 ->
                            NOP

                        -- 0xA6
                        B.HexByte B.HA B.H6 ->
                            NOP

                        -- 0xA7
                        B.HexByte B.HA B.H7 ->
                            NOP

                        -- 0xA8
                        B.HexByte B.HA B.H8 ->
                            NOP

                        -- 0xA9
                        B.HexByte B.HA B.H9 ->
                            NOP

                        -- 0xAA
                        B.HexByte B.HA B.HA ->
                            NOP

                        -- 0xAB
                        B.HexByte B.HA B.HB ->
                            NOP

                        -- 0xAC
                        B.HexByte B.HA B.HC ->
                            NOP

                        -- 0xAD
                        B.HexByte B.HA B.HD ->
                            NOP

                        -- 0xAE
                        B.HexByte B.HA B.HE ->
                            NOP

                        -- 0xAF
                        B.HexByte B.HA B.HF ->
                            NOP

                        -- 0xB0
                        B.HexByte B.HB B.H0 ->
                            NOP

                        -- 0xB1
                        B.HexByte B.HB B.H1 ->
                            NOP

                        -- 0xB2
                        B.HexByte B.HB B.H2 ->
                            NOP

                        -- 0xB3
                        B.HexByte B.HB B.H3 ->
                            NOP

                        -- 0xB4
                        B.HexByte B.HB B.H4 ->
                            NOP

                        -- 0xB5
                        B.HexByte B.HB B.H5 ->
                            NOP

                        -- 0xB6
                        B.HexByte B.HB B.H6 ->
                            NOP

                        -- 0xB7
                        B.HexByte B.HB B.H7 ->
                            NOP

                        -- 0xB8
                        B.HexByte B.HB B.H8 ->
                            NOP

                        -- 0xB9
                        B.HexByte B.HB B.H9 ->
                            NOP

                        -- 0xBA
                        B.HexByte B.HB B.HA ->
                            NOP

                        -- 0xBB
                        B.HexByte B.HB B.HB ->
                            NOP

                        -- 0xBC
                        B.HexByte B.HB B.HC ->
                            NOP

                        -- 0xBD
                        B.HexByte B.HB B.HD ->
                            NOP

                        -- 0xBE
                        B.HexByte B.HB B.HE ->
                            NOP

                        -- 0xBF
                        B.HexByte B.HB B.HF ->
                            NOP

                        ---- SET ----
                        -- 0xC0
                        B.HexByte B.HC B.H0 ->
                            NOP

                        -- 0xC1
                        B.HexByte B.HC B.H1 ->
                            NOP

                        -- 0xC2
                        B.HexByte B.HC B.H2 ->
                            NOP

                        -- 0xC3
                        B.HexByte B.HC B.H3 ->
                            NOP

                        -- 0xC4
                        B.HexByte B.HC B.H4 ->
                            NOP

                        -- 0xC5
                        B.HexByte B.HC B.H5 ->
                            NOP

                        -- 0xC6
                        B.HexByte B.HC B.H6 ->
                            NOP

                        -- 0xC7
                        B.HexByte B.HC B.H7 ->
                            NOP

                        -- 0xC8
                        B.HexByte B.HC B.H8 ->
                            NOP

                        -- 0xC9
                        B.HexByte B.HC B.H9 ->
                            NOP

                        -- 0xCA
                        B.HexByte B.HC B.HA ->
                            NOP

                        -- 0xCB
                        B.HexByte B.HC B.HB ->
                            NOP

                        -- 0xCC
                        B.HexByte B.HC B.HC ->
                            NOP

                        -- 0xCD
                        B.HexByte B.HC B.HD ->
                            NOP

                        -- 0xCE
                        B.HexByte B.HC B.HE ->
                            NOP

                        -- 0xCF
                        B.HexByte B.HC B.HF ->
                            NOP

                        -- 0xD0
                        B.HexByte B.HD B.H0 ->
                            NOP

                        -- 0xD1
                        B.HexByte B.HD B.H1 ->
                            NOP

                        -- 0xD2
                        B.HexByte B.HD B.H2 ->
                            NOP

                        -- 0xD3
                        B.HexByte B.HD B.H3 ->
                            NOP

                        -- 0xD4
                        B.HexByte B.HD B.H4 ->
                            NOP

                        -- 0xD5
                        B.HexByte B.HD B.H5 ->
                            NOP

                        -- 0xD6
                        B.HexByte B.HD B.H6 ->
                            NOP

                        -- 0xD7
                        B.HexByte B.HD B.H7 ->
                            NOP

                        -- 0xD8
                        B.HexByte B.HD B.H8 ->
                            NOP

                        -- 0xD9
                        B.HexByte B.HD B.H9 ->
                            NOP

                        -- 0xDA
                        B.HexByte B.HD B.HA ->
                            NOP

                        -- 0xDB
                        B.HexByte B.HD B.HB ->
                            NOP

                        -- 0xDC
                        B.HexByte B.HD B.HC ->
                            NOP

                        -- 0xDD
                        B.HexByte B.HD B.HD ->
                            NOP

                        -- 0xDE
                        B.HexByte B.HD B.HE ->
                            NOP

                        -- 0xDF
                        B.HexByte B.HD B.HF ->
                            NOP

                        -- 0xE0
                        B.HexByte B.HE B.H0 ->
                            NOP

                        -- 0xE1
                        B.HexByte B.HE B.H1 ->
                            NOP

                        -- 0xE2
                        B.HexByte B.HE B.H2 ->
                            NOP

                        -- 0xE3
                        B.HexByte B.HE B.H3 ->
                            NOP

                        -- 0xE4
                        B.HexByte B.HE B.H4 ->
                            NOP

                        -- 0xE5
                        B.HexByte B.HE B.H5 ->
                            NOP

                        -- 0xE6
                        B.HexByte B.HE B.H6 ->
                            NOP

                        -- 0xE7
                        B.HexByte B.HE B.H7 ->
                            NOP

                        -- 0xE8
                        B.HexByte B.HE B.H8 ->
                            NOP

                        -- 0xE9
                        B.HexByte B.HE B.H9 ->
                            NOP

                        -- 0xEA
                        B.HexByte B.HE B.HA ->
                            NOP

                        -- 0xEB
                        B.HexByte B.HE B.HB ->
                            NOP

                        -- 0xEC
                        B.HexByte B.HE B.HC ->
                            NOP

                        -- 0xED
                        B.HexByte B.HE B.HD ->
                            NOP

                        -- 0xEE
                        B.HexByte B.HE B.HE ->
                            NOP

                        -- 0xEF
                        B.HexByte B.HE B.HF ->
                            NOP

                        -- 0xF0
                        B.HexByte B.HF B.H0 ->
                            NOP

                        -- 0xF1
                        B.HexByte B.HF B.H1 ->
                            NOP

                        -- 0xF2
                        B.HexByte B.HF B.H2 ->
                            NOP

                        -- 0xF3
                        B.HexByte B.HF B.H3 ->
                            NOP

                        -- 0xF4
                        B.HexByte B.HF B.H4 ->
                            NOP

                        -- 0xF5
                        B.HexByte B.HF B.H5 ->
                            NOP

                        -- 0xF6
                        B.HexByte B.HF B.H6 ->
                            NOP

                        -- 0xF7
                        B.HexByte B.HF B.H7 ->
                            NOP

                        -- 0xF8
                        B.HexByte B.HF B.H8 ->
                            NOP

                        -- 0xF9
                        B.HexByte B.HF B.H9 ->
                            NOP

                        -- 0xFA
                        B.HexByte B.HF B.HA ->
                            NOP

                        -- 0xFB
                        B.HexByte B.HF B.HB ->
                            NOP

                        -- 0xFC
                        B.HexByte B.HF B.HC ->
                            NOP

                        -- 0xFD
                        B.HexByte B.HF B.HD ->
                            NOP

                        -- 0xFE
                        B.HexByte B.HF B.HE ->
                            NOP

                        -- 0xFF
                        B.HexByte B.HF B.HF ->
                            NOP

                _ ->
                    NOP

        TwoByteOpcode byte byte2 byte3 ->
            NOP
