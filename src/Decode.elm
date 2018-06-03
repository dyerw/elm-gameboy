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

                ---- SUB ----
                -- 0x90
                B.HexByte B.H9 B.H0 ->
                    -- SUB A, B
                    SUB (RegArg8 CPU.B)

                -- 0x91
                B.HexByte B.H9 B.H1 ->
                    -- SUB A, C
                    SUB (RegArg8 CPU.C)

                -- 0x92
                B.HexByte B.H9 B.H2 ->
                    -- SUB A, D
                    SUB (RegArg8 CPU.D)

                -- 0x93
                B.HexByte B.H9 B.H3 ->
                    -- SUB A, E
                    SUB (RegArg8 CPU.E)

                -- 0x94
                B.HexByte B.H9 B.H4 ->
                    -- SUB A, H
                    SUB (RegArg8 CPU.H)

                -- 0x95
                B.HexByte B.H9 B.H5 ->
                    -- SUB A, L
                    SUB (RegArg8 CPU.L)

                -- 0x96
                B.HexByte B.H9 B.H6 ->
                    -- SUB A, (HL)
                    SUB (Address16 CPU.HL)

                -- 0x97
                B.HexByte B.H9 B.H7 ->
                    -- SUB A, A
                    SUB (RegArg8 CPU.A)

                -- 0x98
                B.HexByte B.H9 B.H8 ->
                    -- SBC A, B
                    SBC (RegArg8 CPU.B)

                -- 0x99
                B.HexByte B.H9 B.H9 ->
                    -- SBC A, C
                    SBC (RegArg8 CPU.C)

                -- 0x9A
                B.HexByte B.H9 B.HA ->
                    -- SBC A, D
                    SBC (RegArg8 CPU.D)

                -- 0x9B
                B.HexByte B.H9 B.HB ->
                    -- SBC A, E
                    SBC (RegArg8 CPU.E)

                -- 0x9C
                B.HexByte B.H9 B.HC ->
                    -- SBC A, H
                    SBC (RegArg8 CPU.H)

                -- 0x9D
                B.HexByte B.H9 B.HD ->
                    -- SBC A, L
                    SBC (RegArg8 CPU.L)

                -- 0x9E
                B.HexByte B.H9 B.HE ->
                    -- SBC A, (HL)
                    SBC (Address16 CPU.HL)

                -- 0x9F
                B.HexByte B.H9 B.HF ->
                    -- SBC A, A
                    SBC (RegArg8 CPU.C)

                ---- SUB ----
                -- 0xA0
                B.HexByte B.HA B.H0 ->
                    -- AND B
                    AND (RegArg8 CPU.B)

                -- 0xA1
                B.HexByte B.HA B.H1 ->
                    -- AND C
                    AND (RegArg8 CPU.C)

                -- 0xA2
                B.HexByte B.HA B.H2 ->
                    -- AND D
                    AND (RegArg8 CPU.D)

                -- 0xA3
                B.HexByte B.HA B.H3 ->
                    -- AND E
                    AND (RegArg8 CPU.E)

                -- 0xA4
                B.HexByte B.HA B.H4 ->
                    -- AND H
                    AND (RegArg8 CPU.H)

                -- 0xA5
                B.HexByte B.HA B.H5 ->
                    -- AND L
                    AND (RegArg8 CPU.L)

                -- 0xA6
                B.HexByte B.HA B.H6 ->
                    -- AND (HL)
                    AND (Address16 CPU.HL)

                -- 0xA7
                B.HexByte B.HA B.H7 ->
                    -- AND A
                    AND (RegArg8 CPU.A)

                ---- XOR ----
                -- 0xA8
                B.HexByte B.HA B.H8 ->
                    -- XOR B
                    XOR (RegArg8 CPU.B)

                -- 0xA9
                B.HexByte B.HA B.H9 ->
                    -- XOR C
                    XOR (RegArg8 CPU.C)

                -- 0xAA
                B.HexByte B.HA B.HA ->
                    -- XOR D
                    XOR (RegArg8 CPU.D)

                -- 0xAB
                B.HexByte B.HA B.HB ->
                    -- XOR E
                    XOR (RegArg8 CPU.E)

                -- 0xAC
                B.HexByte B.HA B.HC ->
                    -- XOR H
                    XOR (RegArg8 CPU.H)

                -- 0xAD
                B.HexByte B.HA B.HD ->
                    -- XOR L
                    XOR (RegArg8 CPU.L)

                -- 0xAE
                B.HexByte B.HA B.HE ->
                    -- XOR (HL)
                    XOR (Address16 CPU.HL)

                -- 0xAF
                B.HexByte B.HA B.HF ->
                    -- XOR A
                    XOR (RegArg8 CPU.C)

                ---- SUB ----
                -- 0xB0
                B.HexByte B.HB B.H0 ->
                    -- OR B
                    OR (RegArg8 CPU.B)

                -- 0xB1
                B.HexByte B.HB B.H1 ->
                    -- OR C
                    OR (RegArg8 CPU.C)

                -- 0xB2
                B.HexByte B.HB B.H2 ->
                    -- OR D
                    OR (RegArg8 CPU.D)

                -- 0xB3
                B.HexByte B.HB B.H3 ->
                    -- OR E
                    OR (RegArg8 CPU.E)

                -- 0xB4
                B.HexByte B.HB B.H4 ->
                    -- OR H
                    OR (RegArg8 CPU.H)

                -- 0xB5
                B.HexByte B.HB B.H5 ->
                    -- OR L
                    OR (RegArg8 CPU.L)

                -- 0xB6
                B.HexByte B.HB B.H6 ->
                    -- OR (HL)
                    OR (Address16 CPU.HL)

                -- 0xB7
                B.HexByte B.HB B.H7 ->
                    -- OR A
                    OR (RegArg8 CPU.A)

                ---- CP ----
                -- 0xB8
                B.HexByte B.HB B.H8 ->
                    -- CP B
                    CP (RegArg8 CPU.B)

                -- 0xB9
                B.HexByte B.HB B.H9 ->
                    -- CP C
                    CP (RegArg8 CPU.C)

                -- 0xBA
                B.HexByte B.HB B.HA ->
                    -- CP D
                    CP (RegArg8 CPU.D)

                -- 0xBB
                B.HexByte B.HB B.HB ->
                    -- CP E
                    CP (RegArg8 CPU.E)

                -- 0xBC
                B.HexByte B.HB B.HC ->
                    -- CP H
                    CP (RegArg8 CPU.H)

                -- 0xBD
                B.HexByte B.HB B.HD ->
                    -- CP L
                    CP (RegArg8 CPU.L)

                -- 0xBE
                B.HexByte B.HB B.HE ->
                    -- CP (HL)
                    CP (Address16 CPU.HL)

                -- 0xBF
                B.HexByte B.HB B.HF ->
                    -- CP A
                    CP (RegArg8 CPU.C)

                ---- RET ----
                -- 0xC0
                B.HexByte B.HC B.H0 ->
                    -- RET NZ
                    RETFlag CPU.NonZeroFlag

                -- 0xD1
                B.HexByte B.HD B.H1 ->
                    -- RET NC
                    RETFlag CPU.NonCarryFlag

                -- 0xC8
                B.HexByte B.HC B.H8 ->
                    -- RET Z
                    RETFlag CPU.ZeroFlag

                -- 0xD8
                B.HexByte B.HD B.H8 ->
                    -- RET C
                    RETFlag CPU.CarryFlag

                -- 0xC9
                B.HexByte B.HC B.H9 ->
                    -- RET
                    RET

                -- 0xD9
                B.HexByte B.HD B.H9 ->
                    -- RETI
                    RETI

                ---- POP ----
                -- 0xC1
                B.HexByte B.HC B.H1 ->
                    -- POP BC
                    POP (RegArg16 CPU.BC)

                -- 0xD1
                B.HexByte B.HD B.H1 ->
                    -- POP DE
                    POP (RegArg16 CPU.DE)

                -- 0xE1
                B.HexByte B.HE B.H1 ->
                    -- POP HL
                    POP (RegArg16 CPU.HL)

                -- 0xF1
                B.HexByte B.HF B.H1 ->
                    -- POP AF
                    POP (RegArg16 CPU.AF)

                ---- PUSH ----
                -- 0xC5
                B.HexByte B.HC B.H5 ->
                    -- PUSH BC
                    PUSH (RegArg16 CPU.BC)

                -- 0xD5
                B.HexByte B.HD B.H5 ->
                    -- PUSH DE
                    PUSH (RegArg16 CPU.DE)

                -- 0xE5
                B.HexByte B.HE B.H5 ->
                    -- PUSH HL
                    PUSH (RegArg16 CPU.HL)

                -- 0xF5
                B.HexByte B.HF B.H5 ->
                    -- PUSH AF
                    PUSH (RegArg16 CPU.AF)

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
                            -- RL B
                            RL (RegArg8 CPU.B)

                        B.HexByte B.H1 B.H1 ->
                            -- RL C
                            RL (RegArg8 CPU.C)

                        -- 0x12
                        B.HexByte B.H1 B.H2 ->
                            -- RL D
                            RL (RegArg8 CPU.D)

                        -- 0x13
                        B.HexByte B.H1 B.H3 ->
                            -- RL D
                            RL (RegArg8 CPU.E)

                        -- 0x14
                        B.HexByte B.H1 B.H4 ->
                            -- RL H
                            RL (RegArg8 CPU.H)

                        -- 0x15
                        B.HexByte B.H1 B.H5 ->
                            -- RL L
                            RL (RegArg8 CPU.L)

                        -- 0x16
                        B.HexByte B.H1 B.H6 ->
                            -- RL (HL)
                            RL (Address16 CPU.HL)

                        -- 0x17
                        B.HexByte B.H1 B.H7 ->
                            -- RL A
                            RL (RegArg8 CPU.A)

                        ---- RR ----
                        -- 0x18
                        B.HexByte B.H1 B.H8 ->
                            -- RL A
                            RR (RegArg8 CPU.B)

                        -- 0x19
                        B.HexByte B.H1 B.H9 ->
                            -- RL C
                            RR (RegArg8 CPU.C)

                        -- 0x1A
                        B.HexByte B.H1 B.HA ->
                            -- RL D
                            RR (RegArg8 CPU.D)

                        -- 0x1B
                        B.HexByte B.H1 B.HB ->
                            -- RL E
                            RR (RegArg8 CPU.E)

                        -- 0x1C
                        B.HexByte B.H1 B.HC ->
                            -- RL H
                            RR (RegArg8 CPU.H)

                        -- 0x1D
                        B.HexByte B.H1 B.HD ->
                            -- RL L
                            RR (RegArg8 CPU.L)

                        -- 0x1E
                        B.HexByte B.H1 B.HE ->
                            -- RL (HL)
                            RR (Address16 CPU.HL)

                        -- 0x1F
                        B.HexByte B.H1 B.HF ->
                            -- RL L
                            RR (RegArg8 CPU.A)

                        -- 0x20
                        B.HexByte B.H2 B.H0 ->
                            -- SLA B
                            SLA (RegArg8 CPU.B)

                        -- 0x21
                        B.HexByte B.H2 B.H1 ->
                            -- SLA C
                            SLA (RegArg8 CPU.C)

                        -- 0x22
                        B.HexByte B.H2 B.H2 ->
                            -- SLA D
                            SLA (RegArg8 CPU.D)

                        -- 0x23
                        B.HexByte B.H2 B.H3 ->
                            -- SLA E
                            SLA (RegArg8 CPU.E)

                        -- 0x24
                        B.HexByte B.H2 B.H4 ->
                            -- SLA H
                            SLA (RegArg8 CPU.H)

                        -- 0x25
                        B.HexByte B.H2 B.H5 ->
                            -- SLA L
                            SLA (RegArg8 CPU.L)

                        -- 0x26
                        B.HexByte B.H2 B.H6 ->
                            -- SLA (HL)
                            SLA (Address16 CPU.HL)

                        -- 0x27
                        B.HexByte B.H2 B.H7 ->
                            -- SLA A
                            SLA (RegArg8 CPU.A)

                        -- 0x28
                        B.HexByte B.H2 B.H8 ->
                            -- SRA B
                            SRA (RegArg8 CPU.B)

                        -- 0x29
                        B.HexByte B.H2 B.H9 ->
                            -- SRA C
                            SRA (RegArg8 CPU.C)

                        -- 0x2A
                        B.HexByte B.H2 B.HA ->
                            -- SRA D
                            SRA (RegArg8 CPU.D)

                        -- 0x2B
                        B.HexByte B.H2 B.HB ->
                            -- SRA E
                            SRA (RegArg8 CPU.E)

                        -- 0x2C
                        B.HexByte B.H2 B.HC ->
                            -- SRA H
                            SRA (RegArg8 CPU.H)

                        -- 0x2D
                        B.HexByte B.H2 B.HD ->
                            -- SRA L
                            SRA (RegArg8 CPU.L)

                        -- 0x2E
                        B.HexByte B.H2 B.HE ->
                            -- SRA (HL)
                            SRA (Address16 CPU.HL)

                        -- 0x2F
                        B.HexByte B.H2 B.HF ->
                            -- SRA A
                            SRA (RegArg8 CPU.A)

                        -- 0x30
                        B.HexByte B.H3 B.H0 ->
                            -- SWAP B
                            SWAP (RegArg8 CPU.B)

                        -- 0x31
                        B.HexByte B.H3 B.H1 ->
                            -- SWAP C
                            SWAP (RegArg8 CPU.C)

                        -- 0x32
                        B.HexByte B.H3 B.H2 ->
                            -- SWAP D
                            SWAP (RegArg8 CPU.D)

                        -- 0x33
                        B.HexByte B.H3 B.H3 ->
                            -- SWAP E
                            SWAP (RegArg8 CPU.E)

                        -- 0x34
                        B.HexByte B.H3 B.H4 ->
                            -- SWAP H
                            SWAP (RegArg8 CPU.H)

                        -- 0x35
                        B.HexByte B.H3 B.H5 ->
                            -- SWAP L
                            SWAP (RegArg8 CPU.L)

                        -- 0x36
                        B.HexByte B.H3 B.H6 ->
                            -- SWAP (HL)
                            SWAP (Address16 CPU.HL)

                        -- 0x37
                        B.HexByte B.H3 B.H7 ->
                            -- SWAP A
                            SWAP (RegArg8 CPU.A)

                        ---- SRL ----
                        -- 0x38
                        B.HexByte B.H3 B.H8 ->
                            -- SRL B
                            SRL (RegArg8 CPU.B)

                        -- 0x39
                        B.HexByte B.H3 B.H9 ->
                            -- SRL C
                            SRL (RegArg8 CPU.C)

                        -- 0x3A
                        B.HexByte B.H3 B.HA ->
                            -- SRL D
                            SRL (RegArg8 CPU.D)

                        -- 0x3B
                        B.HexByte B.H3 B.HB ->
                            -- SRL E
                            SRL (RegArg8 CPU.E)

                        -- 0x3C
                        B.HexByte B.H3 B.HC ->
                            -- SRL H
                            SRL (RegArg8 CPU.H)

                        -- 0x3D
                        B.HexByte B.H3 B.HD ->
                            -- SRL L
                            SRL (RegArg8 CPU.L)

                        -- 0x3E
                        B.HexByte B.H3 B.HE ->
                            -- SRL (HL)
                            SRL (Address16 CPU.HL)

                        -- 0x3F
                        B.HexByte B.H3 B.HF ->
                            -- SRL A
                            SRL (RegArg8 CPU.A)

                        ---- BIT ----
                        -- 0x40
                        B.HexByte B.H4 B.H0 ->
                            -- BIT 0 B
                            BIT B.ZeroIndex (RegArg8 CPU.B)

                        -- 0x41
                        B.HexByte B.H4 B.H1 ->
                            -- BIT 0 C
                            BIT B.ZeroIndex
                                (RegArg8 CPU.C)

                        -- 0x42
                        B.HexByte B.H4 B.H2 ->
                            -- BIT 0 D
                            BIT B.ZeroIndex
                                (RegArg8 CPU.D)

                        -- 0x43
                        B.HexByte B.H4 B.H3 ->
                            -- BIT 0 E
                            BIT B.ZeroIndex
                                (RegArg8 CPU.E)

                        -- 0x44
                        B.HexByte B.H4 B.H4 ->
                            -- BIT 0 H
                            BIT B.ZeroIndex
                                (RegArg8 CPU.H)

                        -- 0x45
                        B.HexByte B.H4 B.H5 ->
                            -- BIT 0 L
                            BIT B.ZeroIndex
                                (RegArg8 CPU.L)

                        -- 0x46
                        B.HexByte B.H4 B.H6 ->
                            -- BIT 0 (HL)
                            BIT B.ZeroIndex (Address16 CPU.HL)

                        -- 0x47
                        B.HexByte B.H4 B.H7 ->
                            -- BIT 0 A
                            BIT B.ZeroIndex
                                (RegArg8 CPU.A)

                        -- 0x48
                        B.HexByte B.H4 B.H8 ->
                            -- BIT 1 B
                            BIT B.OneIndex
                                (RegArg8 CPU.B)

                        -- 0x49
                        B.HexByte B.H4 B.H9 ->
                            -- BIT 1 C
                            BIT B.OneIndex
                                (RegArg8 CPU.C)

                        -- 0x4A
                        B.HexByte B.H4 B.HA ->
                            -- BIT 1 D
                            BIT B.OneIndex
                                (RegArg8 CPU.D)

                        -- 0x4B
                        B.HexByte B.H4 B.HB ->
                            -- BIT 1 E
                            BIT B.OneIndex
                                (RegArg8 CPU.E)

                        -- 0x4C
                        B.HexByte B.H4 B.HC ->
                            -- BIT 1 H
                            BIT B.OneIndex
                                (RegArg8 CPU.H)

                        -- 0x4D
                        B.HexByte B.H4 B.HD ->
                            -- BIT 1 L
                            BIT B.OneIndex
                                (RegArg8 CPU.L)

                        -- 0x4E
                        B.HexByte B.H4 B.HE ->
                            -- BIT 1 (HL)
                            BIT B.OneIndex
                                (Address16 CPU.HL)

                        -- 0x4F
                        B.HexByte B.H4 B.HF ->
                            -- BIT 1 A
                            BIT B.OneIndex
                                (RegArg8 CPU.A)

                        -- 0x50
                        B.HexByte B.H5 B.H0 ->
                            -- BIT 2 B
                            BIT B.TwoIndex (RegArg8 CPU.B)

                        -- 0x51
                        B.HexByte B.H5 B.H1 ->
                            -- BIT 2 C
                            BIT B.TwoIndex
                                (RegArg8 CPU.C)

                        -- 0x52
                        B.HexByte B.H5 B.H2 ->
                            -- BIT 2 D
                            BIT B.TwoIndex
                                (RegArg8 CPU.D)

                        -- 0x53
                        B.HexByte B.H5 B.H3 ->
                            -- BIT 2 E
                            BIT B.TwoIndex
                                (RegArg8 CPU.E)

                        -- 0x54
                        B.HexByte B.H5 B.H4 ->
                            -- BIT 2 H
                            BIT B.TwoIndex
                                (RegArg8 CPU.H)

                        -- 0x55
                        B.HexByte B.H5 B.H5 ->
                            -- BIT 2 L
                            BIT B.TwoIndex
                                (RegArg8 CPU.L)

                        -- 0x56
                        B.HexByte B.H5 B.H6 ->
                            -- BIT 2 (HL)
                            BIT B.TwoIndex (Address16 CPU.HL)

                        -- 0x57
                        B.HexByte B.H5 B.H7 ->
                            -- BIT 2 A
                            BIT B.TwoIndex
                                (RegArg8 CPU.A)

                        -- 0x58
                        B.HexByte B.H5 B.H8 ->
                            -- BIT 3 B
                            BIT B.ThreeIndex
                                (RegArg8 CPU.B)

                        -- 0x59
                        B.HexByte B.H5 B.H9 ->
                            -- BIT 3 C
                            BIT B.ThreeIndex
                                (RegArg8 CPU.C)

                        -- 0x5A
                        B.HexByte B.H5 B.HA ->
                            -- BIT 3 D
                            BIT B.ThreeIndex
                                (RegArg8 CPU.D)

                        -- 0x5B
                        B.HexByte B.H5 B.HB ->
                            -- BIT 3 E
                            BIT B.ThreeIndex
                                (RegArg8 CPU.E)

                        -- 0x5C
                        B.HexByte B.H5 B.HC ->
                            -- BIT 3 H
                            BIT B.ThreeIndex
                                (RegArg8 CPU.H)

                        -- 0x5D
                        B.HexByte B.H5 B.HD ->
                            -- BIT 3 L
                            BIT B.ThreeIndex
                                (RegArg8 CPU.L)

                        -- 0x5E
                        B.HexByte B.H5 B.HE ->
                            -- BIT 3 (HL)
                            BIT B.ThreeIndex
                                (Address16 CPU.HL)

                        -- 0x5F
                        B.HexByte B.H5 B.HF ->
                            -- BIT 3 A
                            BIT B.ThreeIndex
                                (RegArg8 CPU.A)

                        -- 0x60
                        B.HexByte B.H6 B.H0 ->
                            -- BIT 4 B
                            BIT B.FourIndex (RegArg8 CPU.B)

                        -- 0x61
                        B.HexByte B.H6 B.H1 ->
                            -- BIT 4 C
                            BIT B.FourIndex
                                (RegArg8 CPU.C)

                        -- 0x62
                        B.HexByte B.H6 B.H2 ->
                            -- BIT 4 D
                            BIT B.FourIndex
                                (RegArg8 CPU.D)

                        -- 0x63
                        B.HexByte B.H6 B.H3 ->
                            -- BIT 4 E
                            BIT B.FourIndex
                                (RegArg8 CPU.E)

                        -- 0x64
                        B.HexByte B.H6 B.H4 ->
                            -- BIT 4 H
                            BIT B.FourIndex
                                (RegArg8 CPU.H)

                        -- 0x65
                        B.HexByte B.H6 B.H5 ->
                            -- BIT 4 L
                            BIT B.FourIndex
                                (RegArg8 CPU.L)

                        -- 0x66
                        B.HexByte B.H6 B.H6 ->
                            -- BIT 4 (HL)
                            BIT B.FourIndex (Address16 CPU.HL)

                        -- 0x67
                        B.HexByte B.H6 B.H7 ->
                            -- BIT 4 A
                            BIT B.FourIndex
                                (RegArg8 CPU.A)

                        -- 0x68
                        B.HexByte B.H6 B.H8 ->
                            -- BIT 5 B
                            BIT B.FiveIndex
                                (RegArg8 CPU.B)

                        -- 0x69
                        B.HexByte B.H6 B.H9 ->
                            -- BIT 5 C
                            BIT B.FiveIndex
                                (RegArg8 CPU.C)

                        -- 0x6A
                        B.HexByte B.H6 B.HA ->
                            -- BIT 5 D
                            BIT B.FiveIndex
                                (RegArg8 CPU.D)

                        -- 0x6B
                        B.HexByte B.H6 B.HB ->
                            -- BIT 5 E
                            BIT B.FiveIndex
                                (RegArg8 CPU.E)

                        -- 0x6C
                        B.HexByte B.H6 B.HC ->
                            -- BIT 5 H
                            BIT B.FiveIndex
                                (RegArg8 CPU.H)

                        -- 0x6D
                        B.HexByte B.H6 B.HD ->
                            -- BIT 5 L
                            BIT B.FiveIndex
                                (RegArg8 CPU.L)

                        -- 0x6E
                        B.HexByte B.H6 B.HE ->
                            -- BIT 5 (HL)
                            BIT B.FiveIndex
                                (Address16 CPU.HL)

                        -- 0x6F
                        B.HexByte B.H6 B.HF ->
                            -- BIT 5 A
                            BIT B.FiveIndex
                                (RegArg8 CPU.A)

                        -- 0x70
                        B.HexByte B.H7 B.H0 ->
                            -- BIT 6 B
                            BIT B.SixIndex (RegArg8 CPU.B)

                        -- 0x71
                        B.HexByte B.H7 B.H1 ->
                            -- BIT 6 C
                            BIT B.SixIndex
                                (RegArg8 CPU.C)

                        -- 0x72
                        B.HexByte B.H7 B.H2 ->
                            -- BIT 6 D
                            BIT B.SixIndex
                                (RegArg8 CPU.D)

                        -- 0x73
                        B.HexByte B.H7 B.H3 ->
                            -- BIT 6 E
                            BIT B.SixIndex
                                (RegArg8 CPU.E)

                        -- 0x74
                        B.HexByte B.H7 B.H4 ->
                            -- BIT 6 H
                            BIT B.SixIndex
                                (RegArg8 CPU.H)

                        -- 0x75
                        B.HexByte B.H7 B.H5 ->
                            -- BIT 6 L
                            BIT B.SixIndex
                                (RegArg8 CPU.L)

                        -- 0x76
                        B.HexByte B.H7 B.H6 ->
                            -- BIT 6 (HL)
                            BIT B.SixIndex (Address16 CPU.HL)

                        -- 0x77
                        B.HexByte B.H7 B.H7 ->
                            -- BIT 6 A
                            BIT B.SixIndex
                                (RegArg8 CPU.A)

                        -- 0x78
                        B.HexByte B.H7 B.H8 ->
                            -- BIT 7 B
                            BIT B.SevenIndex
                                (RegArg8 CPU.B)

                        -- 0x79
                        B.HexByte B.H7 B.H9 ->
                            -- BIT 7 C
                            BIT B.SevenIndex
                                (RegArg8 CPU.C)

                        -- 0x7A
                        B.HexByte B.H7 B.HA ->
                            -- BIT 7 D
                            BIT B.SevenIndex
                                (RegArg8 CPU.D)

                        -- 0x7B
                        B.HexByte B.H7 B.HB ->
                            -- BIT 7 E
                            BIT B.SevenIndex
                                (RegArg8 CPU.E)

                        -- 0x7C
                        B.HexByte B.H7 B.HC ->
                            -- BIT 7 H
                            BIT B.SevenIndex
                                (RegArg8 CPU.H)

                        -- 0x7D
                        B.HexByte B.H7 B.HD ->
                            -- BIT 7 L
                            BIT B.SevenIndex
                                (RegArg8 CPU.L)

                        -- 0x7E
                        B.HexByte B.H7 B.HE ->
                            -- BIT 7 (HL)
                            BIT B.SevenIndex
                                (Address16 CPU.HL)

                        -- 0x7F
                        B.HexByte B.H7 B.HF ->
                            -- BIT 7 A
                            BIT B.SevenIndex
                                (RegArg8 CPU.A)

                        ---- RES ----
                        -- 0x80
                        B.HexByte B.H8 B.H0 ->
                            -- RES 0 B
                            RES B.ZeroIndex (RegArg8 CPU.B)

                        -- 0x81
                        B.HexByte B.H8 B.H1 ->
                            -- RES 0 C
                            RES B.ZeroIndex
                                (RegArg8 CPU.C)

                        -- 0x82
                        B.HexByte B.H8 B.H2 ->
                            -- RES 0 D
                            RES B.ZeroIndex
                                (RegArg8 CPU.D)

                        -- 0x83
                        B.HexByte B.H8 B.H3 ->
                            -- RES 0 E
                            RES B.ZeroIndex
                                (RegArg8 CPU.E)

                        -- 0x84
                        B.HexByte B.H8 B.H4 ->
                            -- RES 0 H
                            RES B.ZeroIndex
                                (RegArg8 CPU.H)

                        -- 0x85
                        B.HexByte B.H8 B.H5 ->
                            -- RES 0 L
                            RES B.ZeroIndex
                                (RegArg8 CPU.L)

                        -- 0x86
                        B.HexByte B.H8 B.H6 ->
                            -- RES 0 (HL)
                            RES B.ZeroIndex (Address16 CPU.HL)

                        -- 0x87
                        B.HexByte B.H8 B.H7 ->
                            -- RES 0 A
                            RES B.ZeroIndex
                                (RegArg8 CPU.A)

                        -- 0x88
                        B.HexByte B.H8 B.H8 ->
                            -- RES 1 B
                            RES B.OneIndex
                                (RegArg8 CPU.B)

                        -- 0x89
                        B.HexByte B.H8 B.H9 ->
                            -- RES 1 C
                            RES B.OneIndex
                                (RegArg8 CPU.C)

                        -- 0x8A
                        B.HexByte B.H8 B.HA ->
                            -- RES 1 D
                            RES B.OneIndex
                                (RegArg8 CPU.D)

                        -- 0x8B
                        B.HexByte B.H8 B.HB ->
                            -- RES 1 E
                            RES B.OneIndex
                                (RegArg8 CPU.E)

                        -- 0x8C
                        B.HexByte B.H8 B.HC ->
                            -- RES 1 H
                            RES B.OneIndex
                                (RegArg8 CPU.H)

                        -- 0x8D
                        B.HexByte B.H8 B.HD ->
                            -- RES 1 L
                            RES B.OneIndex
                                (RegArg8 CPU.L)

                        -- 0x8E
                        B.HexByte B.H8 B.HE ->
                            -- RES 1 (HL)
                            RES B.OneIndex
                                (Address16 CPU.HL)

                        -- 0x8F
                        B.HexByte B.H8 B.HF ->
                            -- RES 1 A
                            RES B.OneIndex
                                (RegArg8 CPU.A)

                        -- 0x90
                        B.HexByte B.H9 B.H0 ->
                            -- RES 2 B
                            RES B.TwoIndex (RegArg8 CPU.B)

                        -- 0x91
                        B.HexByte B.H9 B.H1 ->
                            -- RES 2 C
                            RES B.TwoIndex
                                (RegArg8 CPU.C)

                        -- 0x92
                        B.HexByte B.H9 B.H2 ->
                            -- RES 2 D
                            RES B.TwoIndex
                                (RegArg8 CPU.D)

                        -- 0x93
                        B.HexByte B.H9 B.H3 ->
                            -- RES 2 E
                            RES B.TwoIndex
                                (RegArg8 CPU.E)

                        -- 0x94
                        B.HexByte B.H9 B.H4 ->
                            -- RES 2 H
                            RES B.TwoIndex
                                (RegArg8 CPU.H)

                        -- 0x95
                        B.HexByte B.H9 B.H5 ->
                            -- RES 2 L
                            RES B.TwoIndex
                                (RegArg8 CPU.L)

                        -- 0x96
                        B.HexByte B.H9 B.H6 ->
                            -- RES 2 (HL)
                            RES B.TwoIndex (Address16 CPU.HL)

                        -- 0x97
                        B.HexByte B.H9 B.H7 ->
                            -- RES 2 A
                            RES B.TwoIndex
                                (RegArg8 CPU.A)

                        -- 0x98
                        B.HexByte B.H9 B.H8 ->
                            -- RES 3 B
                            RES B.ThreeIndex
                                (RegArg8 CPU.B)

                        -- 0x99
                        B.HexByte B.H9 B.H9 ->
                            -- RES 3 C
                            RES B.ThreeIndex
                                (RegArg8 CPU.C)

                        -- 0x9A
                        B.HexByte B.H9 B.HA ->
                            -- RES 3 D
                            RES B.ThreeIndex
                                (RegArg8 CPU.D)

                        -- 0x9B
                        B.HexByte B.H9 B.HB ->
                            -- RES 3 E
                            RES B.ThreeIndex
                                (RegArg8 CPU.E)

                        -- 0x9C
                        B.HexByte B.H9 B.HC ->
                            -- RES 3 H
                            RES B.ThreeIndex
                                (RegArg8 CPU.H)

                        -- 0x9D
                        B.HexByte B.H9 B.HD ->
                            -- RES 3 L
                            RES B.ThreeIndex
                                (RegArg8 CPU.L)

                        -- 0x9E
                        B.HexByte B.H9 B.HE ->
                            -- RES 3 (HL)
                            RES B.ThreeIndex
                                (Address16 CPU.HL)

                        -- 0x9F
                        B.HexByte B.H9 B.HF ->
                            -- RES 3 A
                            RES B.ThreeIndex
                                (RegArg8 CPU.A)

                        -- 0xA0
                        B.HexByte B.HA B.H0 ->
                            -- RES 4 B
                            RES B.FourIndex (RegArg8 CPU.B)

                        -- 0xA1
                        B.HexByte B.HA B.H1 ->
                            -- RES 4 C
                            RES B.FourIndex
                                (RegArg8 CPU.C)

                        -- 0xA2
                        B.HexByte B.HA B.H2 ->
                            -- RES 4 D
                            RES B.FourIndex
                                (RegArg8 CPU.D)

                        -- 0xA3
                        B.HexByte B.HA B.H3 ->
                            -- RES 4 E
                            RES B.FourIndex
                                (RegArg8 CPU.E)

                        -- 0xA4
                        B.HexByte B.HA B.H4 ->
                            -- RES 4 H
                            RES B.FourIndex
                                (RegArg8 CPU.H)

                        -- 0xA5
                        B.HexByte B.HA B.H5 ->
                            -- RES 4 L
                            RES B.FourIndex
                                (RegArg8 CPU.L)

                        -- 0xA6
                        B.HexByte B.HA B.H6 ->
                            -- RES 4 (HL)
                            RES B.FourIndex (Address16 CPU.HL)

                        -- 0xA7
                        B.HexByte B.HA B.H7 ->
                            -- RES 4 A
                            RES B.FourIndex
                                (RegArg8 CPU.A)

                        -- 0xA8
                        B.HexByte B.HA B.H8 ->
                            -- RES 5 B
                            RES B.FiveIndex
                                (RegArg8 CPU.B)

                        -- 0xA9
                        B.HexByte B.HA B.H9 ->
                            -- RES 5 C
                            RES B.FiveIndex
                                (RegArg8 CPU.C)

                        -- 0xAA
                        B.HexByte B.HA B.HA ->
                            -- RES 5 D
                            RES B.FiveIndex
                                (RegArg8 CPU.D)

                        -- 0xAB
                        B.HexByte B.HA B.HB ->
                            -- RES 5 E
                            RES B.FiveIndex
                                (RegArg8 CPU.E)

                        -- 0xAC
                        B.HexByte B.HA B.HC ->
                            -- RES 5 H
                            RES B.FiveIndex
                                (RegArg8 CPU.H)

                        -- 0xAD
                        B.HexByte B.HA B.HD ->
                            -- RES 5 L
                            RES B.FiveIndex
                                (RegArg8 CPU.L)

                        -- 0xAE
                        B.HexByte B.HA B.HE ->
                            -- RES 5 (HL)
                            RES B.FiveIndex
                                (Address16 CPU.HL)

                        -- 0xAF
                        B.HexByte B.HA B.HF ->
                            -- RES 5 A
                            RES B.FiveIndex
                                (RegArg8 CPU.A)

                        -- 0xB0
                        B.HexByte B.HB B.H0 ->
                            -- RES 6 B
                            RES B.SixIndex (RegArg8 CPU.B)

                        -- 0xB1
                        B.HexByte B.HB B.H1 ->
                            -- RES 6 C
                            RES B.SixIndex
                                (RegArg8 CPU.C)

                        -- 0xB2
                        B.HexByte B.HB B.H2 ->
                            -- RES 6 D
                            RES B.SixIndex
                                (RegArg8 CPU.D)

                        -- 0xB3
                        B.HexByte B.HB B.H3 ->
                            -- RES 6 E
                            RES B.SixIndex
                                (RegArg8 CPU.E)

                        -- 0xB4
                        B.HexByte B.HB B.H4 ->
                            -- RES 6 H
                            RES B.SixIndex
                                (RegArg8 CPU.H)

                        -- 0xB5
                        B.HexByte B.HB B.H5 ->
                            -- RES 6 L
                            RES B.SixIndex
                                (RegArg8 CPU.L)

                        -- 0xB6
                        B.HexByte B.HB B.H6 ->
                            -- RES 6 (HL)
                            RES B.SixIndex (Address16 CPU.HL)

                        -- 0xB7
                        B.HexByte B.HB B.H7 ->
                            -- RES 6 A
                            RES B.SixIndex
                                (RegArg8 CPU.A)

                        -- 0xB8
                        B.HexByte B.HB B.H8 ->
                            -- RES 7 B
                            RES B.SevenIndex
                                (RegArg8 CPU.B)

                        -- 0xB9
                        B.HexByte B.HB B.H9 ->
                            -- RES 7 C
                            RES B.SevenIndex
                                (RegArg8 CPU.C)

                        -- 0xBA
                        B.HexByte B.HB B.HA ->
                            -- RES 7 D
                            RES B.SevenIndex
                                (RegArg8 CPU.D)

                        -- 0xBB
                        B.HexByte B.HB B.HB ->
                            -- RES 7 E
                            RES B.SevenIndex
                                (RegArg8 CPU.E)

                        -- 0xBC
                        B.HexByte B.HB B.HC ->
                            -- RES 7 H
                            RES B.SevenIndex
                                (RegArg8 CPU.H)

                        -- 0xBD
                        B.HexByte B.HB B.HD ->
                            -- RES 7 L
                            RES B.SevenIndex
                                (RegArg8 CPU.L)

                        -- 0xBE
                        B.HexByte B.HB B.HE ->
                            -- RES 7 (HL)
                            RES B.SevenIndex
                                (Address16 CPU.HL)

                        -- 0xBF
                        B.HexByte B.HB B.HF ->
                            -- RES 7 A
                            RES B.SevenIndex
                                (RegArg8 CPU.A)

                        ---- SET ----
                        -- 0xC0
                        B.HexByte B.HC B.H0 ->
                            -- SET 0 B
                            SET B.ZeroIndex (RegArg8 CPU.B)

                        -- 0xC1
                        B.HexByte B.HC B.H1 ->
                            -- SET 0 C
                            SET B.ZeroIndex
                                (RegArg8 CPU.C)

                        -- 0xC2
                        B.HexByte B.HC B.H2 ->
                            -- SET 0 D
                            SET B.ZeroIndex
                                (RegArg8 CPU.D)

                        -- 0xC3
                        B.HexByte B.HC B.H3 ->
                            -- SET 0 E
                            SET B.ZeroIndex
                                (RegArg8 CPU.E)

                        -- 0xC4
                        B.HexByte B.HC B.H4 ->
                            -- SET 0 H
                            SET B.ZeroIndex
                                (RegArg8 CPU.H)

                        -- 0xC5
                        B.HexByte B.HC B.H5 ->
                            -- SET 0 L
                            SET B.ZeroIndex
                                (RegArg8 CPU.L)

                        -- 0xC6
                        B.HexByte B.HC B.H6 ->
                            -- SET 0 (HL)
                            SET B.ZeroIndex (Address16 CPU.HL)

                        -- 0xC7
                        B.HexByte B.HC B.H7 ->
                            -- SET 0 A
                            SET B.ZeroIndex
                                (RegArg8 CPU.A)

                        -- 0xC8
                        B.HexByte B.HC B.H8 ->
                            -- SET 1 B
                            SET B.OneIndex
                                (RegArg8 CPU.B)

                        -- 0xC9
                        B.HexByte B.HC B.H9 ->
                            -- SET 1 C
                            SET B.OneIndex
                                (RegArg8 CPU.C)

                        -- 0xCA
                        B.HexByte B.HC B.HA ->
                            -- SET 1 D
                            SET B.OneIndex
                                (RegArg8 CPU.D)

                        -- 0xCB
                        B.HexByte B.HC B.HB ->
                            -- SET 1 E
                            SET B.OneIndex
                                (RegArg8 CPU.E)

                        -- 0xCC
                        B.HexByte B.HC B.HC ->
                            -- SET 1 H
                            SET B.OneIndex
                                (RegArg8 CPU.H)

                        -- 0xCD
                        B.HexByte B.HC B.HD ->
                            -- SET 1 L
                            SET B.OneIndex
                                (RegArg8 CPU.L)

                        -- 0xCE
                        B.HexByte B.HC B.HE ->
                            -- SET 1 (HL)
                            SET B.OneIndex
                                (Address16 CPU.HL)

                        -- 0xCF
                        B.HexByte B.HC B.HF ->
                            -- SET 1 A
                            SET B.OneIndex
                                (RegArg8 CPU.A)

                        -- 0xD0
                        B.HexByte B.HD B.H0 ->
                            -- SET 2 B
                            SET B.TwoIndex (RegArg8 CPU.B)

                        -- 0xD1
                        B.HexByte B.HD B.H1 ->
                            -- SET 2 C
                            SET B.TwoIndex
                                (RegArg8 CPU.C)

                        -- 0xD2
                        B.HexByte B.HD B.H2 ->
                            -- SET 2 D
                            SET B.TwoIndex
                                (RegArg8 CPU.D)

                        -- 0xD3
                        B.HexByte B.HD B.H3 ->
                            -- SET 2 E
                            SET B.TwoIndex
                                (RegArg8 CPU.E)

                        -- 0xD4
                        B.HexByte B.HD B.H4 ->
                            -- SET 2 H
                            SET B.TwoIndex
                                (RegArg8 CPU.H)

                        -- 0xD5
                        B.HexByte B.HD B.H5 ->
                            -- SET 2 L
                            SET B.TwoIndex
                                (RegArg8 CPU.L)

                        -- 0xD6
                        B.HexByte B.HD B.H6 ->
                            -- SET 2 (HL)
                            SET B.TwoIndex (Address16 CPU.HL)

                        -- 0xD7
                        B.HexByte B.HD B.H7 ->
                            -- SET 2 A
                            SET B.TwoIndex
                                (RegArg8 CPU.A)

                        -- 0xD8
                        B.HexByte B.HD B.H8 ->
                            -- SET 3 B
                            SET B.ThreeIndex
                                (RegArg8 CPU.B)

                        -- 0xD9
                        B.HexByte B.HD B.H9 ->
                            -- SET 3 C
                            SET B.ThreeIndex
                                (RegArg8 CPU.C)

                        -- 0xDA
                        B.HexByte B.HD B.HA ->
                            -- SET 3 D
                            SET B.ThreeIndex
                                (RegArg8 CPU.D)

                        -- 0xDB
                        B.HexByte B.HD B.HB ->
                            -- SET 3 E
                            SET B.ThreeIndex
                                (RegArg8 CPU.E)

                        -- 0xDC
                        B.HexByte B.HD B.HC ->
                            -- SET 3 H
                            SET B.ThreeIndex
                                (RegArg8 CPU.H)

                        -- 0xDD
                        B.HexByte B.HD B.HD ->
                            -- SET 3 L
                            SET B.ThreeIndex
                                (RegArg8 CPU.L)

                        -- 0xDE
                        B.HexByte B.HD B.HE ->
                            -- SET 3 (HL)
                            SET B.ThreeIndex
                                (Address16 CPU.HL)

                        -- 0xDF
                        B.HexByte B.HD B.HF ->
                            -- SET 3 A
                            SET B.ThreeIndex
                                (RegArg8 CPU.A)

                        -- 0xE0
                        B.HexByte B.HE B.H0 ->
                            -- SET 4 B
                            SET B.FourIndex (RegArg8 CPU.B)

                        -- 0xE1
                        B.HexByte B.HE B.H1 ->
                            -- SET 4 C
                            SET B.FourIndex
                                (RegArg8 CPU.C)

                        -- 0xE2
                        B.HexByte B.HE B.H2 ->
                            -- SET 4 D
                            SET B.FourIndex
                                (RegArg8 CPU.D)

                        -- 0xE3
                        B.HexByte B.HE B.H3 ->
                            -- SET 4 E
                            SET B.FourIndex
                                (RegArg8 CPU.E)

                        -- 0xE4
                        B.HexByte B.HE B.H4 ->
                            -- SET 4 H
                            SET B.FourIndex
                                (RegArg8 CPU.H)

                        -- 0xE5
                        B.HexByte B.HE B.H5 ->
                            -- SET 4 L
                            SET B.FourIndex
                                (RegArg8 CPU.L)

                        -- 0xE6
                        B.HexByte B.HE B.H6 ->
                            -- SET 4 (HL)
                            SET B.FourIndex (Address16 CPU.HL)

                        -- 0xE7
                        B.HexByte B.HE B.H7 ->
                            -- SET 4 A
                            SET B.FourIndex
                                (RegArg8 CPU.A)

                        -- 0xE8
                        B.HexByte B.HE B.H8 ->
                            -- SET 5 B
                            SET B.FiveIndex
                                (RegArg8 CPU.B)

                        -- 0xE9
                        B.HexByte B.HE B.H9 ->
                            -- SET 5 C
                            SET B.FiveIndex
                                (RegArg8 CPU.C)

                        -- 0xEA
                        B.HexByte B.HE B.HA ->
                            -- SET 5 D
                            SET B.FiveIndex
                                (RegArg8 CPU.D)

                        -- 0xEB
                        B.HexByte B.HE B.HB ->
                            -- SET 5 E
                            SET B.FiveIndex
                                (RegArg8 CPU.E)

                        -- 0xEC
                        B.HexByte B.HE B.HC ->
                            -- SET 5 H
                            SET B.FiveIndex
                                (RegArg8 CPU.H)

                        -- 0xED
                        B.HexByte B.HE B.HD ->
                            -- SET 5 L
                            SET B.FiveIndex
                                (RegArg8 CPU.L)

                        -- 0xEE
                        B.HexByte B.HE B.HE ->
                            -- SET 5 (HL)
                            SET B.FiveIndex
                                (Address16 CPU.HL)

                        -- 0xEF
                        B.HexByte B.HE B.HF ->
                            -- SET 5 A
                            SET B.FiveIndex
                                (RegArg8 CPU.A)

                        -- 0xF0
                        B.HexByte B.HF B.H0 ->
                            -- SET 6 B
                            SET B.SixIndex (RegArg8 CPU.B)

                        -- 0xF1
                        B.HexByte B.HF B.H1 ->
                            -- SET 6 C
                            SET B.SixIndex
                                (RegArg8 CPU.C)

                        -- 0xF2
                        B.HexByte B.HF B.H2 ->
                            -- SET 6 D
                            SET B.SixIndex
                                (RegArg8 CPU.D)

                        -- 0xF3
                        B.HexByte B.HF B.H3 ->
                            -- SET 6 E
                            SET B.SixIndex
                                (RegArg8 CPU.E)

                        -- 0xF4
                        B.HexByte B.HF B.H4 ->
                            -- SET 6 H
                            SET B.SixIndex
                                (RegArg8 CPU.H)

                        -- 0xF5
                        B.HexByte B.HF B.H5 ->
                            -- SET 6 L
                            SET B.SixIndex
                                (RegArg8 CPU.L)

                        -- 0xF6
                        B.HexByte B.HF B.H6 ->
                            -- RES 6 (HL)
                            RES B.SixIndex (Address16 CPU.HL)

                        -- 0xF7
                        B.HexByte B.HF B.H7 ->
                            -- RES 6 A
                            RES B.SixIndex
                                (RegArg8 CPU.A)

                        -- 0xF8
                        B.HexByte B.HF B.H8 ->
                            -- RES 7 B
                            RES B.SevenIndex
                                (RegArg8 CPU.B)

                        -- 0xF9
                        B.HexByte B.HF B.H9 ->
                            -- RES 7 C
                            RES B.SevenIndex
                                (RegArg8 CPU.C)

                        -- 0xFA
                        B.HexByte B.HF B.HA ->
                            -- RES 7 D
                            RES B.SevenIndex
                                (RegArg8 CPU.D)

                        -- 0xFB
                        B.HexByte B.HF B.HB ->
                            -- RES 7 E
                            RES B.SevenIndex
                                (RegArg8 CPU.E)

                        -- 0xFC
                        B.HexByte B.HF B.HC ->
                            -- RES 7 H
                            RES B.SevenIndex
                                (RegArg8 CPU.H)

                        -- 0xFD
                        B.HexByte B.HF B.HD ->
                            -- RES 7 L
                            RES B.SevenIndex
                                (RegArg8 CPU.L)

                        -- 0xFE
                        B.HexByte B.HF B.HE ->
                            -- RES 7 (HL)
                            RES B.SevenIndex
                                (Address16 CPU.HL)

                        -- 0xFF
                        B.HexByte B.HF B.HF ->
                            -- RES 7 A
                            RES B.SevenIndex
                                (RegArg8 CPU.A)

                _ ->
                    NOP

        TwoByteOpcode byte byte2 byte3 ->
            case (B.binaryByteToHexByte byte) of
                ---- LD ----
                -- 0x01
                B.HexByte B.H0 B.H1 ->
                    -- LD BC, d16
                    LDSixteenBitValue (RegArg16 CPU.BC) byte2 byte3

                -- 0x11
                B.HexByte B.H1 B.H1 ->
                    -- LD DE, d16
                    LDSixteenBitValue (RegArg16 CPU.DE) byte2 byte3

                -- 0x21
                B.HexByte B.H2 B.H1 ->
                    -- LD HL, d16
                    LDSixteenBitValue (RegArg16 CPU.HL) byte2 byte3

                -- 0x31
                B.HexByte B.H3 B.H1 ->
                    -- LD SP, d16
                    LDSixteenBitValue (RegArg16 CPU.SP) byte2 byte3

                -- 0x08
                B.HexByte B.H0 B.H8 ->
                    -- LD (a16), SP
                    LDHRegisterToImmediateAddress (ImmediateAddress16 byte2 byte3) (RegArg16 CPU.SP)

                _ ->
                    NOP
