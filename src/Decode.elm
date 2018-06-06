module Decode exposing (..)

import CPU exposing (Instruction(..), RegisterArgument(..), ImmediateAddress(..))
import Binary as B exposing (Bit(..))


-- An op code can include just the op code or
-- an eight bit or sixteen bit immediate value


type OpCode
    = OpCode B.Byte (Maybe B.Byte) (Maybe B.Byte)


type DecodeError
    = DecodeError { opCode : B.Byte, message : String }


decode : OpCode -> Result DecodeError Instruction
decode opCode =
    case opCode of
        OpCode byte byte2 byte3 ->
            case (B.binaryByteToHexByte byte) of
                -- 0x00
                B.HexByte B.H0 B.H0 ->
                    -- NOP
                    Ok NOP

                -- 0xF4
                B.HexByte B.HF B.H4 ->
                    -- NOP
                    Ok NOP

                -- 0xF3
                B.HexByte B.HF B.H3 ->
                    -- DI
                    Ok DI

                -- 0x02
                B.HexByte B.H0 B.H2 ->
                    -- LD (BC), A
                    Ok <| LDRegister (Address16 CPU.BC) (RegArg8 CPU.A)

                -- 0x12
                B.HexByte B.H1 B.H2 ->
                    -- LD (DE), A
                    Ok <| LDRegister (Address16 CPU.DE) (RegArg8 CPU.A)

                -- 0x22
                B.HexByte B.H2 B.H2 ->
                    -- LDI (HL), A
                    Ok <| LDI (Address16 CPU.HL) (RegArg8 CPU.A)

                -- 0x32
                B.HexByte B.H3 B.H2 ->
                    -- LDD (HL), A
                    Ok <| LDD (Address16 CPU.HL) (RegArg8 CPU.A)

                -- 0x40
                B.HexByte B.H4 B.H0 ->
                    -- LD B, B
                    Ok <| LDRegister (RegArg8 CPU.B) (RegArg8 CPU.B)

                -- 0x41
                B.HexByte B.H4 B.H1 ->
                    -- LD B, C
                    Ok <| LDRegister (RegArg8 CPU.B) (RegArg8 CPU.C)

                -- 0x42
                B.HexByte B.H4 B.H2 ->
                    -- LD B, D
                    Ok <| LDRegister (RegArg8 CPU.B) (RegArg8 CPU.D)

                -- 0x43
                B.HexByte B.H4 B.H3 ->
                    -- LD B, E
                    Ok <| LDRegister (RegArg8 CPU.B) (RegArg8 CPU.E)

                -- 0x44
                B.HexByte B.H4 B.H4 ->
                    -- LD B, H
                    Ok <| LDRegister (RegArg8 CPU.B) (RegArg8 CPU.H)

                -- 0x45
                B.HexByte B.H4 B.H5 ->
                    -- LD B, L
                    Ok <| LDRegister (RegArg8 CPU.B) (RegArg8 CPU.L)

                -- 0x46
                B.HexByte B.H4 B.H6 ->
                    -- LD B, (HL)
                    Ok <| LDRegister (RegArg8 CPU.B) (Address16 CPU.HL)

                -- 0x47
                B.HexByte B.H4 B.H7 ->
                    -- LD B, A
                    Ok <| LDRegister (RegArg8 CPU.B) (RegArg8 CPU.A)

                -- 0x48
                B.HexByte B.H4 B.H8 ->
                    -- LD C, B
                    Ok <| LDRegister (RegArg8 CPU.C) (RegArg8 CPU.B)

                -- 0x49
                B.HexByte B.H4 B.H9 ->
                    -- LD C, C
                    Ok <| LDRegister (RegArg8 CPU.C) (RegArg8 CPU.C)

                -- 0x4A
                B.HexByte B.H4 B.HA ->
                    -- LD C, D
                    Ok <| LDRegister (RegArg8 CPU.C) (RegArg8 CPU.D)

                -- 0x4B
                B.HexByte B.H4 B.HB ->
                    -- LD C, E
                    Ok <| LDRegister (RegArg8 CPU.C) (RegArg8 CPU.E)

                -- 0x4C
                B.HexByte B.H4 B.HC ->
                    -- LD C, H
                    Ok <| LDRegister (RegArg8 CPU.C) (RegArg8 CPU.H)

                -- 0x4D
                B.HexByte B.H4 B.HD ->
                    -- LD C, L
                    Ok <| LDRegister (RegArg8 CPU.C) (RegArg8 CPU.L)

                -- 0x4E
                B.HexByte B.H4 B.HE ->
                    -- LD C, (HL)
                    Ok <| LDRegister (RegArg8 CPU.C) (Address16 CPU.HL)

                -- 0x4F
                B.HexByte B.H4 B.HF ->
                    -- LD C, A
                    Ok <| LDRegister (RegArg8 CPU.C) (RegArg8 CPU.A)

                -- 0x50
                B.HexByte B.H5 B.H0 ->
                    -- LD D, B
                    Ok <| LDRegister (RegArg8 CPU.D) (RegArg8 CPU.B)

                -- 0x51
                B.HexByte B.H5 B.H1 ->
                    -- LD D, C
                    Ok <| LDRegister (RegArg8 CPU.D) (RegArg8 CPU.C)

                -- 0x52
                B.HexByte B.H5 B.H2 ->
                    -- LD D, D
                    Ok <| LDRegister (RegArg8 CPU.D) (RegArg8 CPU.D)

                -- 0x53
                B.HexByte B.H5 B.H3 ->
                    -- LD D, E
                    Ok <| LDRegister (RegArg8 CPU.D) (RegArg8 CPU.E)

                -- 0x54
                B.HexByte B.H5 B.H4 ->
                    -- LD D, H
                    Ok <| LDRegister (RegArg8 CPU.D) (RegArg8 CPU.H)

                -- 0x55
                B.HexByte B.H5 B.H5 ->
                    -- LD D, L
                    Ok <| LDRegister (RegArg8 CPU.D) (RegArg8 CPU.L)

                -- 0x56
                B.HexByte B.H5 B.H6 ->
                    -- LD D, (HL)
                    Ok <| LDRegister (RegArg8 CPU.D) (Address16 CPU.HL)

                -- 0x57
                B.HexByte B.H5 B.H7 ->
                    -- LD D, A
                    Ok <| LDRegister (RegArg8 CPU.D) (RegArg8 CPU.A)

                -- 0x58
                B.HexByte B.H5 B.H8 ->
                    -- LD E, B
                    Ok <| LDRegister (RegArg8 CPU.E) (RegArg8 CPU.B)

                -- 0x59
                B.HexByte B.H5 B.H9 ->
                    -- LD E, C
                    Ok <| LDRegister (RegArg8 CPU.E) (RegArg8 CPU.C)

                -- 0x5A
                B.HexByte B.H5 B.HA ->
                    -- LD E, D
                    Ok <| LDRegister (RegArg8 CPU.E) (RegArg8 CPU.D)

                -- 0x5B
                B.HexByte B.H5 B.HB ->
                    -- LD E, E
                    Ok <| LDRegister (RegArg8 CPU.E) (RegArg8 CPU.E)

                -- 0x5C
                B.HexByte B.H5 B.HC ->
                    -- LD E, H
                    Ok <| LDRegister (RegArg8 CPU.E) (RegArg8 CPU.H)

                -- 0x5D
                B.HexByte B.H5 B.HD ->
                    -- LD E, L
                    Ok <| LDRegister (RegArg8 CPU.E) (RegArg8 CPU.L)

                -- 0x5E
                B.HexByte B.H5 B.HE ->
                    -- LD E, (HL)
                    Ok <| LDRegister (RegArg8 CPU.E) (Address16 CPU.HL)

                -- 0x5F
                B.HexByte B.H5 B.HF ->
                    -- LD E, A
                    Ok <| LDRegister (RegArg8 CPU.E) (RegArg8 CPU.A)

                -- 0x60
                B.HexByte B.H6 B.H0 ->
                    -- LD H, B
                    Ok <| LDRegister (RegArg8 CPU.H) (RegArg8 CPU.B)

                -- 0x61
                B.HexByte B.H6 B.H1 ->
                    -- LD H, C
                    Ok <| LDRegister (RegArg8 CPU.H) (RegArg8 CPU.C)

                -- 0x62
                B.HexByte B.H6 B.H2 ->
                    -- LD H, D
                    Ok <| LDRegister (RegArg8 CPU.H) (RegArg8 CPU.D)

                -- 0x63
                B.HexByte B.H6 B.H3 ->
                    -- LD H, E
                    Ok <| LDRegister (RegArg8 CPU.H) (RegArg8 CPU.E)

                -- 0x64
                B.HexByte B.H6 B.H4 ->
                    -- LD H, H
                    Ok <| LDRegister (RegArg8 CPU.H) (RegArg8 CPU.H)

                -- 0x65
                B.HexByte B.H6 B.H5 ->
                    -- LD H, L
                    Ok <| LDRegister (RegArg8 CPU.H) (RegArg8 CPU.L)

                -- 0x66
                B.HexByte B.H6 B.H6 ->
                    -- LD H, (HL)
                    Ok <| LDRegister (RegArg8 CPU.H) (Address16 CPU.HL)

                -- 0x67
                B.HexByte B.H6 B.H7 ->
                    -- LD H, A
                    Ok <| LDRegister (RegArg8 CPU.H) (RegArg8 CPU.A)

                -- 0x68
                B.HexByte B.H6 B.H8 ->
                    -- LD L, B
                    Ok <| LDRegister (RegArg8 CPU.L) (RegArg8 CPU.B)

                -- 0x69
                B.HexByte B.H6 B.H9 ->
                    -- LD L, C
                    Ok <| LDRegister (RegArg8 CPU.L) (RegArg8 CPU.C)

                -- 0x6A
                B.HexByte B.H6 B.HA ->
                    -- LD L, D
                    Ok <| LDRegister (RegArg8 CPU.L) (RegArg8 CPU.D)

                -- 0x6B
                B.HexByte B.H6 B.HB ->
                    -- LD L, E
                    Ok <| LDRegister (RegArg8 CPU.L) (RegArg8 CPU.E)

                -- 0x6C
                B.HexByte B.H6 B.HC ->
                    -- LD L, H
                    Ok <| LDRegister (RegArg8 CPU.L) (RegArg8 CPU.H)

                -- 0x6D
                B.HexByte B.H6 B.HD ->
                    -- LD L, L
                    Ok <| LDRegister (RegArg8 CPU.L) (RegArg8 CPU.L)

                -- 0x6E
                B.HexByte B.H6 B.HE ->
                    -- LD L, (HL)
                    Ok <| LDRegister (RegArg8 CPU.L) (Address16 CPU.HL)

                -- 0x6F
                B.HexByte B.H6 B.HF ->
                    -- LD L, A
                    Ok <| LDRegister (RegArg8 CPU.L) (RegArg8 CPU.A)

                -- 0x70
                B.HexByte B.H7 B.H0 ->
                    -- LD (HL), B
                    Ok <| LDRegister (Address16 CPU.HL) (RegArg8 CPU.B)

                -- 0x71
                B.HexByte B.H7 B.H1 ->
                    -- LD (HL), C
                    Ok <| LDRegister (Address16 CPU.HL) (RegArg8 CPU.C)

                -- 0x72
                B.HexByte B.H7 B.H2 ->
                    -- LD (HL), D
                    Ok <| LDRegister (Address16 CPU.HL) (RegArg8 CPU.D)

                -- 0x73
                B.HexByte B.H7 B.H3 ->
                    -- LD (HL), E
                    Ok <| LDRegister (Address16 CPU.HL) (RegArg8 CPU.E)

                -- 0x74
                B.HexByte B.H7 B.H4 ->
                    -- LD (HL), H
                    Ok <| LDRegister (Address16 CPU.HL) (RegArg8 CPU.H)

                -- 0x75
                B.HexByte B.H7 B.H5 ->
                    -- LD (HL), L
                    Ok <| LDRegister (Address16 CPU.HL) (RegArg8 CPU.L)

                -- 0x76
                B.HexByte B.H7 B.H6 ->
                    -- HALT
                    Ok HALT

                -- 0x77
                B.HexByte B.H7 B.H7 ->
                    -- LD (HL), A
                    Ok <| LDRegister (Address16 CPU.HL) (RegArg8 CPU.A)

                -- 0x78
                B.HexByte B.H7 B.H8 ->
                    -- LD A, B
                    Ok <| LDRegister (RegArg8 CPU.A) (RegArg8 CPU.B)

                -- 0x79
                B.HexByte B.H7 B.H9 ->
                    -- LD A, C
                    Ok <| LDRegister (RegArg8 CPU.A) (RegArg8 CPU.C)

                -- 0x7A
                B.HexByte B.H7 B.HA ->
                    -- LD A, D
                    Ok <| LDRegister (RegArg8 CPU.A) (RegArg8 CPU.D)

                -- 0x7B
                B.HexByte B.H7 B.HB ->
                    -- LD A, E
                    Ok <| LDRegister (RegArg8 CPU.A) (RegArg8 CPU.E)

                -- 0x7C
                B.HexByte B.H7 B.HC ->
                    -- LD A, H
                    Ok <| LDRegister (RegArg8 CPU.A) (RegArg8 CPU.H)

                -- 0x7D
                B.HexByte B.H7 B.HD ->
                    -- LD A, L
                    Ok <| LDRegister (RegArg8 CPU.A) (RegArg8 CPU.L)

                -- 0x7E
                B.HexByte B.H7 B.HE ->
                    -- LD A, (HL)
                    Ok <| LDRegister (RegArg8 CPU.A) (Address16 CPU.HL)

                -- 0x7F
                B.HexByte B.H7 B.HF ->
                    -- LD A, A
                    Ok <| LDRegister (RegArg8 CPU.A) (RegArg8 CPU.A)

                -- 0x0A
                B.HexByte B.H0 B.HA ->
                    -- LD A, (BC)
                    Ok <| LDRegister (RegArg8 CPU.A) (Address16 CPU.BC)

                -- 0x1A
                B.HexByte B.H1 B.HA ->
                    -- LD A, (DE)
                    Ok <| LDRegister (RegArg8 CPU.A) (Address16 CPU.DE)

                -- 0x2A
                B.HexByte B.H2 B.HA ->
                    -- LD A, (HL+)
                    Ok <| LDI (RegArg8 CPU.A) (Address16 CPU.HL)

                -- 0x3A
                B.HexByte B.H3 B.HA ->
                    -- LD A, (HL-)
                    Ok <| LDD (RegArg8 CPU.A) (Address16 CPU.HL)

                ---- ADD ----
                -- 0x80
                B.HexByte B.H8 B.H0 ->
                    -- ADD A, B
                    Ok <| ADD (RegArg8 CPU.B)

                -- 0x81
                B.HexByte B.H8 B.H1 ->
                    -- ADD A, C
                    Ok <| ADD (RegArg8 CPU.C)

                -- 0x82
                B.HexByte B.H8 B.H2 ->
                    -- ADD A, D
                    Ok <| ADD (RegArg8 CPU.D)

                -- 0x83
                B.HexByte B.H8 B.H3 ->
                    -- ADD A, E
                    Ok <| ADD (RegArg8 CPU.E)

                -- 0x84
                B.HexByte B.H8 B.H4 ->
                    -- ADD A, H
                    Ok <| ADD (RegArg8 CPU.H)

                -- 0x85
                B.HexByte B.H8 B.H5 ->
                    -- ADD A, L
                    Ok <| ADD (RegArg8 CPU.L)

                -- 0x86
                B.HexByte B.H8 B.H6 ->
                    -- ADD A, (HL)
                    Ok <| ADD (Address16 CPU.HL)

                -- 0x87
                B.HexByte B.H8 B.H7 ->
                    -- ADD A, A
                    Ok <| ADD (RegArg8 CPU.A)

                -- 0x88
                B.HexByte B.H8 B.H8 ->
                    -- ADC A, B
                    Ok <| ADC (RegArg8 CPU.B)

                -- 0x89
                B.HexByte B.H8 B.H9 ->
                    -- ADC A, C
                    Ok <| ADC (RegArg8 CPU.C)

                -- 0x8A
                B.HexByte B.H8 B.HA ->
                    -- ADC A, D
                    Ok <| ADC (RegArg8 CPU.D)

                -- 0x8B
                B.HexByte B.H8 B.HB ->
                    -- ADC A, E
                    Ok <| ADC (RegArg8 CPU.E)

                -- 0x8C
                B.HexByte B.H8 B.HC ->
                    -- ADC A, H
                    Ok <| ADC (RegArg8 CPU.H)

                -- 0x8D
                B.HexByte B.H8 B.HD ->
                    -- ADC A, L
                    Ok <| ADC (RegArg8 CPU.L)

                -- 0x8E
                B.HexByte B.H8 B.HE ->
                    -- ADC A, (HL)
                    Ok <| ADC (Address16 CPU.HL)

                -- 0x8F
                B.HexByte B.H8 B.HF ->
                    -- ADC A, A
                    Ok <| ADC (RegArg8 CPU.C)

                ---- ALU Immediate Values ----
                -- 0xC6
                B.HexByte B.HC B.H6 ->
                    case byte2 of
                        Just b ->
                            -- ADD A, d8
                            Ok <| ADDEightBitValue (RegArg8 CPU.A) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "ADD A, d8 requires a second byte" }

                -- 0xD6
                B.HexByte B.HD B.H6 ->
                    case byte2 of
                        Just b ->
                            -- SUB d8
                            Ok <| SUBEightBitValue b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "SUB d8 requires a second byte" }

                -- 0xE6
                B.HexByte B.HE B.H6 ->
                    case byte2 of
                        Just b ->
                            -- AND d8
                            Ok <| ANDEightBitValue b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "AND d8 requires a second byte" }

                -- 0xF6
                B.HexByte B.HF B.H6 ->
                    case byte2 of
                        Just b ->
                            -- OR d8
                            Ok <| OREightBitValue b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "OR d8 requires a second byte" }

                -- 0xCE
                B.HexByte B.HC B.HE ->
                    case byte2 of
                        Just b ->
                            -- ADC A, d8
                            Ok <| ADCEightBitValue (RegArg8 CPU.A) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "ADC A, d8 requires a second byte" }

                -- 0xDE
                B.HexByte B.HD B.HE ->
                    case byte2 of
                        Just b ->
                            -- SBC A, d8
                            Ok <| SBCEightBitValue (RegArg8 CPU.A) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "SBC A, d8 requires a second byte" }

                -- 0xEE
                B.HexByte B.HE B.HE ->
                    case byte2 of
                        Just b ->
                            -- XOR d8
                            Ok <| XOREightBitValue b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "XOR d8 requires a second byte" }

                -- 0xFE
                B.HexByte B.HF B.HE ->
                    case byte2 of
                        Just b ->
                            -- CP d8
                            Ok <| CPEightBitValue b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "CP d8 requires a second byte" }

                ---- SUB ----
                -- 0x90
                B.HexByte B.H9 B.H0 ->
                    -- SUB A, B
                    Ok <| SUB (RegArg8 CPU.B)

                -- 0x91
                B.HexByte B.H9 B.H1 ->
                    -- SUB A, C
                    Ok <| SUB (RegArg8 CPU.C)

                -- 0x92
                B.HexByte B.H9 B.H2 ->
                    -- SUB A, D
                    Ok <| SUB (RegArg8 CPU.D)

                -- 0x93
                B.HexByte B.H9 B.H3 ->
                    -- SUB A, E
                    Ok <| SUB (RegArg8 CPU.E)

                -- 0x94
                B.HexByte B.H9 B.H4 ->
                    -- SUB A, H
                    Ok <| SUB (RegArg8 CPU.H)

                -- 0x95
                B.HexByte B.H9 B.H5 ->
                    -- SUB A, L
                    Ok <| SUB (RegArg8 CPU.L)

                -- 0x96
                B.HexByte B.H9 B.H6 ->
                    -- SUB A, (HL)
                    Ok <| SUB (Address16 CPU.HL)

                -- 0x97
                B.HexByte B.H9 B.H7 ->
                    -- SUB A, A
                    Ok <| SUB (RegArg8 CPU.A)

                -- 0x98
                B.HexByte B.H9 B.H8 ->
                    -- SBC A, B
                    Ok <| SBC (RegArg8 CPU.B)

                -- 0x99
                B.HexByte B.H9 B.H9 ->
                    -- SBC A, C
                    Ok <| SBC (RegArg8 CPU.C)

                -- 0x9A
                B.HexByte B.H9 B.HA ->
                    -- SBC A, D
                    Ok <| SBC (RegArg8 CPU.D)

                -- 0x9B
                B.HexByte B.H9 B.HB ->
                    -- SBC A, E
                    Ok <| SBC (RegArg8 CPU.E)

                -- 0x9C
                B.HexByte B.H9 B.HC ->
                    -- SBC A, H
                    Ok <| SBC (RegArg8 CPU.H)

                -- 0x9D
                B.HexByte B.H9 B.HD ->
                    -- SBC A, L
                    Ok <| SBC (RegArg8 CPU.L)

                -- 0x9E
                B.HexByte B.H9 B.HE ->
                    -- SBC A, (HL)
                    Ok <| SBC (Address16 CPU.HL)

                -- 0x9F
                B.HexByte B.H9 B.HF ->
                    -- SBC A, A
                    Ok <| SBC (RegArg8 CPU.C)

                ---- SUB ----
                -- 0xA0
                B.HexByte B.HA B.H0 ->
                    -- AND B
                    Ok <| AND (RegArg8 CPU.B)

                -- 0xA1
                B.HexByte B.HA B.H1 ->
                    -- AND C
                    Ok <| AND (RegArg8 CPU.C)

                -- 0xA2
                B.HexByte B.HA B.H2 ->
                    -- AND D
                    Ok <| AND (RegArg8 CPU.D)

                -- 0xA3
                B.HexByte B.HA B.H3 ->
                    -- AND E
                    Ok <| AND (RegArg8 CPU.E)

                -- 0xA4
                B.HexByte B.HA B.H4 ->
                    -- AND H
                    Ok <| AND (RegArg8 CPU.H)

                -- 0xA5
                B.HexByte B.HA B.H5 ->
                    -- AND L
                    Ok <| AND (RegArg8 CPU.L)

                -- 0xA6
                B.HexByte B.HA B.H6 ->
                    -- AND (HL)
                    Ok <| AND (Address16 CPU.HL)

                -- 0xA7
                B.HexByte B.HA B.H7 ->
                    -- AND A
                    Ok <| AND (RegArg8 CPU.A)

                ---- XOR ----
                -- 0xA8
                B.HexByte B.HA B.H8 ->
                    -- XOR B
                    Ok <| XOR (RegArg8 CPU.B)

                -- 0xA9
                B.HexByte B.HA B.H9 ->
                    -- XOR C
                    Ok <| XOR (RegArg8 CPU.C)

                -- 0xAA
                B.HexByte B.HA B.HA ->
                    -- XOR D
                    Ok <| XOR (RegArg8 CPU.D)

                -- 0xAB
                B.HexByte B.HA B.HB ->
                    -- XOR E
                    Ok <| XOR (RegArg8 CPU.E)

                -- 0xAC
                B.HexByte B.HA B.HC ->
                    -- XOR H
                    Ok <| XOR (RegArg8 CPU.H)

                -- 0xAD
                B.HexByte B.HA B.HD ->
                    -- XOR L
                    Ok <| XOR (RegArg8 CPU.L)

                -- 0xAE
                B.HexByte B.HA B.HE ->
                    -- XOR (HL)
                    Ok <| XOR (Address16 CPU.HL)

                -- 0xAF
                B.HexByte B.HA B.HF ->
                    -- XOR A
                    Ok <| XOR (RegArg8 CPU.C)

                ---- SUB ----
                -- 0xB0
                B.HexByte B.HB B.H0 ->
                    -- OR B
                    Ok <| OR (RegArg8 CPU.B)

                -- 0xB1
                B.HexByte B.HB B.H1 ->
                    -- OR C
                    Ok <| OR (RegArg8 CPU.C)

                -- 0xB2
                B.HexByte B.HB B.H2 ->
                    -- OR D
                    Ok <| OR (RegArg8 CPU.D)

                -- 0xB3
                B.HexByte B.HB B.H3 ->
                    -- OR E
                    Ok <| OR (RegArg8 CPU.E)

                -- 0xB4
                B.HexByte B.HB B.H4 ->
                    -- OR H
                    Ok <| OR (RegArg8 CPU.H)

                -- 0xB5
                B.HexByte B.HB B.H5 ->
                    -- OR L
                    Ok <| OR (RegArg8 CPU.L)

                -- 0xB6
                B.HexByte B.HB B.H6 ->
                    -- OR (HL)
                    Ok <| OR (Address16 CPU.HL)

                -- 0xB7
                B.HexByte B.HB B.H7 ->
                    -- OR A
                    Ok <| OR (RegArg8 CPU.A)

                ---- CP ----
                -- 0xB8
                B.HexByte B.HB B.H8 ->
                    -- CP B
                    Ok <| CP (RegArg8 CPU.B)

                -- 0xB9
                B.HexByte B.HB B.H9 ->
                    -- CP C
                    Ok <| CP (RegArg8 CPU.C)

                -- 0xBA
                B.HexByte B.HB B.HA ->
                    -- CP D
                    Ok <| CP (RegArg8 CPU.D)

                -- 0xBB
                B.HexByte B.HB B.HB ->
                    -- CP E
                    Ok <| CP (RegArg8 CPU.E)

                -- 0xBC
                B.HexByte B.HB B.HC ->
                    -- CP H
                    Ok <| CP (RegArg8 CPU.H)

                -- 0xBD
                B.HexByte B.HB B.HD ->
                    -- CP L
                    Ok <| CP (RegArg8 CPU.L)

                -- 0xBE
                B.HexByte B.HB B.HE ->
                    -- CP (HL)
                    Ok <| CP (Address16 CPU.HL)

                -- 0xBF
                B.HexByte B.HB B.HF ->
                    -- CP A
                    Ok <| CP (RegArg8 CPU.C)

                ---- RET ----
                -- 0xC0
                B.HexByte B.HC B.H0 ->
                    -- RET NZ
                    Ok <| RETFlag CPU.NonZeroFlag

                -- 0xD0
                B.HexByte B.HD B.H0 ->
                    -- RET NC
                    Ok <| RETFlag CPU.NonCarryFlag

                -- 0xC8
                B.HexByte B.HC B.H8 ->
                    -- RET Z
                    Ok <| RETFlag CPU.ZeroFlag

                -- 0xD8
                B.HexByte B.HD B.H8 ->
                    -- RET C
                    Ok <| RETFlag CPU.CarryFlag

                -- 0xC9
                B.HexByte B.HC B.H9 ->
                    -- RET
                    Ok RET

                -- 0xD9
                B.HexByte B.HD B.H9 ->
                    -- RETI
                    Ok RETI

                ---- POP ----
                -- 0xC1
                B.HexByte B.HC B.H1 ->
                    -- POP BC
                    Ok <| POP (RegArg16 CPU.BC)

                -- 0xD1
                B.HexByte B.HD B.H1 ->
                    -- POP DE
                    Ok <| POP (RegArg16 CPU.DE)

                -- 0xE1
                B.HexByte B.HE B.H1 ->
                    -- POP HL
                    Ok <| POP (RegArg16 CPU.HL)

                -- 0xF1
                B.HexByte B.HF B.H1 ->
                    -- POP AF
                    Ok <| POP (RegArg16 CPU.AF)

                ---- PUSH ----
                -- 0xC5
                B.HexByte B.HC B.H5 ->
                    -- PUSH BC
                    Ok <| PUSH (RegArg16 CPU.BC)

                -- 0xD5
                B.HexByte B.HD B.H5 ->
                    -- PUSH DE
                    Ok <| PUSH (RegArg16 CPU.DE)

                -- 0xE5
                B.HexByte B.HE B.H5 ->
                    -- PUSH HL
                    Ok <| PUSH (RegArg16 CPU.HL)

                -- 0xF5
                B.HexByte B.HF B.H5 ->
                    -- PUSH AF
                    Ok <| PUSH (RegArg16 CPU.AF)

                ---- INC ----
                -- 0x03
                B.HexByte B.H0 B.H3 ->
                    -- INC BC
                    Ok <| INC (RegArg16 CPU.BC)

                -- 0x13
                B.HexByte B.H1 B.H3 ->
                    -- INC DE
                    Ok <| INC (RegArg16 CPU.DE)

                -- 0x23
                B.HexByte B.H2 B.H3 ->
                    -- INC HL
                    Ok <| INC (RegArg16 CPU.HL)

                -- 0x33
                B.HexByte B.H3 B.H3 ->
                    -- INC SP
                    Ok <| INC (RegArg16 CPU.SP)

                -- 0x04
                B.HexByte B.H0 B.H4 ->
                    -- INC B
                    Ok <| INC (RegArg8 CPU.B)

                -- 0x14
                B.HexByte B.H1 B.H4 ->
                    -- INC D
                    Ok <| INC (RegArg8 CPU.D)

                -- 0x24
                B.HexByte B.H2 B.H4 ->
                    -- INC H
                    Ok <| INC (RegArg8 CPU.H)

                -- 0x34
                B.HexByte B.H3 B.H4 ->
                    -- INC (HL)
                    Ok <| INC (Address16 CPU.HL)

                -- 0x0C
                B.HexByte B.H0 B.HC ->
                    -- INC C
                    Ok <| INC (RegArg8 CPU.C)

                -- 0x1C
                B.HexByte B.H1 B.HC ->
                    -- INC E
                    Ok <| INC (RegArg8 CPU.E)

                -- 0x2C
                B.HexByte B.H2 B.HC ->
                    -- INC L
                    Ok <| INC (RegArg8 CPU.L)

                -- 0x3C
                B.HexByte B.H3 B.HC ->
                    -- INC A
                    Ok <| INC (RegArg8 CPU.A)

                ---- DEC ---
                -- 0x05
                B.HexByte B.H0 B.H5 ->
                    -- DEC B
                    Ok <| DEC (RegArg8 CPU.B)

                -- 0x15
                B.HexByte B.H1 B.H5 ->
                    -- DEC D
                    Ok <| DEC (RegArg8 CPU.D)

                -- 0x25
                B.HexByte B.H2 B.H5 ->
                    -- DEC H
                    Ok <| DEC (RegArg8 CPU.H)

                -- 0x35
                B.HexByte B.H3 B.H5 ->
                    -- DEC (HL)
                    Ok <| DEC (Address16 CPU.HL)

                -- 0x0B
                B.HexByte B.H0 B.HB ->
                    -- DEC BC
                    Ok <| DEC (RegArg16 CPU.BC)

                -- 0x1B
                B.HexByte B.H1 B.HB ->
                    -- DEC DE
                    Ok <| DEC (RegArg16 CPU.DE)

                -- 0x2B
                B.HexByte B.H2 B.HB ->
                    -- DEC HL
                    Ok <| DEC (RegArg16 CPU.HL)

                -- 0x3B
                B.HexByte B.H3 B.HB ->
                    -- DEC SP
                    Ok <| DEC (RegArg16 CPU.SP)

                -- 0x0D
                B.HexByte B.H0 B.HD ->
                    -- DEC C
                    Ok <| DEC (RegArg8 CPU.C)

                -- 0x1D
                B.HexByte B.H1 B.HD ->
                    -- DEC E
                    Ok <| DEC (RegArg8 CPU.D)

                -- 0x2D
                B.HexByte B.H2 B.HD ->
                    -- DEC L
                    Ok <| DEC (RegArg8 CPU.L)

                -- 0x3D
                B.HexByte B.H3 B.HD ->
                    -- DEC A
                    Ok <| DEC (RegArg8 CPU.A)

                ---- RR / RL ---
                -- 0x07
                B.HexByte B.H0 B.H7 ->
                    -- RLCA
                    Ok RLCA

                -- 0x17
                B.HexByte B.H1 B.H7 ->
                    -- RLA
                    Ok RLA

                -- 0x0F
                B.HexByte B.H0 B.HF ->
                    -- RRCA
                    Ok RRCA

                --- 0x1F
                B.HexByte B.H1 B.HF ->
                    -- RRA
                    Ok RRA

                -- 0x09
                B.HexByte B.H0 B.H9 ->
                    -- ADD HL, BC
                    Ok <| ADDHL (RegArg16 CPU.BC)

                -- 0x19
                B.HexByte B.H1 B.H9 ->
                    -- ADD HL, DE
                    Ok <| ADDHL (RegArg16 CPU.DE)

                -- 0x29
                B.HexByte B.H2 B.H9 ->
                    -- ADD HL, HL
                    Ok <| ADDHL (RegArg16 CPU.HL)

                -- 0x39
                B.HexByte B.H3 B.H9 ->
                    -- ADD HL, SP
                    Ok <| ADDHL (RegArg16 CPU.SP)

                ---- MISC ----
                -- 0x10
                B.HexByte B.H1 B.H0 ->
                    -- STOP 0
                    Ok STOP

                -- 0x27
                B.HexByte B.H2 B.H7 ->
                    -- DAA
                    Ok DAA

                -- 0x37
                B.HexByte B.H3 B.H7 ->
                    -- SCF
                    Ok SCF

                -- 0x2F
                B.HexByte B.H2 B.HF ->
                    -- CPL
                    Ok CPL

                -- 0x3F
                B.HexByte B.H3 B.HF ->
                    -- CCF
                    Ok CCF

                ---- RST ----
                -- 0xC7
                B.HexByte B.HC B.H7 ->
                    -- RST 00H
                    Ok <| RST B.zeroByte

                -- 0xD7
                B.HexByte B.HD B.H7 ->
                    -- RST 10H
                    Ok <| RST (B.Byte O O O I O O O O)

                -- 0xE7
                B.HexByte B.HE B.H7 ->
                    -- RST 20H
                    Ok <| RST (B.Byte O O I O O O O O)

                -- 0xF7
                B.HexByte B.HF B.H7 ->
                    -- RST 30H
                    Ok <| RST (B.Byte O O I I O O O O)

                -- 0xCF
                B.HexByte B.HC B.HF ->
                    -- RST 08H
                    Ok <| RST (B.Byte O O O O I O O O)

                -- 0xDF
                B.HexByte B.HD B.HF ->
                    -- RST 28H
                    Ok <| RST (B.Byte O O O I I O O O)

                --0xEF
                B.HexByte B.HE B.HF ->
                    -- RST 28H
                    Ok <| RST (B.Byte O O I O I O O O)

                --0xFF
                B.HexByte B.HF B.HF ->
                    -- RST 38H
                    Ok <| RST (B.Byte O O I I I O O O)

                ---- LD ----
                -- 0x06
                B.HexByte B.H0 B.H6 ->
                    case byte2 of
                        Just b ->
                            -- LD B,d8
                            Ok <| LDEightBitValue (RegArg8 CPU.B) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LD B, d8 requires a second byte" }

                -- 0x16
                B.HexByte B.H1 B.H6 ->
                    case byte2 of
                        Just b ->
                            -- LD D,d8
                            Ok <| LDEightBitValue (RegArg8 CPU.D) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LD D, d8 requires a second byte" }

                -- 0x26
                B.HexByte B.H2 B.H6 ->
                    case byte2 of
                        Just b ->
                            -- LD H,d8
                            Ok <| LDEightBitValue (RegArg8 CPU.H) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LD H, d8 requires a second byte" }

                -- 0x36
                B.HexByte B.H3 B.H6 ->
                    case byte2 of
                        Just b ->
                            -- LD (HL),d8
                            Ok <| LDEightBitValue (Address16 CPU.HL) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LD (HL), d8 requires a second byte" }

                -- 0x0E
                B.HexByte B.H0 B.HE ->
                    case byte2 of
                        Just b ->
                            -- LD C,d8
                            Ok <| LDEightBitValue (RegArg8 CPU.C) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LD, C, d8 requires a second byte" }

                -- 0x1E
                B.HexByte B.H1 B.HE ->
                    case byte2 of
                        Just b ->
                            -- LD E,d8
                            Ok <| LDEightBitValue (RegArg8 CPU.E) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LD E, d8 requires a second byte" }

                -- 0x2E
                B.HexByte B.H2 B.HE ->
                    case byte2 of
                        Just b ->
                            -- LD L,d8
                            Ok <| LDEightBitValue (RegArg8 CPU.L) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LD L, d8 requires a second byte" }

                -- 0x3E
                B.HexByte B.H3 B.HE ->
                    case byte2 of
                        Just b ->
                            --LD A,d8
                            Ok <| LDEightBitValue (RegArg8 CPU.A) b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LD A, d8 requires a second byte" }

                -- 0xE2
                B.HexByte B.HE B.H2 ->
                    -- LD (C),A
                    Ok <| LDRegister (Address8 CPU.C) (RegArg8 CPU.A)

                -- 0xF2
                B.HexByte B.HF B.H2 ->
                    --LD A,(c)
                    Ok <| LDRegister (RegArg8 CPU.A) (Address8 CPU.C)

                ---- LDH ----
                -- 0xE0
                B.HexByte B.HE B.H0 ->
                    case byte2 of
                        Just b ->
                            --LDH (a8),A
                            Ok <| LDHRegisterToImmediateAddress (ImmediateAddress8 b) (RegArg8 CPU.A)

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LDH (a8), A requires a second byte" }

                -- 0xF0
                B.HexByte B.HF B.H0 ->
                    case byte2 of
                        Just b ->
                            --LDH A,(a8)
                            Ok <| LDHImmediateAddressToRegister (RegArg8 CPU.A) (ImmediateAddress8 b)

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "LDH A, (a8) requires a second byte" }

                ---- PREFIX CB ----
                B.HexByte B.HC B.HB ->
                    case byte2 of
                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "0xCB prefixed opcodes require a second byte" }

                        Just b ->
                            case (B.binaryByteToHexByte b) of
                                ---- RLC ----
                                -- 0x00
                                B.HexByte B.H0 B.H0 ->
                                    -- RLC B
                                    Ok <| RLC (RegArg8 CPU.B)

                                -- 0x01
                                B.HexByte B.H0 B.H1 ->
                                    -- RLC C
                                    Ok <| RLC (RegArg8 CPU.C)

                                -- 0x02
                                B.HexByte B.H0 B.H2 ->
                                    -- RLC D
                                    Ok <| RLC (RegArg8 CPU.D)

                                -- 0x03
                                B.HexByte B.H0 B.H3 ->
                                    -- RLC E
                                    Ok <| RLC (RegArg8 CPU.E)

                                -- 0x04
                                B.HexByte B.H0 B.H4 ->
                                    -- RLC H
                                    Ok <| RLC (RegArg8 CPU.H)

                                -- 0x05
                                B.HexByte B.H0 B.H5 ->
                                    -- RLC L
                                    Ok <| RLC (RegArg8 CPU.L)

                                -- 0x06
                                B.HexByte B.H0 B.H6 ->
                                    -- RLC (HL)
                                    Ok <| RLC (Address16 CPU.HL)

                                -- 0x07
                                B.HexByte B.H0 B.H7 ->
                                    -- RLC A
                                    Ok <| RLC (RegArg8 CPU.A)

                                ---- RRC ----
                                -- 0x08
                                B.HexByte B.H0 B.H8 ->
                                    -- RRC B
                                    Ok <| RRC (RegArg8 CPU.B)

                                -- 0x09
                                B.HexByte B.H0 B.H9 ->
                                    -- RRC C
                                    Ok <| RRC (RegArg8 CPU.C)

                                -- 0x0A
                                B.HexByte B.H0 B.HA ->
                                    -- RRC D
                                    Ok <| RRC (RegArg8 CPU.D)

                                -- 0x0B
                                B.HexByte B.H0 B.HB ->
                                    -- RRC E
                                    Ok <| RRC (RegArg8 CPU.E)

                                -- 0x0C
                                B.HexByte B.H0 B.HC ->
                                    -- RRC H
                                    Ok <| RRC (RegArg8 CPU.H)

                                -- 0x0D
                                B.HexByte B.H0 B.HD ->
                                    -- RRC L
                                    Ok <| RRC (RegArg8 CPU.L)

                                -- 0x0E
                                B.HexByte B.H0 B.HE ->
                                    -- RRC (HL)
                                    Ok <| RRC (Address16 CPU.HL)

                                -- 0x0F
                                B.HexByte B.H0 B.HF ->
                                    -- RRC A
                                    Ok <| RRC (RegArg8 CPU.A)

                                ---- RL ----
                                -- 0x10
                                B.HexByte B.H1 B.H0 ->
                                    -- RL B
                                    Ok <| RL (RegArg8 CPU.B)

                                B.HexByte B.H1 B.H1 ->
                                    -- RL C
                                    Ok <| RL (RegArg8 CPU.C)

                                -- 0x12
                                B.HexByte B.H1 B.H2 ->
                                    -- RL D
                                    Ok <| RL (RegArg8 CPU.D)

                                -- 0x13
                                B.HexByte B.H1 B.H3 ->
                                    -- RL D
                                    Ok <| RL (RegArg8 CPU.E)

                                -- 0x14
                                B.HexByte B.H1 B.H4 ->
                                    -- RL H
                                    Ok <| RL (RegArg8 CPU.H)

                                -- 0x15
                                B.HexByte B.H1 B.H5 ->
                                    -- RL L
                                    Ok <| RL (RegArg8 CPU.L)

                                -- 0x16
                                B.HexByte B.H1 B.H6 ->
                                    -- RL (HL)
                                    Ok <| RL (Address16 CPU.HL)

                                -- 0x17
                                B.HexByte B.H1 B.H7 ->
                                    -- RL A
                                    Ok <| RL (RegArg8 CPU.A)

                                ---- RR ----
                                -- 0x18
                                B.HexByte B.H1 B.H8 ->
                                    -- RL A
                                    Ok <| RR (RegArg8 CPU.B)

                                -- 0x19
                                B.HexByte B.H1 B.H9 ->
                                    -- RL C
                                    Ok <| RR (RegArg8 CPU.C)

                                -- 0x1A
                                B.HexByte B.H1 B.HA ->
                                    -- RL D
                                    Ok <| RR (RegArg8 CPU.D)

                                -- 0x1B
                                B.HexByte B.H1 B.HB ->
                                    -- RL E
                                    Ok <| RR (RegArg8 CPU.E)

                                -- 0x1C
                                B.HexByte B.H1 B.HC ->
                                    -- RL H
                                    Ok <| RR (RegArg8 CPU.H)

                                -- 0x1D
                                B.HexByte B.H1 B.HD ->
                                    -- RL L
                                    Ok <| RR (RegArg8 CPU.L)

                                -- 0x1E
                                B.HexByte B.H1 B.HE ->
                                    -- RL (HL)
                                    Ok <| RR (Address16 CPU.HL)

                                -- 0x1F
                                B.HexByte B.H1 B.HF ->
                                    -- RL L
                                    Ok <| RR (RegArg8 CPU.A)

                                -- 0x20
                                B.HexByte B.H2 B.H0 ->
                                    -- SLA B
                                    Ok <| SLA (RegArg8 CPU.B)

                                -- 0x21
                                B.HexByte B.H2 B.H1 ->
                                    -- SLA C
                                    Ok <| SLA (RegArg8 CPU.C)

                                -- 0x22
                                B.HexByte B.H2 B.H2 ->
                                    -- SLA D
                                    Ok <| SLA (RegArg8 CPU.D)

                                -- 0x23
                                B.HexByte B.H2 B.H3 ->
                                    -- SLA E
                                    Ok <| SLA (RegArg8 CPU.E)

                                -- 0x24
                                B.HexByte B.H2 B.H4 ->
                                    -- SLA H
                                    Ok <| SLA (RegArg8 CPU.H)

                                -- 0x25
                                B.HexByte B.H2 B.H5 ->
                                    -- SLA L
                                    Ok <| SLA (RegArg8 CPU.L)

                                -- 0x26
                                B.HexByte B.H2 B.H6 ->
                                    -- SLA (HL)
                                    Ok <| SLA (Address16 CPU.HL)

                                -- 0x27
                                B.HexByte B.H2 B.H7 ->
                                    -- SLA A
                                    Ok <| SLA (RegArg8 CPU.A)

                                -- 0x28
                                B.HexByte B.H2 B.H8 ->
                                    -- SRA B
                                    Ok <| SRA (RegArg8 CPU.B)

                                -- 0x29
                                B.HexByte B.H2 B.H9 ->
                                    -- SRA C
                                    Ok <| SRA (RegArg8 CPU.C)

                                -- 0x2A
                                B.HexByte B.H2 B.HA ->
                                    -- SRA D
                                    Ok <| SRA (RegArg8 CPU.D)

                                -- 0x2B
                                B.HexByte B.H2 B.HB ->
                                    -- SRA E
                                    Ok <| SRA (RegArg8 CPU.E)

                                -- 0x2C
                                B.HexByte B.H2 B.HC ->
                                    -- SRA H
                                    Ok <| SRA (RegArg8 CPU.H)

                                -- 0x2D
                                B.HexByte B.H2 B.HD ->
                                    -- SRA L
                                    Ok <| SRA (RegArg8 CPU.L)

                                -- 0x2E
                                B.HexByte B.H2 B.HE ->
                                    -- SRA (HL)
                                    Ok <| SRA (Address16 CPU.HL)

                                -- 0x2F
                                B.HexByte B.H2 B.HF ->
                                    -- SRA A
                                    Ok <| SRA (RegArg8 CPU.A)

                                -- 0x30
                                B.HexByte B.H3 B.H0 ->
                                    -- SWAP B
                                    Ok <| SWAP (RegArg8 CPU.B)

                                -- 0x31
                                B.HexByte B.H3 B.H1 ->
                                    -- SWAP C
                                    Ok <| SWAP (RegArg8 CPU.C)

                                -- 0x32
                                B.HexByte B.H3 B.H2 ->
                                    -- SWAP D
                                    Ok <| SWAP (RegArg8 CPU.D)

                                -- 0x33
                                B.HexByte B.H3 B.H3 ->
                                    -- SWAP E
                                    Ok <| SWAP (RegArg8 CPU.E)

                                -- 0x34
                                B.HexByte B.H3 B.H4 ->
                                    -- SWAP H
                                    Ok <| SWAP (RegArg8 CPU.H)

                                -- 0x35
                                B.HexByte B.H3 B.H5 ->
                                    -- SWAP L
                                    Ok <| SWAP (RegArg8 CPU.L)

                                -- 0x36
                                B.HexByte B.H3 B.H6 ->
                                    -- SWAP (HL)
                                    Ok <| SWAP (Address16 CPU.HL)

                                -- 0x37
                                B.HexByte B.H3 B.H7 ->
                                    -- SWAP A
                                    Ok <| SWAP (RegArg8 CPU.A)

                                ---- SRL ----
                                -- 0x38
                                B.HexByte B.H3 B.H8 ->
                                    -- SRL B
                                    Ok <| SRL (RegArg8 CPU.B)

                                -- 0x39
                                B.HexByte B.H3 B.H9 ->
                                    -- SRL C
                                    Ok <| SRL (RegArg8 CPU.C)

                                -- 0x3A
                                B.HexByte B.H3 B.HA ->
                                    -- SRL D
                                    Ok <| SRL (RegArg8 CPU.D)

                                -- 0x3B
                                B.HexByte B.H3 B.HB ->
                                    -- SRL E
                                    Ok <| SRL (RegArg8 CPU.E)

                                -- 0x3C
                                B.HexByte B.H3 B.HC ->
                                    -- SRL H
                                    Ok <| SRL (RegArg8 CPU.H)

                                -- 0x3D
                                B.HexByte B.H3 B.HD ->
                                    -- SRL L
                                    Ok <| SRL (RegArg8 CPU.L)

                                -- 0x3E
                                B.HexByte B.H3 B.HE ->
                                    -- SRL (HL)
                                    Ok <| SRL (Address16 CPU.HL)

                                -- 0x3F
                                B.HexByte B.H3 B.HF ->
                                    -- SRL A
                                    Ok <| SRL (RegArg8 CPU.A)

                                ---- BIT ----
                                -- 0x40
                                B.HexByte B.H4 B.H0 ->
                                    -- BIT 0 B
                                    Ok <| BIT B.ZeroIndex (RegArg8 CPU.B)

                                -- 0x41
                                B.HexByte B.H4 B.H1 ->
                                    -- BIT 0 C
                                    Ok <|
                                        BIT B.ZeroIndex
                                            (RegArg8 CPU.C)

                                -- 0x42
                                B.HexByte B.H4 B.H2 ->
                                    -- BIT 0 D
                                    Ok <|
                                        BIT B.ZeroIndex
                                            (RegArg8 CPU.D)

                                -- 0x43
                                B.HexByte B.H4 B.H3 ->
                                    -- BIT 0 E
                                    Ok <|
                                        BIT B.ZeroIndex
                                            (RegArg8 CPU.E)

                                -- 0x44
                                B.HexByte B.H4 B.H4 ->
                                    -- BIT 0 H
                                    Ok <|
                                        BIT B.ZeroIndex
                                            (RegArg8 CPU.H)

                                -- 0x45
                                B.HexByte B.H4 B.H5 ->
                                    -- BIT 0 L
                                    Ok <|
                                        BIT B.ZeroIndex
                                            (RegArg8 CPU.L)

                                -- 0x46
                                B.HexByte B.H4 B.H6 ->
                                    -- BIT 0 (HL)
                                    Ok <| BIT B.ZeroIndex (Address16 CPU.HL)

                                -- 0x47
                                B.HexByte B.H4 B.H7 ->
                                    -- BIT 0 A
                                    Ok <|
                                        BIT B.ZeroIndex
                                            (RegArg8 CPU.A)

                                -- 0x48
                                B.HexByte B.H4 B.H8 ->
                                    -- BIT 1 B
                                    Ok <|
                                        BIT B.OneIndex
                                            (RegArg8 CPU.B)

                                -- 0x49
                                B.HexByte B.H4 B.H9 ->
                                    -- BIT 1 C
                                    Ok <|
                                        BIT B.OneIndex
                                            (RegArg8 CPU.C)

                                -- 0x4A
                                B.HexByte B.H4 B.HA ->
                                    -- BIT 1 D
                                    Ok <|
                                        BIT B.OneIndex
                                            (RegArg8 CPU.D)

                                -- 0x4B
                                B.HexByte B.H4 B.HB ->
                                    -- BIT 1 E
                                    Ok <|
                                        BIT B.OneIndex
                                            (RegArg8 CPU.E)

                                -- 0x4C
                                B.HexByte B.H4 B.HC ->
                                    -- BIT 1 H
                                    Ok <|
                                        BIT B.OneIndex
                                            (RegArg8 CPU.H)

                                -- 0x4D
                                B.HexByte B.H4 B.HD ->
                                    -- BIT 1 L
                                    Ok <|
                                        BIT B.OneIndex
                                            (RegArg8 CPU.L)

                                -- 0x4E
                                B.HexByte B.H4 B.HE ->
                                    -- BIT 1 (HL)
                                    Ok <|
                                        BIT B.OneIndex
                                            (Address16 CPU.HL)

                                -- 0x4F
                                B.HexByte B.H4 B.HF ->
                                    -- BIT 1 A
                                    Ok <|
                                        BIT B.OneIndex
                                            (RegArg8 CPU.A)

                                -- 0x50
                                B.HexByte B.H5 B.H0 ->
                                    -- BIT 2 B
                                    Ok <| BIT B.TwoIndex (RegArg8 CPU.B)

                                -- 0x51
                                B.HexByte B.H5 B.H1 ->
                                    -- BIT 2 C
                                    Ok <|
                                        BIT B.TwoIndex
                                            (RegArg8 CPU.C)

                                -- 0x52
                                B.HexByte B.H5 B.H2 ->
                                    -- BIT 2 D
                                    Ok <|
                                        BIT B.TwoIndex
                                            (RegArg8 CPU.D)

                                -- 0x53
                                B.HexByte B.H5 B.H3 ->
                                    -- BIT 2 E
                                    Ok <|
                                        BIT B.TwoIndex
                                            (RegArg8 CPU.E)

                                -- 0x54
                                B.HexByte B.H5 B.H4 ->
                                    -- BIT 2 H
                                    Ok <|
                                        BIT B.TwoIndex
                                            (RegArg8 CPU.H)

                                -- 0x55
                                B.HexByte B.H5 B.H5 ->
                                    -- BIT 2 L
                                    Ok <|
                                        BIT B.TwoIndex
                                            (RegArg8 CPU.L)

                                -- 0x56
                                B.HexByte B.H5 B.H6 ->
                                    -- BIT 2 (HL)
                                    Ok <| BIT B.TwoIndex (Address16 CPU.HL)

                                -- 0x57
                                B.HexByte B.H5 B.H7 ->
                                    -- BIT 2 A
                                    Ok <|
                                        BIT B.TwoIndex
                                            (RegArg8 CPU.A)

                                -- 0x58
                                B.HexByte B.H5 B.H8 ->
                                    -- BIT 3 B
                                    Ok <|
                                        BIT B.ThreeIndex
                                            (RegArg8 CPU.B)

                                -- 0x59
                                B.HexByte B.H5 B.H9 ->
                                    -- BIT 3 C
                                    Ok <|
                                        BIT B.ThreeIndex
                                            (RegArg8 CPU.C)

                                -- 0x5A
                                B.HexByte B.H5 B.HA ->
                                    -- BIT 3 D
                                    Ok <|
                                        BIT B.ThreeIndex
                                            (RegArg8 CPU.D)

                                -- 0x5B
                                B.HexByte B.H5 B.HB ->
                                    -- BIT 3 E
                                    Ok <|
                                        BIT B.ThreeIndex
                                            (RegArg8 CPU.E)

                                -- 0x5C
                                B.HexByte B.H5 B.HC ->
                                    -- BIT 3 H
                                    Ok <|
                                        BIT B.ThreeIndex
                                            (RegArg8 CPU.H)

                                -- 0x5D
                                B.HexByte B.H5 B.HD ->
                                    -- BIT 3 L
                                    Ok <|
                                        BIT B.ThreeIndex
                                            (RegArg8 CPU.L)

                                -- 0x5E
                                B.HexByte B.H5 B.HE ->
                                    -- BIT 3 (HL)
                                    Ok <|
                                        BIT B.ThreeIndex
                                            (Address16 CPU.HL)

                                -- 0x5F
                                B.HexByte B.H5 B.HF ->
                                    -- BIT 3 A
                                    Ok <|
                                        BIT B.ThreeIndex
                                            (RegArg8 CPU.A)

                                -- 0x60
                                B.HexByte B.H6 B.H0 ->
                                    -- BIT 4 B
                                    Ok <| BIT B.FourIndex (RegArg8 CPU.B)

                                -- 0x61
                                B.HexByte B.H6 B.H1 ->
                                    -- BIT 4 C
                                    Ok <|
                                        BIT B.FourIndex
                                            (RegArg8 CPU.C)

                                -- 0x62
                                B.HexByte B.H6 B.H2 ->
                                    -- BIT 4 D
                                    Ok <|
                                        BIT B.FourIndex
                                            (RegArg8 CPU.D)

                                -- 0x63
                                B.HexByte B.H6 B.H3 ->
                                    -- BIT 4 E
                                    Ok <|
                                        BIT B.FourIndex
                                            (RegArg8 CPU.E)

                                -- 0x64
                                B.HexByte B.H6 B.H4 ->
                                    -- BIT 4 H
                                    Ok <|
                                        BIT B.FourIndex
                                            (RegArg8 CPU.H)

                                -- 0x65
                                B.HexByte B.H6 B.H5 ->
                                    -- BIT 4 L
                                    Ok <|
                                        BIT B.FourIndex
                                            (RegArg8 CPU.L)

                                -- 0x66
                                B.HexByte B.H6 B.H6 ->
                                    -- BIT 4 (HL)
                                    Ok <| BIT B.FourIndex (Address16 CPU.HL)

                                -- 0x67
                                B.HexByte B.H6 B.H7 ->
                                    -- BIT 4 A
                                    Ok <|
                                        BIT B.FourIndex
                                            (RegArg8 CPU.A)

                                -- 0x68
                                B.HexByte B.H6 B.H8 ->
                                    -- BIT 5 B
                                    Ok <|
                                        BIT B.FiveIndex
                                            (RegArg8 CPU.B)

                                -- 0x69
                                B.HexByte B.H6 B.H9 ->
                                    -- BIT 5 C
                                    Ok <|
                                        BIT B.FiveIndex
                                            (RegArg8 CPU.C)

                                -- 0x6A
                                B.HexByte B.H6 B.HA ->
                                    -- BIT 5 D
                                    Ok <|
                                        BIT B.FiveIndex
                                            (RegArg8 CPU.D)

                                -- 0x6B
                                B.HexByte B.H6 B.HB ->
                                    -- BIT 5 E
                                    Ok <|
                                        BIT B.FiveIndex
                                            (RegArg8 CPU.E)

                                -- 0x6C
                                B.HexByte B.H6 B.HC ->
                                    -- BIT 5 H
                                    Ok <|
                                        BIT B.FiveIndex
                                            (RegArg8 CPU.H)

                                -- 0x6D
                                B.HexByte B.H6 B.HD ->
                                    -- BIT 5 L
                                    Ok <|
                                        BIT B.FiveIndex
                                            (RegArg8 CPU.L)

                                -- 0x6E
                                B.HexByte B.H6 B.HE ->
                                    -- BIT 5 (HL)
                                    Ok <|
                                        BIT B.FiveIndex
                                            (Address16 CPU.HL)

                                -- 0x6F
                                B.HexByte B.H6 B.HF ->
                                    -- BIT 5 A
                                    Ok <|
                                        BIT B.FiveIndex
                                            (RegArg8 CPU.A)

                                -- 0x70
                                B.HexByte B.H7 B.H0 ->
                                    -- BIT 6 B
                                    Ok <| BIT B.SixIndex (RegArg8 CPU.B)

                                -- 0x71
                                B.HexByte B.H7 B.H1 ->
                                    -- BIT 6 C
                                    Ok <|
                                        BIT B.SixIndex
                                            (RegArg8 CPU.C)

                                -- 0x72
                                B.HexByte B.H7 B.H2 ->
                                    -- BIT 6 D
                                    Ok <|
                                        BIT B.SixIndex
                                            (RegArg8 CPU.D)

                                -- 0x73
                                B.HexByte B.H7 B.H3 ->
                                    -- BIT 6 E
                                    Ok <|
                                        BIT B.SixIndex
                                            (RegArg8 CPU.E)

                                -- 0x74
                                B.HexByte B.H7 B.H4 ->
                                    -- BIT 6 H
                                    Ok <|
                                        BIT B.SixIndex
                                            (RegArg8 CPU.H)

                                -- 0x75
                                B.HexByte B.H7 B.H5 ->
                                    -- BIT 6 L
                                    Ok <|
                                        BIT B.SixIndex
                                            (RegArg8 CPU.L)

                                -- 0x76
                                B.HexByte B.H7 B.H6 ->
                                    -- BIT 6 (HL)
                                    Ok <| BIT B.SixIndex (Address16 CPU.HL)

                                -- 0x77
                                B.HexByte B.H7 B.H7 ->
                                    -- BIT 6 A
                                    Ok <|
                                        BIT B.SixIndex
                                            (RegArg8 CPU.A)

                                -- 0x78
                                B.HexByte B.H7 B.H8 ->
                                    -- BIT 7 B
                                    Ok <|
                                        BIT B.SevenIndex
                                            (RegArg8 CPU.B)

                                -- 0x79
                                B.HexByte B.H7 B.H9 ->
                                    -- BIT 7 C
                                    Ok <|
                                        BIT B.SevenIndex
                                            (RegArg8 CPU.C)

                                -- 0x7A
                                B.HexByte B.H7 B.HA ->
                                    -- BIT 7 D
                                    Ok <|
                                        BIT B.SevenIndex
                                            (RegArg8 CPU.D)

                                -- 0x7B
                                B.HexByte B.H7 B.HB ->
                                    -- BIT 7 E
                                    Ok <|
                                        BIT B.SevenIndex
                                            (RegArg8 CPU.E)

                                -- 0x7C
                                B.HexByte B.H7 B.HC ->
                                    -- BIT 7 H
                                    Ok <|
                                        BIT B.SevenIndex
                                            (RegArg8 CPU.H)

                                -- 0x7D
                                B.HexByte B.H7 B.HD ->
                                    -- BIT 7 L
                                    Ok <|
                                        BIT B.SevenIndex
                                            (RegArg8 CPU.L)

                                -- 0x7E
                                B.HexByte B.H7 B.HE ->
                                    -- BIT 7 (HL)
                                    Ok <|
                                        BIT B.SevenIndex
                                            (Address16 CPU.HL)

                                -- 0x7F
                                B.HexByte B.H7 B.HF ->
                                    -- BIT 7 A
                                    Ok <|
                                        BIT B.SevenIndex
                                            (RegArg8 CPU.A)

                                ---- RES ----
                                -- 0x80
                                B.HexByte B.H8 B.H0 ->
                                    -- RES 0 B
                                    Ok <| RES B.ZeroIndex (RegArg8 CPU.B)

                                -- 0x81
                                B.HexByte B.H8 B.H1 ->
                                    -- RES 0 C
                                    Ok <|
                                        RES B.ZeroIndex
                                            (RegArg8 CPU.C)

                                -- 0x82
                                B.HexByte B.H8 B.H2 ->
                                    -- RES 0 D
                                    Ok <|
                                        RES B.ZeroIndex
                                            (RegArg8 CPU.D)

                                -- 0x83
                                B.HexByte B.H8 B.H3 ->
                                    -- RES 0 E
                                    Ok <|
                                        RES B.ZeroIndex
                                            (RegArg8 CPU.E)

                                -- 0x84
                                B.HexByte B.H8 B.H4 ->
                                    -- RES 0 H
                                    Ok <|
                                        RES B.ZeroIndex
                                            (RegArg8 CPU.H)

                                -- 0x85
                                B.HexByte B.H8 B.H5 ->
                                    -- RES 0 L
                                    Ok <|
                                        RES B.ZeroIndex
                                            (RegArg8 CPU.L)

                                -- 0x86
                                B.HexByte B.H8 B.H6 ->
                                    -- RES 0 (HL)
                                    Ok <| RES B.ZeroIndex (Address16 CPU.HL)

                                -- 0x87
                                B.HexByte B.H8 B.H7 ->
                                    -- RES 0 A
                                    Ok <|
                                        RES B.ZeroIndex
                                            (RegArg8 CPU.A)

                                -- 0x88
                                B.HexByte B.H8 B.H8 ->
                                    -- RES 1 B
                                    Ok <|
                                        RES B.OneIndex
                                            (RegArg8 CPU.B)

                                -- 0x89
                                B.HexByte B.H8 B.H9 ->
                                    -- RES 1 C
                                    Ok <|
                                        RES B.OneIndex
                                            (RegArg8 CPU.C)

                                -- 0x8A
                                B.HexByte B.H8 B.HA ->
                                    -- RES 1 D
                                    Ok <|
                                        RES B.OneIndex
                                            (RegArg8 CPU.D)

                                -- 0x8B
                                B.HexByte B.H8 B.HB ->
                                    -- RES 1 E
                                    Ok <|
                                        RES B.OneIndex
                                            (RegArg8 CPU.E)

                                -- 0x8C
                                B.HexByte B.H8 B.HC ->
                                    -- RES 1 H
                                    Ok <|
                                        RES B.OneIndex
                                            (RegArg8 CPU.H)

                                -- 0x8D
                                B.HexByte B.H8 B.HD ->
                                    -- RES 1 L
                                    Ok <|
                                        RES B.OneIndex
                                            (RegArg8 CPU.L)

                                -- 0x8E
                                B.HexByte B.H8 B.HE ->
                                    -- RES 1 (HL)
                                    Ok <|
                                        RES B.OneIndex
                                            (Address16 CPU.HL)

                                -- 0x8F
                                B.HexByte B.H8 B.HF ->
                                    -- RES 1 A
                                    Ok <|
                                        RES B.OneIndex
                                            (RegArg8 CPU.A)

                                -- 0x90
                                B.HexByte B.H9 B.H0 ->
                                    -- RES 2 B
                                    Ok <| RES B.TwoIndex (RegArg8 CPU.B)

                                -- 0x91
                                B.HexByte B.H9 B.H1 ->
                                    -- RES 2 C
                                    Ok <|
                                        RES B.TwoIndex
                                            (RegArg8 CPU.C)

                                -- 0x92
                                B.HexByte B.H9 B.H2 ->
                                    -- RES 2 D
                                    Ok <|
                                        RES B.TwoIndex
                                            (RegArg8 CPU.D)

                                -- 0x93
                                B.HexByte B.H9 B.H3 ->
                                    -- RES 2 E
                                    Ok <|
                                        RES B.TwoIndex
                                            (RegArg8 CPU.E)

                                -- 0x94
                                B.HexByte B.H9 B.H4 ->
                                    -- RES 2 H
                                    Ok <|
                                        RES B.TwoIndex
                                            (RegArg8 CPU.H)

                                -- 0x95
                                B.HexByte B.H9 B.H5 ->
                                    -- RES 2 L
                                    Ok <|
                                        RES B.TwoIndex
                                            (RegArg8 CPU.L)

                                -- 0x96
                                B.HexByte B.H9 B.H6 ->
                                    -- RES 2 (HL)
                                    Ok <| RES B.TwoIndex (Address16 CPU.HL)

                                -- 0x97
                                B.HexByte B.H9 B.H7 ->
                                    -- RES 2 A
                                    Ok <|
                                        RES B.TwoIndex
                                            (RegArg8 CPU.A)

                                -- 0x98
                                B.HexByte B.H9 B.H8 ->
                                    -- RES 3 B
                                    Ok <|
                                        RES B.ThreeIndex
                                            (RegArg8 CPU.B)

                                -- 0x99
                                B.HexByte B.H9 B.H9 ->
                                    -- RES 3 C
                                    Ok <|
                                        RES B.ThreeIndex
                                            (RegArg8 CPU.C)

                                -- 0x9A
                                B.HexByte B.H9 B.HA ->
                                    -- RES 3 D
                                    Ok <|
                                        RES B.ThreeIndex
                                            (RegArg8 CPU.D)

                                -- 0x9B
                                B.HexByte B.H9 B.HB ->
                                    -- RES 3 E
                                    Ok <|
                                        RES B.ThreeIndex
                                            (RegArg8 CPU.E)

                                -- 0x9C
                                B.HexByte B.H9 B.HC ->
                                    -- RES 3 H
                                    Ok <|
                                        RES B.ThreeIndex
                                            (RegArg8 CPU.H)

                                -- 0x9D
                                B.HexByte B.H9 B.HD ->
                                    -- RES 3 L
                                    Ok <|
                                        RES B.ThreeIndex
                                            (RegArg8 CPU.L)

                                -- 0x9E
                                B.HexByte B.H9 B.HE ->
                                    -- RES 3 (HL)
                                    Ok <|
                                        RES B.ThreeIndex
                                            (Address16 CPU.HL)

                                -- 0x9F
                                B.HexByte B.H9 B.HF ->
                                    -- RES 3 A
                                    Ok <|
                                        RES B.ThreeIndex
                                            (RegArg8 CPU.A)

                                -- 0xA0
                                B.HexByte B.HA B.H0 ->
                                    -- RES 4 B
                                    Ok <| RES B.FourIndex (RegArg8 CPU.B)

                                -- 0xA1
                                B.HexByte B.HA B.H1 ->
                                    -- RES 4 C
                                    Ok <|
                                        RES B.FourIndex
                                            (RegArg8 CPU.C)

                                -- 0xA2
                                B.HexByte B.HA B.H2 ->
                                    -- RES 4 D
                                    Ok <|
                                        RES B.FourIndex
                                            (RegArg8 CPU.D)

                                -- 0xA3
                                B.HexByte B.HA B.H3 ->
                                    -- RES 4 E
                                    Ok <|
                                        RES B.FourIndex
                                            (RegArg8 CPU.E)

                                -- 0xA4
                                B.HexByte B.HA B.H4 ->
                                    -- RES 4 H
                                    Ok <|
                                        RES B.FourIndex
                                            (RegArg8 CPU.H)

                                -- 0xA5
                                B.HexByte B.HA B.H5 ->
                                    -- RES 4 L
                                    Ok <|
                                        RES B.FourIndex
                                            (RegArg8 CPU.L)

                                -- 0xA6
                                B.HexByte B.HA B.H6 ->
                                    -- RES 4 (HL)
                                    Ok <| RES B.FourIndex (Address16 CPU.HL)

                                -- 0xA7
                                B.HexByte B.HA B.H7 ->
                                    -- RES 4 A
                                    Ok <|
                                        RES B.FourIndex
                                            (RegArg8 CPU.A)

                                -- 0xA8
                                B.HexByte B.HA B.H8 ->
                                    -- RES 5 B
                                    Ok <|
                                        RES B.FiveIndex
                                            (RegArg8 CPU.B)

                                -- 0xA9
                                B.HexByte B.HA B.H9 ->
                                    -- RES 5 C
                                    Ok <|
                                        RES B.FiveIndex
                                            (RegArg8 CPU.C)

                                -- 0xAA
                                B.HexByte B.HA B.HA ->
                                    -- RES 5 D
                                    Ok <|
                                        RES B.FiveIndex
                                            (RegArg8 CPU.D)

                                -- 0xAB
                                B.HexByte B.HA B.HB ->
                                    -- RES 5 E
                                    Ok <|
                                        RES B.FiveIndex
                                            (RegArg8 CPU.E)

                                -- 0xAC
                                B.HexByte B.HA B.HC ->
                                    -- RES 5 H
                                    Ok <|
                                        RES B.FiveIndex
                                            (RegArg8 CPU.H)

                                -- 0xAD
                                B.HexByte B.HA B.HD ->
                                    -- RES 5 L
                                    Ok <|
                                        RES B.FiveIndex
                                            (RegArg8 CPU.L)

                                -- 0xAE
                                B.HexByte B.HA B.HE ->
                                    -- RES 5 (HL)
                                    Ok <|
                                        RES B.FiveIndex
                                            (Address16 CPU.HL)

                                -- 0xAF
                                B.HexByte B.HA B.HF ->
                                    -- RES 5 A
                                    Ok <|
                                        RES B.FiveIndex
                                            (RegArg8 CPU.A)

                                -- 0xB0
                                B.HexByte B.HB B.H0 ->
                                    -- RES 6 B
                                    Ok <| RES B.SixIndex (RegArg8 CPU.B)

                                -- 0xB1
                                B.HexByte B.HB B.H1 ->
                                    -- RES 6 C
                                    Ok <|
                                        RES B.SixIndex
                                            (RegArg8 CPU.C)

                                -- 0xB2
                                B.HexByte B.HB B.H2 ->
                                    -- RES 6 D
                                    Ok <|
                                        RES B.SixIndex
                                            (RegArg8 CPU.D)

                                -- 0xB3
                                B.HexByte B.HB B.H3 ->
                                    -- RES 6 E
                                    Ok <|
                                        RES B.SixIndex
                                            (RegArg8 CPU.E)

                                -- 0xB4
                                B.HexByte B.HB B.H4 ->
                                    -- RES 6 H
                                    Ok <|
                                        RES B.SixIndex
                                            (RegArg8 CPU.H)

                                -- 0xB5
                                B.HexByte B.HB B.H5 ->
                                    -- RES 6 L
                                    Ok <|
                                        RES B.SixIndex
                                            (RegArg8 CPU.L)

                                -- 0xB6
                                B.HexByte B.HB B.H6 ->
                                    -- RES 6 (HL)
                                    Ok <| RES B.SixIndex (Address16 CPU.HL)

                                -- 0xB7
                                B.HexByte B.HB B.H7 ->
                                    -- RES 6 A
                                    Ok <|
                                        RES B.SixIndex
                                            (RegArg8 CPU.A)

                                -- 0xB8
                                B.HexByte B.HB B.H8 ->
                                    -- RES 7 B
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.B)

                                -- 0xB9
                                B.HexByte B.HB B.H9 ->
                                    -- RES 7 C
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.C)

                                -- 0xBA
                                B.HexByte B.HB B.HA ->
                                    -- RES 7 D
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.D)

                                -- 0xBB
                                B.HexByte B.HB B.HB ->
                                    -- RES 7 E
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.E)

                                -- 0xBC
                                B.HexByte B.HB B.HC ->
                                    -- RES 7 H
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.H)

                                -- 0xBD
                                B.HexByte B.HB B.HD ->
                                    -- RES 7 L
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.L)

                                -- 0xBE
                                B.HexByte B.HB B.HE ->
                                    -- RES 7 (HL)
                                    Ok <|
                                        RES B.SevenIndex
                                            (Address16 CPU.HL)

                                -- 0xBF
                                B.HexByte B.HB B.HF ->
                                    -- RES 7 A
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.A)

                                ---- SET ----
                                -- 0xC0
                                B.HexByte B.HC B.H0 ->
                                    -- SET 0 B
                                    Ok <| SET B.ZeroIndex (RegArg8 CPU.B)

                                -- 0xC1
                                B.HexByte B.HC B.H1 ->
                                    -- SET 0 C
                                    Ok <|
                                        SET B.ZeroIndex
                                            (RegArg8 CPU.C)

                                -- 0xC2
                                B.HexByte B.HC B.H2 ->
                                    -- SET 0 D
                                    Ok <|
                                        SET B.ZeroIndex
                                            (RegArg8 CPU.D)

                                -- 0xC3
                                B.HexByte B.HC B.H3 ->
                                    -- SET 0 E
                                    Ok <|
                                        SET B.ZeroIndex
                                            (RegArg8 CPU.E)

                                -- 0xC4
                                B.HexByte B.HC B.H4 ->
                                    -- SET 0 H
                                    Ok <|
                                        SET B.ZeroIndex
                                            (RegArg8 CPU.H)

                                -- 0xC5
                                B.HexByte B.HC B.H5 ->
                                    -- SET 0 L
                                    Ok <|
                                        SET B.ZeroIndex
                                            (RegArg8 CPU.L)

                                -- 0xC6
                                B.HexByte B.HC B.H6 ->
                                    -- SET 0 (HL)
                                    Ok <| SET B.ZeroIndex (Address16 CPU.HL)

                                -- 0xC7
                                B.HexByte B.HC B.H7 ->
                                    -- SET 0 A
                                    Ok <|
                                        SET B.ZeroIndex
                                            (RegArg8 CPU.A)

                                -- 0xC8
                                B.HexByte B.HC B.H8 ->
                                    -- SET 1 B
                                    Ok <|
                                        SET B.OneIndex
                                            (RegArg8 CPU.B)

                                -- 0xC9
                                B.HexByte B.HC B.H9 ->
                                    -- SET 1 C
                                    Ok <|
                                        SET B.OneIndex
                                            (RegArg8 CPU.C)

                                -- 0xCA
                                B.HexByte B.HC B.HA ->
                                    -- SET 1 D
                                    Ok <|
                                        SET B.OneIndex
                                            (RegArg8 CPU.D)

                                -- 0xCB
                                B.HexByte B.HC B.HB ->
                                    -- SET 1 E
                                    Ok <|
                                        SET B.OneIndex
                                            (RegArg8 CPU.E)

                                -- 0xCC
                                B.HexByte B.HC B.HC ->
                                    -- SET 1 H
                                    Ok <|
                                        SET B.OneIndex
                                            (RegArg8 CPU.H)

                                -- 0xCD
                                B.HexByte B.HC B.HD ->
                                    -- SET 1 L
                                    Ok <|
                                        SET B.OneIndex
                                            (RegArg8 CPU.L)

                                -- 0xCE
                                B.HexByte B.HC B.HE ->
                                    -- SET 1 (HL)
                                    Ok <|
                                        SET B.OneIndex
                                            (Address16 CPU.HL)

                                -- 0xCF
                                B.HexByte B.HC B.HF ->
                                    -- SET 1 A
                                    Ok <|
                                        SET B.OneIndex
                                            (RegArg8 CPU.A)

                                -- 0xD0
                                B.HexByte B.HD B.H0 ->
                                    -- SET 2 B
                                    Ok <| SET B.TwoIndex (RegArg8 CPU.B)

                                -- 0xD1
                                B.HexByte B.HD B.H1 ->
                                    -- SET 2 C
                                    Ok <|
                                        SET B.TwoIndex
                                            (RegArg8 CPU.C)

                                -- 0xD2
                                B.HexByte B.HD B.H2 ->
                                    -- SET 2 D
                                    Ok <|
                                        SET B.TwoIndex
                                            (RegArg8 CPU.D)

                                -- 0xD3
                                B.HexByte B.HD B.H3 ->
                                    -- SET 2 E
                                    Ok <|
                                        SET B.TwoIndex
                                            (RegArg8 CPU.E)

                                -- 0xD4
                                B.HexByte B.HD B.H4 ->
                                    -- SET 2 H
                                    Ok <|
                                        SET B.TwoIndex
                                            (RegArg8 CPU.H)

                                -- 0xD5
                                B.HexByte B.HD B.H5 ->
                                    -- SET 2 L
                                    Ok <|
                                        SET B.TwoIndex
                                            (RegArg8 CPU.L)

                                -- 0xD6
                                B.HexByte B.HD B.H6 ->
                                    -- SET 2 (HL)
                                    Ok <| SET B.TwoIndex (Address16 CPU.HL)

                                -- 0xD7
                                B.HexByte B.HD B.H7 ->
                                    -- SET 2 A
                                    Ok <|
                                        SET B.TwoIndex
                                            (RegArg8 CPU.A)

                                -- 0xD8
                                B.HexByte B.HD B.H8 ->
                                    -- SET 3 B
                                    Ok <|
                                        SET B.ThreeIndex
                                            (RegArg8 CPU.B)

                                -- 0xD9
                                B.HexByte B.HD B.H9 ->
                                    -- SET 3 C
                                    Ok <|
                                        SET B.ThreeIndex
                                            (RegArg8 CPU.C)

                                -- 0xDA
                                B.HexByte B.HD B.HA ->
                                    -- SET 3 D
                                    Ok <|
                                        SET B.ThreeIndex
                                            (RegArg8 CPU.D)

                                -- 0xDB
                                B.HexByte B.HD B.HB ->
                                    -- SET 3 E
                                    Ok <|
                                        SET B.ThreeIndex
                                            (RegArg8 CPU.E)

                                -- 0xDC
                                B.HexByte B.HD B.HC ->
                                    -- SET 3 H
                                    Ok <|
                                        SET B.ThreeIndex
                                            (RegArg8 CPU.H)

                                -- 0xDD
                                B.HexByte B.HD B.HD ->
                                    -- SET 3 L
                                    Ok <|
                                        SET B.ThreeIndex
                                            (RegArg8 CPU.L)

                                -- 0xDE
                                B.HexByte B.HD B.HE ->
                                    -- SET 3 (HL)
                                    Ok <|
                                        SET B.ThreeIndex
                                            (Address16 CPU.HL)

                                -- 0xDF
                                B.HexByte B.HD B.HF ->
                                    -- SET 3 A
                                    Ok <|
                                        SET B.ThreeIndex
                                            (RegArg8 CPU.A)

                                -- 0xE0
                                B.HexByte B.HE B.H0 ->
                                    -- SET 4 B
                                    Ok <| SET B.FourIndex (RegArg8 CPU.B)

                                -- 0xE1
                                B.HexByte B.HE B.H1 ->
                                    -- SET 4 C
                                    Ok <|
                                        SET B.FourIndex
                                            (RegArg8 CPU.C)

                                -- 0xE2
                                B.HexByte B.HE B.H2 ->
                                    -- SET 4 D
                                    Ok <|
                                        SET B.FourIndex
                                            (RegArg8 CPU.D)

                                -- 0xE3
                                B.HexByte B.HE B.H3 ->
                                    -- SET 4 E
                                    Ok <|
                                        SET B.FourIndex
                                            (RegArg8 CPU.E)

                                -- 0xE4
                                B.HexByte B.HE B.H4 ->
                                    -- SET 4 H
                                    Ok <|
                                        SET B.FourIndex
                                            (RegArg8 CPU.H)

                                -- 0xE5
                                B.HexByte B.HE B.H5 ->
                                    -- SET 4 L
                                    Ok <|
                                        SET B.FourIndex
                                            (RegArg8 CPU.L)

                                -- 0xE6
                                B.HexByte B.HE B.H6 ->
                                    -- SET 4 (HL)
                                    Ok <| SET B.FourIndex (Address16 CPU.HL)

                                -- 0xE7
                                B.HexByte B.HE B.H7 ->
                                    -- SET 4 A
                                    Ok <|
                                        SET B.FourIndex
                                            (RegArg8 CPU.A)

                                -- 0xE8
                                B.HexByte B.HE B.H8 ->
                                    -- SET 5 B
                                    Ok <|
                                        SET B.FiveIndex
                                            (RegArg8 CPU.B)

                                -- 0xE9
                                B.HexByte B.HE B.H9 ->
                                    -- SET 5 C
                                    Ok <|
                                        SET B.FiveIndex
                                            (RegArg8 CPU.C)

                                -- 0xEA
                                B.HexByte B.HE B.HA ->
                                    -- SET 5 D
                                    Ok <|
                                        SET B.FiveIndex
                                            (RegArg8 CPU.D)

                                -- 0xEB
                                B.HexByte B.HE B.HB ->
                                    -- SET 5 E
                                    Ok <|
                                        SET B.FiveIndex
                                            (RegArg8 CPU.E)

                                -- 0xEC
                                B.HexByte B.HE B.HC ->
                                    -- SET 5 H
                                    Ok <|
                                        SET B.FiveIndex
                                            (RegArg8 CPU.H)

                                -- 0xED
                                B.HexByte B.HE B.HD ->
                                    -- SET 5 L
                                    Ok <|
                                        SET B.FiveIndex
                                            (RegArg8 CPU.L)

                                -- 0xEE
                                B.HexByte B.HE B.HE ->
                                    -- SET 5 (HL)
                                    Ok <|
                                        SET B.FiveIndex
                                            (Address16 CPU.HL)

                                -- 0xEF
                                B.HexByte B.HE B.HF ->
                                    -- SET 5 A
                                    Ok <|
                                        SET B.FiveIndex
                                            (RegArg8 CPU.A)

                                -- 0xF0
                                B.HexByte B.HF B.H0 ->
                                    -- SET 6 B
                                    Ok <| SET B.SixIndex (RegArg8 CPU.B)

                                -- 0xF1
                                B.HexByte B.HF B.H1 ->
                                    -- SET 6 C
                                    Ok <|
                                        SET B.SixIndex
                                            (RegArg8 CPU.C)

                                -- 0xF2
                                B.HexByte B.HF B.H2 ->
                                    -- SET 6 D
                                    Ok <|
                                        SET B.SixIndex
                                            (RegArg8 CPU.D)

                                -- 0xF3
                                B.HexByte B.HF B.H3 ->
                                    -- SET 6 E
                                    Ok <|
                                        SET B.SixIndex
                                            (RegArg8 CPU.E)

                                -- 0xF4
                                B.HexByte B.HF B.H4 ->
                                    -- SET 6 H
                                    Ok <|
                                        SET B.SixIndex
                                            (RegArg8 CPU.H)

                                -- 0xF5
                                B.HexByte B.HF B.H5 ->
                                    -- SET 6 L
                                    Ok <|
                                        SET B.SixIndex
                                            (RegArg8 CPU.L)

                                -- 0xF6
                                B.HexByte B.HF B.H6 ->
                                    -- RES 6 (HL)
                                    Ok <| RES B.SixIndex (Address16 CPU.HL)

                                -- 0xF7
                                B.HexByte B.HF B.H7 ->
                                    -- RES 6 A
                                    Ok <|
                                        RES B.SixIndex
                                            (RegArg8 CPU.A)

                                -- 0xF8
                                B.HexByte B.HF B.H8 ->
                                    -- RES 7 B
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.B)

                                -- 0xF9
                                B.HexByte B.HF B.H9 ->
                                    -- RES 7 C
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.C)

                                -- 0xFA
                                B.HexByte B.HF B.HA ->
                                    -- RES 7 D
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.D)

                                -- 0xFB
                                B.HexByte B.HF B.HB ->
                                    -- RES 7 E
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.E)

                                -- 0xFC
                                B.HexByte B.HF B.HC ->
                                    -- RES 7 H
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.H)

                                -- 0xFD
                                B.HexByte B.HF B.HD ->
                                    -- RES 7 L
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.L)

                                -- 0xFE
                                B.HexByte B.HF B.HE ->
                                    -- RES 7 (HL)
                                    Ok <|
                                        RES B.SevenIndex
                                            (Address16 CPU.HL)

                                -- 0xFF
                                B.HexByte B.HF B.HF ->
                                    -- RES 7 A
                                    Ok <|
                                        RES B.SevenIndex
                                            (RegArg8 CPU.A)

                ---- LD ----
                -- 0x01
                B.HexByte B.H0 B.H1 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- LD BC, d16
                            Ok <| LDSixteenBitValue (RegArg16 CPU.BC) b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "LD BC, d16 requires two extra bytes" }

                -- 0x11
                B.HexByte B.H1 B.H1 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- LD DE, d16
                            Ok <| LDSixteenBitValue (RegArg16 CPU.DE) b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "LD DE, d16 requires two extra bytes" }

                -- 0x21
                B.HexByte B.H2 B.H1 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- LD HL, d16
                            Ok <| LDSixteenBitValue (RegArg16 CPU.HL) b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "LD HL, d16 requires two extra bytes" }

                -- 0x31
                B.HexByte B.H3 B.H1 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- LD SP, d16
                            Ok <| LDSixteenBitValue (RegArg16 CPU.SP) b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "LD SP, d16 requires two extra bytes" }

                -- 0x08
                B.HexByte B.H0 B.H8 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- LD (a16), SP
                            Ok <| LDHRegisterToImmediateAddress (ImmediateAddress16 b b2) (RegArg16 CPU.SP)

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "LD (a16), SP requires two extra bytes" }

                -- 0xEA
                B.HexByte B.HE B.HA ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- LD (a16), A
                            Ok <| LDHRegisterToImmediateAddress (ImmediateAddress16 b b2) (RegArg8 CPU.A)

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "LD (a16), A requires two extra bytes" }

                -- 0xFA
                B.HexByte B.HF B.HA ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- LD A, (a16)
                            Ok <| LDHImmediateAddressToRegister (RegArg8 CPU.A) (ImmediateAddress16 b b2)

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "LD A, (a16) requires two extra bytes" }

                ---- JP ----
                -- 0xC2
                B.HexByte B.HC B.H2 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- JP NZ, a16
                            Ok <| JPFlag CPU.NonZeroFlag b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "JP NZ, a16 requires two extra bytes" }

                -- 0xD3
                B.HexByte B.HD B.H2 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- JP NC, a16
                            Ok <| JPFlag CPU.NonCarryFlag b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "JP NC, a16 requires two extra bytes" }

                -- 0xC3
                B.HexByte B.HC B.H3 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- JP a16
                            Ok <| JP b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "JP a16 requires two extra bytes" }

                -- 0xCA
                B.HexByte B.HC B.HA ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- JP Z, a16
                            Ok <| JPFlag CPU.ZeroFlag b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "JP Z, a16 requires two extra bytes" }

                -- 0xDA
                B.HexByte B.HD B.HA ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- JP C, a16
                            Ok <| JPFlag CPU.CarryFlag b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "JP C, a16 requires two extra bytes" }

                ---- JR ----
                -- 0x18
                B.HexByte B.H1 B.H8 ->
                    case byte2 of
                        Just b ->
                            -- JR r8
                            Ok <| JR b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "JR r8 requires a second byte" }

                -- 0x28
                B.HexByte B.H2 B.H8 ->
                    case byte2 of
                        Just b ->
                            -- JR Z, r8
                            Ok <| JRFlag CPU.ZeroFlag b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "JR Z, r8 requires a second byte" }

                -- 0x38
                B.HexByte B.H3 B.H8 ->
                    case byte2 of
                        Just b ->
                            -- JR C, r8
                            Ok <| JRFlag CPU.CarryFlag b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "JR C, r8 requires a second byte" }

                -- 0x20
                B.HexByte B.H2 B.H0 ->
                    case byte2 of
                        Just b ->
                            -- JR NZ, r8
                            Ok <| JRFlag CPU.NonZeroFlag b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "JR NZ, r8 requires a second byte" }

                -- 0x30
                B.HexByte B.H3 B.H0 ->
                    case byte2 of
                        Just b ->
                            -- JR NC, r8
                            Ok <| JRFlag CPU.NonCarryFlag b

                        Nothing ->
                            Err <| DecodeError { opCode = byte, message = "JR NC, r8 requires a second byte" }

                ---- CALL ----
                -- 0xC4
                B.HexByte B.HC B.H4 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- CALL NZ, a16
                            Ok <| CALLFlag CPU.NonZeroFlag b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "CALL NZ, a16 requires two extra bytes" }

                -- 0xD4
                B.HexByte B.HD B.H4 ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- CALL NC, a16
                            Ok <| CALLFlag CPU.NonCarryFlag b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "CALL NC, a16 requires two extra bytes" }

                -- 0xCC
                B.HexByte B.HC B.HC ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- CALL Z, a16
                            Ok <| CALLFlag CPU.ZeroFlag b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "CALL Z, a16 requires two extra bytes" }

                -- 0xDC
                B.HexByte B.HD B.HC ->
                    case ( byte2, byte3 ) of
                        ( Just b, Just b2 ) ->
                            -- CALL C, a16
                            Ok <| CALLFlag CPU.CarryFlag b b2

                        _ ->
                            Err <| DecodeError { opCode = byte, message = "CALL C, a16 requires two extra bytes" }
