module CPU exposing (..)

import Binary
    exposing
        ( Bit(..)
        , BitIndex(..)
        , Byte
        , Word
        , zeroByte
        )
import MMU exposing (..)


type EightBitRegisterName
    = A -- the accumulator register
    | B -- registers B thru L are general purpose
    | C
    | D
    | E
    | H
    | L
    | F -- status register


type SixteenBitRegisterName
    = SP -- stack pointer
    | PC -- program counter
    | BC -- BC thru HL are "virtual" 16-bit registers using two 8-bit registers
    | DE
    | HL
    | AF


type RegisterArgument
    = RegArg8 EightBitRegisterName
    | RegArg16 SixteenBitRegisterName
    | Address8 EightBitRegisterName
    | Address16 SixteenBitRegisterName


type ImmediateAddress
    = ImmediateAddress8 Byte
    | ImmediateAddress16 Byte Byte


type alias RegisterState =
    { a : Byte
    , b : Byte
    , c : Byte
    , d : Byte
    , e : Byte
    , h : Byte
    , l : Byte
    , f : Byte
    , sp : ( Byte, Byte )
    , pc : ( Byte, Byte )
    }


setEightBitRegister : EightBitRegisterName -> Byte -> RegisterState -> RegisterState
setEightBitRegister eightBitRegister byte registerState =
    case eightBitRegister of
        A ->
            { registerState | a = byte }

        B ->
            { registerState | b = byte }

        C ->
            { registerState | c = byte }

        D ->
            { registerState | d = byte }

        E ->
            { registerState | e = byte }

        H ->
            { registerState | h = byte }

        L ->
            { registerState | l = byte }

        F ->
            -- TODO: Mask away right 4 bits as they are never written
            { registerState | f = byte }


setSixteenbitRegister :
    SixteenBitRegisterName
    -> ( Byte, Byte )
    -> RegisterState
    -> RegisterState
setSixteenbitRegister sixteenBitRegisterName ( byte1, byte2 ) registerState =
    case sixteenBitRegisterName of
        SP ->
            { registerState | sp = ( byte1, byte2 ) }

        PC ->
            { registerState | pc = ( byte1, byte2 ) }

        BC ->
            { registerState | b = byte1, c = byte2 }

        DE ->
            { registerState | d = byte1, e = byte2 }

        HL ->
            { registerState | h = byte1, l = byte2 }

        AF ->
            { registerState | a = byte1, f = byte2 }


initialRegisterState : RegisterState
initialRegisterState =
    { a = zeroByte
    , b = zeroByte
    , c = zeroByte
    , d = zeroByte
    , e = zeroByte
    , h = zeroByte
    , l = zeroByte
    , f = zeroByte
    , sp = ( zeroByte, zeroByte )
    , pc = ( zeroByte, zeroByte )
    }


type Flag
    = NonZeroFlag
    | ZeroFlag
    | NonCarryFlag
    | CarryFlag


type Instruction
    = NOP
      -- Stop CPU and LCD until button interupt
    | STOP
      -- Load eight bit immediate value into register
    | LDEightBitValue RegisterArgument Byte
      -- Load sixteen bit immediate value into register
    | LDSixteenBitValue RegisterArgument Byte Byte
      -- Load from one register to another
    | LDRegister RegisterArgument RegisterArgument
    | LDHRegisterToImmediateAddress ImmediateAddress RegisterArgument
    | LDHImmediateAddressToRegister RegisterArgument ImmediateAddress
    | LDD RegisterArgument RegisterArgument
    | LDI RegisterArgument RegisterArgument
    | LDHL RegisterArgument Byte
    | PUSH RegisterArgument
    | POP RegisterArgument
    | ADDHL RegisterArgument
    | ADD RegisterArgument
    | ADDEightBitValue RegisterArgument Byte
    | ADC RegisterArgument
    | ADCEightBitValue RegisterArgument Byte
    | SUB RegisterArgument
    | SUBEightBitValue Byte
    | SBC RegisterArgument
    | SBCEightBitValue RegisterArgument Byte
    | AND RegisterArgument
    | ANDEightBitValue Byte
    | OR RegisterArgument
    | OREightBitValue Byte
    | XOR RegisterArgument
    | XOREightBitValue Byte
    | CP RegisterArgument
    | CPEightBitValue Byte
    | INC RegisterArgument
    | DEC RegisterArgument
    | ADDSP Byte
    | SWAP RegisterArgument
    | DAA
    | CPL
    | CCF
    | SCF
    | HALT
    | DI
    | EI
    | RLCA
    | RLA
    | RRCA
    | RRA
    | RLC RegisterArgument
    | RL RegisterArgument
    | RRC RegisterArgument
    | RR RegisterArgument
    | SLA RegisterArgument
    | SRA RegisterArgument
    | SRL RegisterArgument
    | BIT BitIndex RegisterArgument
    | SET BitIndex RegisterArgument
    | RES BitIndex RegisterArgument
    | JP Byte Byte
    | JPFlag Flag Byte Byte
    | JPHL
    | JR Byte
    | JRFlag Flag Byte
    | CALL Byte Byte
    | CALLFlag Flag Byte Byte
    | RST Byte
    | RET
    | RETFlag Flag
    | RETI


type CPUError
    = CPUError { instruction : Instruction, message : String }


execute : Instruction -> MMU -> RegisterState -> Result CPUError ( MMU, RegisterState )
execute instruction mmu registerState =
    Ok ( mmu, registerState )
